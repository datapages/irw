#!/usr/bin/env Rscript
#
# The site's sitemaps. Runs as the LAST post-render step, after
# landing/emit_landing_pages.R, and leaves _site/ with:
#
#   sitemap.xml          a sitemap index -- the URL robots.txt names, and the one
#                        submitted to Search Console, so it must not move
#   sitemap-pages.xml    every page Quarto rendered (top-level pages, vignettes,
#                        welcome pages)
#   sitemap-tables.xml   /tables/ and every /tables/<slug>/ page; written by the
#                        emitter, only listed here (it is absent when the emitter
#                        skipped itself for want of a Redivis token)
#
# Quarto writes its own flat sitemap.xml just before post-render runs. This script
# reads the URLs out of it and rewrites them as sitemap-pages.xml, fixing the two
# things Quarto gets wrong for us:
#
# 1. lastmod. Quarto uses the source file's mtime, which on a fresh CI checkout
#    is the checkout time, so every page claimed to change on every deploy and
#    the date carried no information. Here it is the last commit to touch the
#    source file or anything it pulls in with {{< include >}}. That needs full
#    history: in a shallow clone every file's "last commit" is the one commit
#    there is, so lastmod is omitted (with a warning) rather than made up. The
#    publish workflow checks out with fetch-depth: 0 for this.
#    Pages that query Redivis when rendered (data.qmd, docs.qmd, ...) change with
#    the warehouse as well as with their source; lastmod reflects the source only.
#
# 2. Directory index pages. Quarto lists /index.html and /vignettes/index.html;
#    they are listed here by their clean URLs, / and /vignettes/, which is also
#    what their canonical tags say (format.html.canonical-url in _quarto.yml).
#
# A page carrying a robots noindex meta tag is left out, and so is any URL whose
# file is not in the output. Fails the build if a child sitemap breaks the
# protocol's limits (50,000 URLs, 50MB uncompressed).
#
# On an incremental render (`quarto render one.qmd`) Quarto's sitemap holds only
# the pages it just rendered, because it cannot read ours back, so the previous
# sitemap-pages.xml is merged in. A full render (QUARTO_PROJECT_RENDER_ALL=1,
# always the case in CI) starts from Quarto's list alone.

SITE_URL <- "https://itemresponsewarehouse.org"
OUT      <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", "_site")
MAX_URLS <- 50000
MAX_BYTES <- 50 * 1024^2
SOURCE_EXT <- c(".qmd", ".md", ".Rmd", ".ipynb")

locs_in <- function(file) {
  if (!file.exists(file)) return(character(0))
  txt <- paste(readLines(file, warn = FALSE), collapse = "\n")
  m <- regmatches(txt, gregexpr("<loc>[^<]*</loc>", txt))[[1]]
  unxml(gsub("</?loc>", "", m))
}
is_urlset <- function(file)
  file.exists(file) && any(grepl("<urlset", readLines(file, n = 5, warn = FALSE), fixed = TRUE))

xml_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
unxml <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# https://.../vignettes/index.html -> https://.../vignettes/
clean_url <- function(u) sub("(^|/)index\\.html$", "\\1", u)

# The URL's path inside the output directory, and the file that serves it.
rel_of  <- function(u) sub(paste0("^", SITE_URL, "/?"), "", u)
file_of <- function(u) {
  r <- rel_of(u)
  file.path(OUT, ifelse(!nzchar(r) | grepl("/$", r), paste0(r, "index.html"), r))
}

# The source file a rendered page came from: the same path with a source
# extension. NA when there is none (a page Quarto listed but did not render from
# a file in this checkout).
source_of <- function(u) {
  stem <- sub("\\.html$", "", rel_of(sub("/$", "/index.html", u)))
  cand <- paste0(stem, SOURCE_EXT)
  hit <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else NA_character_
}

# The file and everything it includes, transitively. Include paths are relative
# to the including file, or to the project root when they start with "/".
with_includes <- function(file, seen = character(0)) {
  if (is.na(file) || file %in% seen || !file.exists(file)) return(seen)
  seen <- c(seen, file)
  txt <- readLines(file, warn = FALSE)
  inc <- regmatches(txt, regexpr("\\{\\{<\\s*include\\s+[^ >]+\\s*>\\}\\}", txt))
  inc <- trimws(sub("\\{\\{<\\s*include\\s+([^ >]+)\\s*>\\}\\}", "\\1", inc))
  for (p in inc) {
    p <- gsub("^[\"']|[\"']$", "", p)
    target <- if (startsWith(p, "/")) sub("^/", "", p) else file.path(dirname(file), p)
    seen <- with_includes(relpath(target), seen)
  }
  seen
}
# "vignettes/../components/_x.qmd" -> "components/_x.qmd", so a file included
# twice by different routes is visited once.
relpath <- function(p) {
  root <- paste0(normalizePath(".", mustWork = TRUE), "/")
  p <- normalizePath(p, mustWork = FALSE)
  if (startsWith(p, root)) substring(p, nchar(root) + 1) else p
}

git_ok <- function() {
  inside <- suppressWarnings(tryCatch(
    system2("git", c("rev-parse", "--is-inside-work-tree"), stdout = TRUE, stderr = FALSE),
    error = function(e) "false"))
  if (!identical(inside, "true")) {
    message("[sitemaps] not a git checkout -- pages get no lastmod")
    return(FALSE)
  }
  shallow <- suppressWarnings(system2("git", c("rev-parse", "--is-shallow-repository"),
                                      stdout = TRUE, stderr = FALSE))
  if (identical(shallow, "true")) {
    message("[sitemaps] WARNING: shallow clone, so commit dates are meaningless -- ",
            "pages get no lastmod. Check out with fetch-depth: 0.")
    return(FALSE)
  }
  TRUE
}

# Latest commit over a set of files: "<epoch> <strict ISO 8601>", the ISO part
# being a valid W3C datetime. NA when none of them has been committed.
last_commit <- function(files) {
  out <- suppressWarnings(system2("git", c("log", "-1", "--format='%ct %cI'", "--", shQuote(files)),
                                  stdout = TRUE, stderr = FALSE))
  if (!length(out) || !grepl("^[0-9]+ ", out[1])) return(NA_character_)
  out[1]
}

write_urlset <- function(file, urls, lastmod) {
  body <- vapply(seq_along(urls), function(i) paste0(
    "  <url>\n    <loc>", xml_esc(urls[i]), "</loc>\n",
    if (!is.na(lastmod[i])) paste0("    <lastmod>", lastmod[i], "</lastmod>\n") else "",
    "  </url>"), character(1))
  writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
               "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
               body, "</urlset>"), file)
}

check_limits <- function(file) {
  n <- length(locs_in(file)); b <- file.size(file)
  if (n > MAX_URLS || b > MAX_BYTES)
    stop("[sitemaps] ", basename(file), " has ", n, " URLs and ", b, " bytes; the ",
         "sitemap protocol allows ", MAX_URLS, " and ", MAX_BYTES, ". Split it.", call. = FALSE)
  n
}

main <- function() {
  quarto_sm <- file.path(OUT, "sitemap.xml")
  pages_sm  <- file.path(OUT, "sitemap-pages.xml")
  tables_sm <- file.path(OUT, "sitemap-tables.xml")

  urls <- if (is_urlset(quarto_sm)) locs_in(quarto_sm) else character(0)
  if (!identical(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"), "1") || !length(urls))
    urls <- c(urls, locs_in(pages_sm))
  urls <- unique(clean_url(urls))
  # The table pages have their own sitemap, whatever else happened.
  urls <- urls[!grepl(paste0("^", SITE_URL, "/tables/"), urls)]

  exists_ <- file.exists(file_of(urls))
  if (any(!exists_))
    message("[sitemaps] dropped, no file in the output: ", paste(urls[!exists_], collapse = ", "))
  urls <- urls[exists_]
  noindex <- vapply(file_of(urls), function(f)
    any(grepl("<meta[^>]+name=\"robots\"[^>]+noindex", readLines(f, warn = FALSE))),
    logical(1))
  if (any(noindex))
    message("[sitemaps] dropped, noindex: ", paste(urls[noindex], collapse = ", "))
  urls <- sort(urls[!noindex])

  commit <- rep(NA_character_, length(urls))
  if (git_ok()) {
    src <- vapply(urls, source_of, character(1))
    commit <- vapply(seq_along(urls), function(i)
      if (is.na(src[i])) NA_character_ else last_commit(with_includes(src[i])),
      character(1))
    if (any(is.na(commit)))
      message("[sitemaps] no commit date (no lastmod) for: ",
              paste(urls[is.na(commit)], collapse = ", "))
  }
  lastmod <- sub("^[0-9]+ ", "", commit)
  write_urlset(pages_sm, urls, lastmod)
  n_pages <- check_limits(pages_sm)

  children <- paste0(SITE_URL, "/sitemap-pages.xml")
  # The pages sitemap changed when its newest page did.
  child_mod <- if (all(is.na(commit))) NA_character_ else
    lastmod[which.max(as.numeric(sub(" .*", "", commit)))]
  n_tables <- 0
  if (file.exists(tables_sm)) {
    n_tables <- check_limits(tables_sm)
    children <- c(children, paste0(SITE_URL, "/sitemap-tables.xml"))
    child_mod <- c(child_mod, NA_character_)
  } else {
    message("[sitemaps] no sitemap-tables.xml (landing pages skipped) -- index lists pages only")
  }
  writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
               "<sitemapindex xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
               vapply(seq_along(children), function(i) paste0(
                 "  <sitemap>\n    <loc>", children[i], "</loc>\n",
                 if (!is.na(child_mod[i])) paste0("    <lastmod>", child_mod[i], "</lastmod>\n") else "",
                 "  </sitemap>"), character(1)),
               "</sitemapindex>"), quarto_sm)
  message("[sitemaps] sitemap.xml indexes sitemap-pages.xml (", n_pages, " URLs, ",
          sum(!is.na(lastmod)), " with lastmod)",
          if (n_tables) paste0(" and sitemap-tables.xml (", n_tables, " URLs)") else "")
}

main()
