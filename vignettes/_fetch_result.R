# Vignette results over 5 MB are not committed (ben-domingue/irw#1719, 17.1).
# They live as assets on the `vignette-data` release of datapages/irw, and a
# page calls fetch_result() on the path it is about to readRDS(). A local copy,
# if there is one, always wins, so a fresh compute run previews immediately --
# but CI only ever sees the release, so upload after a rerun:
#   gh release upload vignette-data <file> --clobber -R datapages/irw
#
# In CI a failed download is an error, not a fallback. The pages degrade to a
# "results not yet computed" notice when their cache is missing, and publishing
# that would silently replace real results on the live site.
fetch_result <- function(path, tag = "vignette-data") {
  if (file.exists(path)) return(invisible(path))
  url <- sprintf("https://github.com/datapages/irw/releases/download/%s/%s",
                 tag, basename(path))
  old <- options(timeout = max(300, getOption("timeout"))); on.exit(options(old))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ok <- tryCatch(utils::download.file(url, path, mode = "wb", quiet = TRUE) == 0,
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) {
    unlink(path)
    msg <- paste0("could not fetch ", basename(path), " from ", url)
    if (nzchar(Sys.getenv("CI"))) stop(msg, call. = FALSE)
    message(msg)
  }
  invisible(path)
}
