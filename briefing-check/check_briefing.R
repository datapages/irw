# Repeatable check of the agents briefing (itemresponsewarehouse.org/llms.txt) against the R package.
#
# Section 0 of the briefing is offline and its numbers are fixed by the data shipped in the package,
# so those are asserted exactly. The warehouse checks assert properties a silent no-op cannot satisfy
# (a filter returns fewer tables than the catalogue; the quota guard excludes the big tables). Any FAIL
# makes the script exit with status 1. WARNs flag briefing text that may have gone stale.
#
# Needs the package installed with the documented lines, and REDIVIS_API_TOKEN in the environment for
# the warehouse part. Downloads no response tables, so it spends no Redivis quota.
#
#     Rscript check_briefing.R

options(repos = c(CRAN = "https://cloud.r-project.org"))
results <- list()
record <- function(status, name, detail = "") {
  results[[length(results) + 1]] <<- list(status = status, name = name, detail = detail)
  cat(sprintf("[%s] %s%s\n", status, name, if (nzchar(detail)) paste0(": ", detail) else ""))
}
check <- function(cond, name, detail = "") record(if (isTRUE(cond)) "PASS" else "FAIL", name, detail)
warn_if <- function(cond, name, detail = "") record(if (isTRUE(cond)) "WARN" else "PASS", name, if (isTRUE(cond)) detail else "")
near <- function(x, target, tol) {
  x <- suppressWarnings(as.numeric(x))[1]
  is.finite(x) && abs(x - target) <= tol
}

# 0. The install lines of section 0, as documented. redivis is needed for the warehouse part only.
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
if (!requireNamespace("irw", quietly = TRUE)) pak::pak("itemresponsewarehouse/Rpkg")
for (p in c("psych", "mokken")) if (!requireNamespace(p, quietly = TRUE)) pak::pak(p)
check(requireNamespace("irw", quietly = TRUE), "irw installs from itemresponsewarehouse/Rpkg")
check(requireNamespace("redivis", quietly = TRUE), "redivis is installed",
      "section 0 documents pak::pak(\"redivis/redivis-r\"); without it the first warehouse call stops")
suppressPackageStartupMessages(library(irw))
cat("irw", as.character(packageVersion("irw")), "\n")

# 1. Section 0, offline, exact numbers.
data(swmd_mokken, package = "irw")
check(nrow(swmd_mokken) == 4557, "swmd_mokken has 4,557 rows", nrow(swmd_mokken))
wide <- suppressMessages(irw_long2resp(swmd_mokken, agg_method = "mode", id_density_threshold = NULL))
resp <- as.matrix(wide[, setdiff(names(wide), "id"), drop = FALSE])
alpha <- psych::alpha(resp)$total$raw_alpha
check(near(alpha, 0.79, 0.01), "section 0: alpha is 0.79", sprintf("%.3f", alpha))
# coefH() returns H with its standard error, and prints its whole table unless told not to.
H <- suppressWarnings(mokken::coefH(as.data.frame(resp), se = FALSE, results = FALSE))$H
H <- suppressWarnings(as.numeric(H))[1]
check(near(H, 0.616, 0.005), "section 0: Mokken H is 0.616", sprintf("%.3f", H))
w2 <- suppressMessages(irw_long2resp(swmd_mokken))
m2 <- as.matrix(w2[, setdiff(names(w2), "id"), drop = FALSE])
check(any(abs(m2 - round(m2)) > 1e-9, na.rm = TRUE),
      "section 0: default irw_long2resp() produces fractional values (the mean of repeated ratings)")
data(diff_long, package = "irw")
check(nrow(diff_long) == 6143 && length(unique(diff_long$dataset)) == 145,
      "section 0: diff_long has 6,143 rows from 145 datasets",
      paste(nrow(diff_long), "rows,", length(unique(diff_long$dataset)), "datasets"))

# 2. Section 1 and 2: the functions the briefing names must exist with the arguments it uses.
ex <- getNamespaceExports("irw")
for (fn in c("irw_filter", "irw_fetch", "irw_long2resp", "irw_covariates", "irw_table_sets",
             "irw_check_resp", "irw_metadata", "irw_version", "irw_info"))
  check(fn %in% ex, paste0("the briefing's ", fn, "() is exported"))
check("resp_col" %in% names(formals(irw_long2resp)), "irw_long2resp() takes resp_col (nominal reshape, section 1)")
check("density" %in% names(formals(irw_filter)), "irw_filter() takes density (pitfall 1)")

# 2b. Pitfall 2, offline. The briefing says irw_long2resp() drops every cov_* column and, by default,
#     every id answering under 10% of the items, and that irw_covariates(align = wide) is the way to
#     reattach them without misassigning people. A toy frame checks that without touching the warehouse.
toy <- data.frame(id = c(rep(1, 20), rep(2, 20), 3),
                  item = c(paste0("i", 0:19), paste0("i", 0:19), "i0"),
                  resp = c(rep(1, 20), rep(0, 20), 1),
                  cov_g = c(rep("a", 20), rep("b", 20), "c"))
wide_toy <- suppressMessages(irw_long2resp(toy))
warn_if("cov_g" %in% names(wide_toy), "pitfall 2: irw_long2resp() drops cov_* columns", "it now keeps them: update section 2")
check(!(3 %in% wide_toy$id), "pitfall 2: the default id_density_threshold drops an id answering 5% of items",
      paste("ids kept:", paste(wide_toy$id, collapse = ",")))
cv <- tryCatch(suppressMessages(irw_covariates(toy, align = wide_toy)), error = function(e) e)
if (inherits(cv, "error")) {
  record("FAIL", "pitfall 2: irw_covariates(align = wide) runs", conditionMessage(cv))
} else {
  check(nrow(cv) == nrow(wide_toy) && all(cv$id == wide_toy$id) && "cov_g" %in% names(cv),
        "pitfall 2: irw_covariates(align = wide) returns one row per kept id, in order, with cov_g",
        paste(nrow(cv), "rows"))
}

# 3. The warehouse: filters must filter, and the quota guard must guard.
if (!nzchar(Sys.getenv("REDIVIS_API_TOKEN"))) {
  record("WARN", "warehouse checks skipped", "REDIVIS_API_TOKEN not set")
} else {
  md <- irw_metadata()
  total <- nrow(md)
  check(total > 1000 && "n_responses" %in% names(md), "irw_metadata() returns the catalogue with n_responses",
        paste(total, "tables"))
  f1 <- suppressMessages(irw_filter(n_categories = 2))
  f2 <- suppressMessages(irw_filter(n_categories = 2, density = NULL))
  f3 <- suppressMessages(irw_filter(n_responses = c(0, 1000)))
  check(length(f1) < total, "irw_filter(n_categories = 2) filters", paste(length(f1), "of", total))
  check(length(f2) >= length(f1), "dropping the default density filter never removes tables",
        paste(length(f1), "->", length(f2)))
  check(length(f3) < total && length(f3) != length(f1), "irw_filter(n_responses = c(0, 1000)) filters",
        paste(length(f3), "of", total))
  guarded <- suppressMessages(irw_filter(n_responses = c(0, 1e6)))
  big <- md$table[!is.na(md$n_responses) & md$n_responses >= 1e6]
  leaked <- intersect(guarded, big)
  check(length(leaked) == 0, "n_responses = c(0, 1e6) excludes every table at 1M+ rows",
        paste(length(guarded), "pass,", length(big), "are 1M+, leaked:", paste(head(leaked, 5), collapse = ", ")))
  # Section 3 size claims: they drift with the corpus, so out-of-range is a WARN to update the text.
  n <- md$n_responses
  warn_if(!near(median(n, na.rm = TRUE), 8200, 1500), "section 3 says the median table is ~8,200 rows",
          paste("median is", median(n, na.rm = TRUE)))
  warn_if(!near(100 * mean(n < 1e6, na.rm = TRUE), 96, 2), "section 3 says ~96% of tables are under 1M rows",
          sprintf("%.1f%%", 100 * mean(n < 1e6, na.rm = TRUE)))
  warn_if(!near(sum(n >= 1e7, na.rm = TRUE), 82, 15), "section 3 says ~82 tables are at 10M+ rows",
          sum(n >= 1e7, na.rm = TRUE))
  v <- irw_version()
  check(!is.null(v), "irw_version() returns")
  # The counts, for compare.py. Same keys as the Python side; no jsonlite needed.
  json <- sprintf(paste0('{"package": "r", "irw_version": "%s", "filters": {"n_categories=2": %d, ',
                         '"n_categories=2, density=None": %d, "n_responses=[0, 1000]": %d}, "quota_guard_pass": %d}'),
                  as.character(packageVersion("irw")), length(f1), length(f2), length(f3), length(guarded))
  writeLines(json, "counts_r.json")
  cat("counts written to counts_r.json\n")
}

fails <- sum(vapply(results, function(r) r$status == "FAIL", logical(1)))
warns <- sum(vapply(results, function(r) r$status == "WARN", logical(1)))
cat(sprintf("\n%d checks: %d pass, %d warn, %d fail\n", length(results), length(results) - fails - warns, warns, fails))
quit(status = if (fails > 0) 1 else 0)
