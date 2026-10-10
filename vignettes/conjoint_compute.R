# conjoint_compute.R
#
# A conjoint experiment shows respondents pairs of randomly assembled profiles
# and asks them to choose. The usual summary is the AMCE, the average effect of
# each attribute level on the chance a profile is chosen. Read as item response
# data, every choice is a paired comparison, so two measurement questions apply:
# how well do the attributes predict a held-out choice, and how much do
# respondent-specific preferences add on top of the shared (average) model?
# We answer both across the IRW conjoint source and set the second against the
# same quantity for person ability in dichotomous IRW tables.
#
# Three parts:
#   A. Hainmueller, Hopkins & Yamamoto (2014): reproduce the published AMCEs
#      from the IRW table and draw the task sample the page's widget uses.
#   B. Every eligible conjoint table: held-out IMV of the attribute model over a
#      coin flip at the base rate, and of respondent-specific preferences over
#      the shared attribute model.
#   C. A random sample of dichotomous core IRW tables: held-out IMV of a person
#      ability over item difficulties alone (the IRT counterpart of B's second
#      quantity).
#
# Everything is read from Redivis without a login, through the same row URLs
# the table landing pages on itemresponsewarehouse.org link to. With an
# account, irw::irw_fetch(table, source = "conj") returns the same rows.
#
# Produces the precomputed results loaded by conjoint.qmd.
#
# Output: conjoint_data/conjoint_results.rds
#         conjoint_data/references.bib
#
# Usage:
#   Rscript vignettes/conjoint_compute.R   # from project root

library(data.table)
library(dplyr)
library(tibble)
library(sandwich)
library(furrr)

set.seed(20261008)

# Conjoint spans shards (Redivis caps a dataset at 1,000 tables). Newest first:
# a table is read from the first shard that has it, as the packages resolve names.
CONJ_VERSIONS <- c("irw_conjoint_2:v1_0", "irw_conjoint:v4_2")
META_VERSION  <- "irw_meta:v41_0"

out_dir   <- "vignettes/conjoint_data"
# Per-table fits are kept per source version, so a version change refits everything
fits_dir  <- file.path(out_dir, "fits", gsub("[^A-Za-z0-9_]", "_", paste(CONJ_VERSIONS, collapse = "__")))
fetch_dir <- file.path(out_dir, "fits", "rows")   # raw row cache, gitignored with fits/
dir.create(fetch_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
bib_file  <- file.path(out_dir, "references.bib")

MIN_TASKS     <- 500    # usable forced-choice tasks per table
MIN_RESP_HET  <- 200    # respondents with >= 3 tasks, for the preference model
MAX_N         <- 5000   # downsample respondents/persons before fitting
N_IRT         <- 80     # dichotomous core tables in the comparison
N_WIDGET      <- 400    # HHY tasks shipped to the page's widget
LAMBDA_CONJ   <- c(1, 3, 10, 30, 100, 300, 1000)
LAMBDA_IRT    <- c(0.3, 1, 3, 10, 30, 100)
PILOT         <- as.logical(Sys.getenv("CONJ_PILOT", "FALSE"))  # TRUE: a few tables for a draft page
PILOT_N       <- 12

# ==============================================================================
# 0. Reading rows without a login
# ==============================================================================

rows_url <- function(dataset, table, vars = NULL) {
  paste0("https://redivis.com/api/v1/tables/datapages.", dataset, ".", table,
         "/rows?format=csv",
         if (length(vars)) paste0("&selectedVariables=", paste(vars, collapse = ",")))
}

read_rows <- function(dataset, table, vars = NULL, cache = TRUE, missing_ok = FALSE) {
  f <- file.path(fetch_dir, paste0(gsub("[^A-Za-z0-9_]", "_", dataset), "__", table, ".csv.gz"))
  if (cache && file.exists(f)) return(fread(f))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  for (k in 0:5) {
    h <- curl::new_handle()
    r <- tryCatch(curl::curl_fetch_disk(rows_url(dataset, table, vars), tmp, handle = h),
                  error = function(e) NULL)
    if (!is.null(r) && r$status_code == 200) break
    if (!is.null(r) && r$status_code == 404 && missing_ok) return(NULL)
    if (!is.null(r) && r$status_code != 429) stop("HTTP ", r$status_code, " for ", table)
    Sys.sleep(3 * 2^k)
  }
  if (is.null(r) || r$status_code != 200) stop("fetch failed for ", table)
  d <- fread(tmp)
  if (cache) fwrite(d, f)
  d
}

# A conjoint table from whichever shard holds it (a 404 means "not in this shard").
read_conj <- function(table) {
  for (v in CONJ_VERSIONS) {
    d <- read_rows(v, table, missing_ok = TRUE)
    if (!is.null(d)) return(d)
  }
  stop(table, " is in none of ", paste(CONJ_VERSIONS, collapse = ", "))
}

# ==============================================================================
# 1. Shared machinery
# ==============================================================================

# IMV (Domingue et al.): convert each model's mean held-out log-likelihood to the
# accuracy w of a weighted coin with the same log-likelihood, then report the
# relative gain (w1 - w0) / w0.
w_of <- function(ll) {
  if (ll <= log(0.5)) return(0.5)
  uniroot(function(w) w * log(w) + (1 - w) * log(1 - w) - ll, c(0.5, 1 - 1e-12), tol = 1e-12)$root
}
imv <- function(ll0, ll1) { w0 <- w_of(ll0); (w_of(ll1) - w0) / w0 }
mean_ll <- function(y, p) { p <- pmin(pmax(p, 1e-9), 1 - 1e-9); mean(y * log(p) + (1 - y) * log(1 - p)) }

# L2-penalised logistic regression by Newton's method: maximise
# sum(loglik) - lambda/2 * ||b||^2, intercept unpenalised (lambda = 1 is
# scikit-learn's default C = 1, which the first version of this analysis used).
ridge_logit <- function(X, y, lambda = 1, iters = 50) {
  X1 <- cbind(1, X); k <- ncol(X1); b <- numeric(k)
  P <- diag(c(0, rep(lambda, k - 1)), k)
  for (i in seq_len(iters)) {
    p <- plogis(drop(X1 %*% b))
    g <- crossprod(X1, y - p) - P %*% b
    H <- crossprod(X1 * (p * (1 - p)), X1) + P
    step <- solve(H, g); b <- b + drop(step)
    if (max(abs(step)) < 1e-8) break
  }
  b
}

# Forced-choice tasks with two profiles and exactly one chosen. Returns one row
# per task (id, task, y = 1 if the first profile was chosen) and D, the
# difference between the two profiles' attribute dummies. Attributes missing on
# a profile are a level of their own ("(not shown)").
tasks_frame <- function(d) {
  d <- as.data.table(d)[!is.na(choice)]
  d[, `:=`(n_ = .N, s_ = sum(choice), np_ = uniqueN(profile)), by = .(id, task)]
  d <- d[n_ == 2 & s_ == 1 & np_ == 2]
  setorder(d, id, task, profile)
  attrs <- grep("^attr_", names(d), value = TRUE)
  for (a in attrs) {
    x <- as.character(d[[a]]); x[is.na(x) | x == ""] <- "(not shown)"
    set(d, j = a, value = x)
  }
  attrs <- attrs[vapply(attrs, function(a) uniqueN(d[[a]]) > 1, logical(1))]
  X <- model.matrix(~ ., data = as.data.frame(lapply(d[, ..attrs], factor)))[, -1, drop = FALSE]
  first <- seq(1, nrow(d), by = 2)
  D <- X[first, , drop = FALSE] - X[first + 1, , drop = FALSE]
  D <- D[, colSums(abs(D)) > 0, drop = FALSE]
  list(t = data.table(id = d$id[first], task = d$task[first], y = as.numeric(d$choice[first])),
       D = D, n_attr = length(attrs))
}

# ==============================================================================
# A. Hainmueller, Hopkins & Yamamoto (2014)
# ==============================================================================

message("A. HHY replication")
hhy <- read_conj("hainmueller_2014_immigrant")
published <- fread(file.path(out_dir, "hhy_published_amce.csv"))

hhy_attrs <- unique(published$attribute)
for (a in hhy_attrs) {
  lv <- published[attribute == a]$level           # baseline first, as in the paper
  lv <- c(lv[published[attribute == a]$baseline], lv[!published[attribute == a]$baseline])
  set(hhy, j = a, value = factor(hhy[[a]], levels = lv))
}

# The design forbade two kinds of profile: the four high-skill professions with
# less than two years of college, and fleeing persecution from the six countries
# outside China, Sudan, Somalia and Iraq. AMCEs for those attributes are
# estimated, as in cjoint, from a model with the two interactions, averaging
# each level's conditional effect over the levels of the other attribute where
# both it and the baseline could appear (the design is uniform over allowed
# profiles, so the weights are equal).
allowed <- function(a, b) table(hhy[[a]], hhy[[b]]) > 0
dep <- list(c("attr_education", "attr_profession"), c("attr_origin", "attr_application_reason"))

f_hhy <- as.formula(paste("choice ~", paste(hhy_attrs, collapse = " + "), "+",
                          paste(vapply(dep, paste, "", collapse = ":"), collapse = " + ")))
fit <- lm(f_hhy, data = hhy)
V   <- vcovCL(fit, cluster = ~ id, type = "HC1")   # = cjoint's respondent-clustered SEs
b   <- coef(fit)

amce_row <- function(a, lv) {
  L <- setNames(numeric(length(b)), names(b))
  main <- paste0(a, lv); L[main] <- 1
  for (pr in dep) if (a %in% pr) {
    z  <- setdiff(pr, a); A <- allowed(a, z)
    zs <- colnames(A)[A[lv, ] & A[levels(hhy[[a]])[1], ]]
    w  <- 1 / length(zs)
    for (zl in zs) {
      nm <- if (match(a, pr) == 1) paste0(main, ":", z, zl) else paste0(z, zl, ":", main)
      if (!is.na(b[nm])) L[nm] <- L[nm] + w
    }
  }
  L[is.na(b)] <- 0
  bb <- b; bb[is.na(bb)] <- 0
  keep <- names(b)[!is.na(b)]
  data.table(attribute = a, level = lv, irw_amce = sum(L * bb),
             irw_se = sqrt(drop(t(L[keep]) %*% V[keep, keep] %*% L[keep])))
}
hhy_amce <- rbindlist(lapply(hhy_attrs, function(a) rbindlist(lapply(levels(hhy[[a]])[-1], function(lv) amce_row(a, lv)))))
hhy_amce <- merge(published, hhy_amce, by = c("attribute", "level"), all.x = TRUE, sort = FALSE)
hhy_amce[baseline == TRUE, `:=`(irw_amce = 0, irw_se = NA_real_)]
message("  max |IRW - published| AMCE: ", signif(max(abs(hhy_amce$irw_amce - hhy_amce$published_amce)), 3),
        "; SE: ", signif(max(abs(hhy_amce$irw_se - hhy_amce$published_se), na.rm = TRUE), 3))

# Support rules for the widget's simple estimator: for a level of a restricted
# attribute, compare it with the baseline only among profiles whose other
# attribute takes a value where both could appear.
support <- rbindlist(lapply(hhy_attrs, function(a) {
  pr <- Filter(function(p) a %in% p, dep)
  rbindlist(lapply(levels(hhy[[a]])[-1], function(lv) {
    if (!length(pr)) return(data.table(attribute = a, level = lv, other = NA_character_, other_levels = list(NULL)))
    z <- setdiff(pr[[1]], a); A <- allowed(a, z)
    data.table(attribute = a, level = lv, other = z,
               other_levels = list(colnames(A)[A[lv, ] & A[levels(hhy[[a]])[1], ]]))
  }))
}))

# The same estimator on all 1,396 respondents, to show how close it is to the AMCE.
dim_full <- rbindlist(lapply(seq_len(nrow(support)), function(i) {
  s <- support[i]; a <- s$attribute; base <- levels(hhy[[a]])[1]
  ok <- if (is.na(s$other)) rep(TRUE, nrow(hhy)) else hhy[[s$other]] %in% s$other_levels[[1]]
  data.table(attribute = a, level = s$level,
             dim = mean(hhy$choice[ok & hhy[[a]] == s$level]) - mean(hhy$choice[ok & hhy[[a]] == base]))
}))
hhy_amce <- merge(hhy_amce, dim_full, by = c("attribute", "level"), all.x = TRUE, sort = FALSE)

# Tasks for the widget: N_WIDGET random complete tasks, with the real
# respondent's choice so the reader can see whether they agreed.
tk <- hhy[, .(n = .N, s = sum(choice)), by = .(id, task)][n == 2 & s == 1]
tk <- tk[sample(.N, N_WIDGET)]
widget_tasks <- merge(hhy, tk[, .(id, task)], by = c("id", "task"))
setorder(widget_tasks, id, task, profile)
widget_tasks[, task_key := .GRP, by = .(id, task)]
widget_tasks <- widget_tasks[, c("task_key", "profile", "choice", hhy_attrs), with = FALSE]
for (a in hhy_attrs) set(widget_tasks, j = a, value = as.character(widget_tasks[[a]]))

# ==============================================================================
# B. Every eligible conjoint table
# ==============================================================================

message("B. Conjoint tables")
conj_meta <- read_rows(META_VERSION, "conj_metadata", cache = FALSE)
conj_bib  <- read_rows(META_VERSION, "conj_biblio", cache = FALSE)

is_choice <- grepl("(^|;)choice(;|$)", conj_meta$outcomes)
candidates <- conj_meta[is_choice & n_profiles == 2 & n_respondents * n_tasks >= MIN_TASKS]$table
# Left out: in the deposit itself every respondent's chosen candidate has the
# same index in all six tasks, and co-partisans are chosen 52% vs 48%. The
# outcomes do not line up with the profiles (reported to Ben, 2026-10-10).
EXCLUDE <- c("joo_2026_descriptive_rep")
candidates <- setdiff(candidates, EXCLUDE)
message("  candidates: ", length(candidates), " of ", nrow(conj_meta))

conj_tables <- if (PILOT) unique(c("hainmueller_2014_immigrant", "kreps_2020_covid_vaccine",
                                    sample(setdiff(candidates, "hainmueller_2014_immigrant"), PILOT_N - 2))) else candidates

fit_conj <- function(tab) {
  d <- read_conj(tab)
  if (!all(c("id", "task", "profile", "choice") %in% names(d))) return(NULL)
  ids <- unique(d$id)
  if (length(ids) > MAX_N) d <- d[id %in% sample(ids, MAX_N)]
  tf <- tasks_frame(d); t <- tf$t; D <- tf$D
  if (nrow(t) < MIN_TASKS) return(NULL)
  y <- t$y

  # Question 1: attributes vs. a coin flip at the base rate. Folds by respondent.
  ids <- unique(t$id); fold <- setNames(sample(rep_len(1:5, length(ids))), ids)
  f <- fold[as.character(t$id)]
  p_attr <- p_base <- numeric(nrow(t))
  for (k in 1:5) {
    tr <- f != k; te <- f == k
    bk <- ridge_logit(D[tr, , drop = FALSE], y[tr])
    p_attr[te] <- plogis(drop(cbind(1, D[te, , drop = FALSE]) %*% bk))
    p_base[te] <- mean(y[tr])
  }
  q1 <- list(imv_attr = imv(mean_ll(y, p_base), mean_ll(y, p_attr)),
             w_base = w_of(mean_ll(y, p_base)), w_attr = w_of(mean_ll(y, p_attr)))

  # Question 2: respondent-specific preferences vs. the shared attribute model.
  # Hold out tasks within respondent (fold = task position mod min(5, tasks)).
  # Each respondent gets a deviation delta_i on every attribute term and their
  # own intercept, shrunk to zero by an L2 penalty lambda (the MAP of a
  # random-coefficient logit), fitted with the shared model as an offset.
  # lambda is chosen on 20% of respondents; the IMV is reported on the rest.
  q2 <- list(imv_indiv = NA_real_, w_shared = NA_real_, w_indiv = NA_real_, lambda = NA_real_,
             n_resp_q2 = NA_integer_, mean_tasks = NA_real_)
  t[, ntask := .N, by = id]
  keep <- t$ntask >= 3
  if (uniqueN(t$id[keep]) >= MIN_RESP_HET) {
    t2 <- t[keep]; D2 <- D[keep, , drop = FALSE]; y2 <- t2$y
    t2[, pos := seq_len(.N) - 1L, by = id]
    t2[, fold := pos %% pmin(5L, ntask)]
    Z <- cbind(D2, 1)                            # deviation terms incl. own intercept
    rid <- split(seq_len(nrow(t2)), t2$id)
    tune <- setNames(runif(length(rid)) < 0.2, names(rid))
    p0 <- rep(NA_real_, nrow(t2)); p1 <- matrix(NA_real_, nrow(t2), length(LAMBDA_CONJ))
    for (k in 0:4) {
      tr <- t2$fold != k; te <- t2$fold == k
      if (!any(te)) next
      bk  <- ridge_logit(D2[tr, , drop = FALSE], y2[tr])
      off <- drop(cbind(1, D2) %*% bk)
      p0[te] <- plogis(off[te])
      for (r in rid) {
        rtr <- r[tr[r]]; rte <- r[te[r]]
        if (!length(rte) || !length(rtr)) next
        # The penalised optimum lies in the span of the training rows, so solve
        # for alpha (one per training task) instead of delta: delta = Z_tr' alpha.
        Zt <- Z[rtr, , drop = FALSE]; K <- tcrossprod(Zt); Kte <- Z[rte, , drop = FALSE] %*% t(Zt)
        for (j in seq_along(LAMBDA_CONJ)) {
          lam <- LAMBDA_CONJ[j]; a <- numeric(length(rtr))
          for (it in 1:50) {
            p <- plogis(off[rtr] + drop(K %*% a)); W <- p * (1 - p)
            step <- solve(W * K + diag(lam, length(rtr)), y2[rtr] - p - lam * a)
            a <- a + step
            if (max(abs(step)) < 1e-8) break
          }
          p1[rte, j] <- plogis(off[rte] + drop(Kte %*% a))
        }
      }
    }
    ok <- !is.na(p0) & !is.na(p1[, 1])
    is_tune <- tune[as.character(t2$id)]
    st <- ok & is_tune; se <- ok & !is_tune
    best <- which.max(apply(p1, 2, function(p) mean_ll(y2[st], p[st])))
    q2 <- list(imv_indiv = imv(mean_ll(y2[se], p0[se]), mean_ll(y2[se], p1[se, best])),
               w_shared = w_of(mean_ll(y2[se], p0[se])), w_indiv = w_of(mean_ll(y2[se], p1[se, best])),
               lambda = LAMBDA_CONJ[best], n_resp_q2 = sum(!tune), mean_tasks = mean(t2[, .N, by = id]$N))
  }

  m <- conj_meta[table == tab]
  c(list(table = tab, n_resp = length(ids), n_tasks_used = nrow(t), n_attr = tf$n_attr,
         n_terms = ncol(D), restrictions = m$restrictions, country = m$country), q1, q2)
}

# ==============================================================================
# C. Dichotomous core IRW tables
# ==============================================================================

message("C. IRT comparison")
core_meta <- read_rows(META_VERSION, "metadata", cache = FALSE)
irt_pool <- core_meta[n_categories == 2 & longitudinal == FALSE & density > 0.9 &
                      n_participants >= 300 & n_participants <= 20000 & n_items >= 5 & n_items <= 100]
irt_tables <- irt_pool[sample(.N, min(if (PILOT) PILOT_N else N_IRT, .N))]
message("  pool: ", nrow(irt_pool), "; sampled: ", nrow(irt_tables))

# Same held-out design, items in place of tasks: hold out a fifth of each
# person's responses; shared model = item difficulties (logit of the training
# item mean); individual model adds a person ability theta_i shrunk by lambda.
fit_irt <- function(tab, dataset) {
  d <- read_rows(dataset, tab, vars = c("id", "item", "resp"))
  d <- unique(d[!is.na(resp)], by = c("id", "item"))
  vals <- sort(unique(d$resp))
  if (length(vals) != 2) return(NULL)
  d[, y := as.numeric(resp == vals[2])]
  ids <- unique(d$id)
  if (length(ids) > MAX_N) { d <- d[id %in% sample(ids, MAX_N)]; ids <- unique(d$id) }
  if (all(d[, mean(y), by = id]$V1 %in% c(0, 1))) return(NULL)
  d <- d[sample(.N)]
  d[, fold := (seq_len(.N) - 1L) %% 5L, by = id]
  r <- match(d$id, ids); y <- d$y; n <- length(ids)
  tune <- runif(n) < 0.2
  p0 <- numeric(nrow(d)); p1 <- matrix(0, nrow(d), length(LAMBDA_IRT))
  for (k in 0:4) {
    tr <- d$fold != k; te <- d$fold == k
    im <- d[tr, .(s = sum(y), n = .N), by = item]
    bdiff <- setNames(log((im$s + .5) / (im$n - im$s + .5)), im$item)
    off <- unname(bdiff[as.character(d$item)]); off[is.na(off)] <- 0
    p0[te] <- plogis(off[te])
    for (j in seq_along(LAMBDA_IRT)) {
      th <- numeric(n)
      for (it in 1:30) {
        p <- plogis(off + th[r])
        g <- tabulate_sum(r[tr], (y - p)[tr], n) - LAMBDA_IRT[j] * th
        h <- tabulate_sum(r[tr], (p * (1 - p))[tr], n) + LAMBDA_IRT[j]
        th <- th + g / h
      }
      p1[te, j] <- plogis(off + th[r])[te]
    }
  }
  st <- tune[r]; se <- !tune[r]
  best <- which.max(apply(p1, 2, function(p) mean_ll(y[st], p[st])))
  list(table = tab, persons = n, items = uniqueN(d$item), per_person = nrow(d) / n, lambda = LAMBDA_IRT[best],
       imv_indiv = imv(mean_ll(y[se], p0[se]), mean_ll(y[se], p1[se, best])),
       w_shared = w_of(mean_ll(y[se], p0[se])), w_indiv = w_of(mean_ll(y[se], p1[se, best])))
}
tabulate_sum <- function(i, x, n) { out <- numeric(n); s <- rowsum(x, i); out[as.integer(rownames(s))] <- s[, 1]; out }

# ==============================================================================
# Run, writing each result to disk as it completes (re-running resumes)
# ==============================================================================

to_disk <- function(key, fun) {
  out_file <- file.path(fits_dir, paste0(key, ".rds"))
  if (file.exists(out_file)) return(invisible(NULL))
  res <- tryCatch(fun(), error = function(e) { message("  failed ", key, ": ", conditionMessage(e)); e })
  # An error (a failed fetch, say) is not saved, so re-running retries it. A
  # NULL means the table is ineligible, and is saved so it is not retried.
  if (!inherits(res, "error")) saveRDS(res, out_file)
  invisible(NULL)
}

plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
future_walk(conj_tables, function(tab) to_disk(paste0("conj__", tab), function() fit_conj(tab)),
            .options = furrr_options(seed = TRUE))
future_walk(seq_len(nrow(irt_tables)), function(i) {
  tab <- irt_tables$table[i]; ds <- irt_tables$dataset[i]
  to_disk(paste0("irt__", tab), function() fit_irt(tab, ds))
}, .options = furrr_options(seed = TRUE))
plan(sequential)

collect <- function(prefix, keys) {
  rbindlist(lapply(keys, function(k) {
    f <- file.path(fits_dir, paste0(prefix, k, ".rds"))
    if (file.exists(f)) { x <- readRDS(f); if (!is.null(x)) as.data.table(x) }
  }), fill = TRUE)
}
conj_summary <- collect("conj__", conj_tables)
irt_summary  <- collect("irt__", irt_tables$table)
message("  conjoint tables with results: ", nrow(conj_summary), " of ", length(conj_tables),
        "; with a preference model: ", sum(!is.na(conj_summary$imv_indiv)))
message("  IRT tables with results: ", nrow(irt_summary), " of ", nrow(irt_tables))

# ==============================================================================
# Save combined output and references
# ==============================================================================

titles <- conj_bib[, .(table, reference = Reference_x)]
conj_summary <- merge(conj_summary, titles, by = "table", all.x = TRUE)

saveRDS(list(
  hhy_amce         = as_tibble(hhy_amce),
  widget_tasks     = as_tibble(widget_tasks),
  support          = support,
  summary          = as_tibble(conj_summary),
  irt_summary      = as_tibble(irt_summary),
  candidate_tables = candidates,
  n_all_candidates = length(candidates),
  n_conj_tables    = nrow(conj_meta),
  irt_pool_n       = nrow(irt_pool),
  versions         = c(conj = paste(CONJ_VERSIONS, collapse = " + "), meta = META_VERSION),
  pilot            = PILOT,
  date_run         = Sys.Date(),
  session          = sessionInfo()
), file.path(out_dir, "conjoint_results.rds"))

bib <- unique(trimws(conj_bib[table %in% c("hainmueller_2014_immigrant", conj_summary$table)]$BibTex))
bib <- bib[nzchar(bib) & !is.na(bib)]
writeLines(c(bib, readLines(file.path(out_dir, "methods.bib"))), bib_file)   # + the two IMV papers
message("Done: ", file.path(out_dir, "conjoint_results.rds"))
