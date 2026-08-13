## Vectorized BISG engine with pluggable dictionaries.
##
## predict_demog() reimplements the predict_names() probability model as
## matrix algebra over deduplicated name / geography values instead of a
## per-row predict_race() loop, and generalizes the tables: the name
## dictionaries and the geographic prior table can be user-supplied, and
## the category grouping is read off the supplied tables' columns rather
## than being fixed to the six Census race / Hispanic-origin groups.
## The matching cascade is shared with lookup_name() via cascade_match(),
## so per-name matching behavior is identical to the per-row path.

## Vectorized normalize_name(): trim, NFD-decompose + drop combining
## marks, uppercase. NA -> "".
normalize_names_vec <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trim_both(x)
  x <- stringi::stri_trans_nfd(x)
  x <- stringi::stri_replace_all_regex(x, "\\p{Mn}", "")
  x <- stringi::stri_trans_toupper(x)
  x[is.na(x)] <- ""
  x
}

## A "spec" is one dictionary ready for cascade lookup: a name -> row
## hash environment plus a row-major probability matrix over `groups`.
demog_spec_custom <- function(df, groups, is_last, what) {
  pm <- as.matrix(df[, groups, drop = FALSE])
  storage.mode(pm) <- "double"
  if (any(pm < 0, na.rm = TRUE)) {
    stop("`", what, "` has negative values in its category columns.",
         call. = FALSE)
  }
  rs <- rowSums(pm)
  ok <- is.finite(rs) & rs > 0
  if (!all(ok)) {
    warning(sum(!ok), " row(s) of `", what, "` have missing or ",
            "non-positive category values and were dropped.", call. = FALSE)
  }
  pm <- pm / rs                       # renormalize rows to sum to 1
  keys <- normalize_names_vec(df$name)
  keep <- ok & nzchar(keys) & !duplicated(keys)
  idx <- which(keep)
  env <- list2env(
    stats::setNames(as.list(idx), keys[idx]),
    envir = new.env(parent = emptyenv(), hash = TRUE,
                    size = max(1L, length(idx)))
  )
  list(env = env, is_last = is_last, pm = pm)
}

demog_spec_bundled <- function(table) {
  df <- table_df(table)
  pm <- as.matrix(df[, attr(df, "groups"), drop = FALSE])
  storage.mode(pm) <- "double"
  list(env = lookup_env(table), is_last = table %in% c("last", "last_extra"),
       pm = pm)
}

## First hit across an ordered chain of specs. `norm` must already be
## normalized. Returns list(spec, idx, rule) or NULL.
chain_match <- function(norm, specs) {
  for (si in seq_along(specs)) {
    m <- cascade_match(norm, specs[[si]]$env, specs[[si]]$is_last)
    if (!is.null(m)) return(list(spec = si, idx = m$idx, rule = m$rule))
  }
  NULL
}

## Resolve one raw given-name field value: compound-first, then
## per-token (mirrors lookup_compound_or_tokens()). Returns the
## elementwise product of the matched rows' probabilities and the
## number of matched evidence pieces.
resolve_given_value <- function(value, specs, n_groups) {
  toks <- tokenize_names(value)
  if (length(toks) == 0L) return(list(prod = NULL, k = 0L))
  if (length(toks) > 1L) {
    joined <- normalize_name(paste(toks, collapse = " "))
    if (nzchar(joined)) {
      m <- chain_match(joined, specs)
      if (!is.null(m) && !grepl("segment", m$rule, fixed = TRUE)) {
        return(list(prod = specs[[m$spec]]$pm[m$idx, ], k = 1L))
      }
    }
  }
  prod <- NULL
  k <- 0L
  for (t in toks) {
    norm <- normalize_name(t)
    if (!nzchar(norm)) next
    m <- chain_match(norm, specs)
    if (is.null(m)) next
    p <- specs[[m$spec]]$pm[m$idx, ]
    prod <- if (is.null(prod)) p else prod * p
    k <- k + 1L
  }
  list(prod = prod, k = k)
}

## Resolve one raw surname field value: per-token only (mirrors
## lookup_surname_tokens()).
resolve_surname_value <- function(value, specs, n_groups) {
  toks <- tokenize_names(value)
  prod <- NULL
  k <- 0L
  for (t in toks) {
    norm <- normalize_name(t)
    if (!nzchar(norm)) next
    m <- chain_match(norm, specs)
    if (is.null(m)) next
    p <- specs[[m$spec]]$pm[m$idx, ]
    prod <- if (is.null(prod)) p else prod * p
    k <- k + 1L
  }
  list(prod = prod, k = k, has_tokens = length(toks) > 0L)
}

## Run a resolver once per unique field value. The cascade only ever
## runs on unique values; rows are mapped back by match(). With
## `n_cores > 1` the unique values are split into chunks and resolved
## via parallel::mclapply (fork-based); `tick`, when non-NULL, is called
## once per resolved value on the serial path to advance a progress bar.
resolve_uniques <- function(values, resolver, specs, n_groups,
                            n_cores = 1L, tick = NULL) {
  u <- length(values)
  prodm  <- matrix(1, nrow = u, ncol = n_groups)
  kvec   <- integer(u)
  hastok <- logical(u)
  if (u == 0L) return(list(prod = prodm, k = kvec, has_tokens = hastok))
  if (n_cores > 1L && u > 1L) {
    nc <- min(n_cores, u)
    chunks <- split(seq_len(u), cut(seq_len(u), nc, labels = FALSE))
    parts <- parallel::mclapply(chunks, function(idx) {
      m <- matrix(1, nrow = length(idx), ncol = n_groups)
      k <- integer(length(idx))
      h <- logical(length(idx))
      for (j in seq_along(idx)) {
        r <- resolver(values[idx[j]], specs, n_groups)
        if (!is.null(r$prod)) m[j, ] <- r$prod
        k[j] <- r$k
        h[j] <- isTRUE(r$has_tokens)
      }
      list(prod = m, k = k, has_tokens = h)
    }, mc.cores = nc)
    prodm  <- do.call(rbind, lapply(parts, `[[`, "prod"))
    kvec   <- unlist(lapply(parts, `[[`, "k"), use.names = FALSE)
    hastok <- unlist(lapply(parts, `[[`, "has_tokens"), use.names = FALSE)
  } else {
    for (i in seq_len(u)) {
      r <- resolver(values[i], specs, n_groups)
      if (!is.null(r$prod)) prodm[i, ] <- r$prod
      kvec[i]   <- r$k
      hastok[i] <- isTRUE(r$has_tokens)
      if (!is.null(tick)) tick()
    }
  }
  list(prod = prodm, k = kvec, has_tokens = hastok)
}

## Turn per-row evidence products into posteriors: divide by
## prior^(k - 1) for rows with more than one evidence piece (the
## Naive-Bayes combination in combine_bayes_n()), renormalize, and set
## rows with no evidence (k == 0) or a degenerate normalizer to NA.
posterior_from_prod <- function(prodm, kvec, priorv) {
  n_groups <- length(priorv)
  npost <- matrix(NA_real_, nrow = length(kvec), ncol = n_groups)
  rows <- which(kvec >= 1L)
  if (length(rows) == 0L) return(npost)
  num <- prodm[rows, , drop = FALSE]
  multi <- kvec[rows] > 1L
  if (any(multi)) {
    denom <- outer(kvec[rows][multi] - 1L, priorv, function(k, p) p^k)
    sub <- num[multi, , drop = FALSE] / denom
    sub[, priorv == 0] <- 0            # combine_bayes_n(): zero prior wins
    num[multi, ] <- sub
  }
  z <- rowSums(num, na.rm = TRUE)
  bad <- !is.finite(z) | z == 0
  res <- num / z
  res[bad, ] <- NA_real_
  npost[rows, ] <- res
  npost
}

## Validate a user-supplied name dictionary and return its group columns.
check_name_dict <- function(df, what) {
  if (!is.data.frame(df)) {
    stop("`", what, "` must be a data.frame.", call. = FALSE)
  }
  if (!"name" %in% names(df)) {
    stop("`", what, "` must have a `name` column.", call. = FALSE)
  }
  groups <- setdiff(names(df), c("name", "frequency"))
  groups <- groups[vapply(df[groups], is.numeric, logical(1))]
  if (length(groups) < 2L) {
    stop("`", what, "` must have at least two numeric category columns ",
         "besides `name` (and optionally `frequency`).", call. = FALSE)
  }
  groups
}

#' Vectorized demographic prediction with pluggable dictionaries
#'
#' `predict_demog()` is the vectorized successor to [predict_names()]:
#' the same probability model (compound-first cascade for given-name
#' fields, per-token cascade for surname fields, maiden replacing last,
#' Naive-Bayes combination with the *(k − 1)* prior division, and the
#' BISG geography fold), but computed via deduplicated lookups and
#' matrix algebra instead of a per-row loop — typically orders of
#' magnitude faster on large data frames. With the default (bundled)
#' tables it reproduces the race columns of [predict_names()] to
#' floating-point precision and, like [predict_names()], appends a
#' `p_female` column computed from the first-name-by-sex table.
#'
#' Unlike [predict_names()], the tables are pluggable and the category
#' grouping is not fixed: supply your own name dictionary and/or
#' geography table and the prediction categories are read off the
#' supplied tables' columns. Any component you do not supply falls back
#' to the bundled 2020 Census tables and their six race /
#' Hispanic-origin categories — in that case a user-supplied component
#' must use those same six categories (an informative error is raised
#' otherwise).
#'
#' @section Recognized input columns:
#' Detection is case-insensitive, as in [predict_names()]: any subset of
#' the name fields `first`, `middle`, `last`, `maiden` and the
#' geography fields `zcta`, `tract`, `block_group`, `block`. When
#' `geo_dict` is supplied, a `geoid` column is also recognized and takes
#' precedence; its values (and, with `geo_dict`, the values of any other
#' geography column) are matched against `geo_dict$geoid` as-is, with no
#' normalization. With the bundled geography tables the usual ID
#' normalization applies (ZIP zero-padding, `"1400000US"` /
#' `"1500000US"` / `"7500000US"` prefix stripping) and the most specific
#' column wins (`block` > `block_group` > `tract` > `zcta`).
#'
#' @section Block-level geography and the block-group fallback:
#' A `block` column holds 15-digit 2020 Census Block GEOIDs and is
#' matched against [geo_block_vap], which covers the 5,704,969 blocks
#' with any voting-age population (50 states, DC, and Puerto Rico) and
#' stores integer VAP counts (row-normalized to proportions before the
#' fold). Two situations reroute a row to the block's parent block
#' group (`substr(geoid, 1, 12)`), each reported once per call via
#' [message()] (suppressible with [suppressMessages()]):
#' \itemize{
#'   \item `geography_type = "cvap"` — no block-level CVAP table exists
#'     (citizenship is not collected in the decennial census), so every
#'     block row uses its block group's CVAP proportions instead.
#'   \item A block GEOID not among the populated 2020 blocks (zero-VAP
#'     block, or an ID that does not exist). With
#'     `block_fallback = TRUE` (default) such rows use their block
#'     group's VAP proportions, and the message reports how many rows
#'     fell back; with `block_fallback = FALSE` they get no geography
#'     component (name-only posterior), and the message reports how
#'     many rows were skipped.
#' }
#' Populated-block rows additionally blend `block_shrink` pseudo-people
#' drawn from the parent block group's composition into the block's
#' counts before normalizing (default `10`; `0` restores the raw
#' counts) — see [geo_prior()], **Block-count shrinkage toward the
#' block group**, for the formula and the validation behind the
#' default.
#'
#' `block_fallback` and `block_shrink` are ignored when `geo_dict` is
#' supplied (a `block` column is then matched against `geo_dict$geoid`
#' as-is, like any other geography column).
#'
#' @section User-supplied name dictionaries:
#' `name_dict` may be a single data frame (used for given names and
#' surnames alike) or a named list with elements `first` and/or `last`.
#' Each dictionary needs a `name` column plus two or more numeric
#' category columns; a `frequency` column is optional and, when
#' present, is used to derive the marginal prior. Rows are renormalized
#' to sum to 1; names are normalized like [normalize_name()] before
#' indexing. Lookup uses the same five-step cascade as [lookup_name()],
#' with the same cross-dictionary fallback as the bundled path: given
#' names try `first` then `last`, surnames try `last` then `first`
#' (whichever of the two are available). When both `first` and `last`
#' are supplied their category columns must match.
#'
#' Because the categories are arbitrary, the bundled sex table works as
#' a dictionary too:
#' `predict_demog(df, name_dict = list(first = first_names_sex))`
#' returns `p_male` / `p_female`.
#'
#' @section User-supplied geography:
#' `geo_dict` is a data frame with a `geoid` column plus one numeric
#' column per category (a `total` column is optional and used only for
#' prior derivation). Rows are renormalized to sum to 1. When both
#' `name_dict` and `geo_dict` are supplied, their category columns must
#' agree (order may differ; the name dictionary's order wins). When
#' only `geo_dict` is supplied and name columns are present, the
#' bundled name tables are used, so `geo_dict` must use the six
#' [race_groups()] categories.
#'
#' @section The marginal prior:
#' The Naive-Bayes combination divides by `P(category)^(k-1)` and the
#' geography fold divides by `P(category)` once. The prior is taken
#' from, in order: the `prior` argument; the `prior` attribute of the
#' supplied `last` (then `first`) dictionary; a frequency-weighted
#' average of the supplied dictionary's rows; a `total`-weighted
#' average of `geo_dict`; the bundled Census prior when the categories
#' are the six bundled ones. If none of these applies, a uniform prior
#' is used and a warning is raised whenever the prior materially enters
#' the computation (some row has more than one evidence piece, or a
#' name posterior is folded with geography).
#'
#' @param data A data frame with any subset of the recognized columns.
#' @param name_dict Optional user name dictionary — a data frame, or a
#'   list with elements `first` and/or `last` (see Details). `NULL`
#'   (default) uses the bundled Census 2020 tables.
#' @param geo_dict Optional user geography table with a `geoid` column
#'   (see Details). `NULL` (default) uses the bundled CVAP / VAP tables
#'   selected by `geography_type`.
#' @param prior Optional named numeric vector over the category
#'   grouping — the marginal `P(category)` used by the Naive-Bayes
#'   combination and the geography fold. Renormalized to sum to 1.
#' @param include_extra Bundled tables only: fall back to the
#'   Rosenman, Olivella, and Imai (2023) voter-file tables for names
#'   absent from Census 2020, as in [predict_names()]. Ignored (with a
#'   warning) when `name_dict` is supplied.
#' @param geography_type `"cvap"` (default), `"vap"`, or `"pop"` —
#'   selects the bundled geography table when `geo_dict` is not
#'   supplied. `"pop"` (total population of all ages, P.L. 94-171
#'   Table P2 — the `wru` package's basis) covers tract / block group
#'   / block only and requires the separately built `geo_*_pop`
#'   tables — see [geo_prior()].
#' @param geo_smooth Pseudo-count, in people, used to shrink each
#'   geography's composition toward the population-weighted marginal of
#'   the whole geography table before the fold — see [geo_prior()] for
#'   the formula and rationale. Default `1`, matching [predict_race()] /
#'   [predict_names()]; `0` folds in the published shares unchanged.
#'   With a user-supplied `geo_dict`, smoothing needs a scale for each
#'   row and so applies only when `geo_dict` has a `total` column;
#'   without one it is silently skipped.
#' @param block_fallback When `TRUE` (default), rows whose `block`
#'   GEOID is not among the populated 2020 blocks fall back to their
#'   parent block group's proportions — see **Block-level geography
#'   and the block-group fallback**. When `FALSE`, those rows get no
#'   geography component. Only consulted for a `block` column with the
#'   bundled tables.
#' @param block_shrink Pseudo-count, in people, of the parent block
#'   group's composition blended into each populated block's VAP
#'   counts before normalizing — a Dirichlet prior with the block
#'   group as the base measure; see [geo_prior()], **Block-count
#'   shrinkage toward the block group**. Default `10`; set to `0` for
#'   the raw 0.7.0 block counts. Only consulted for a `block` column
#'   with the bundled tables and `geography_type = "vap"`.
#' @param include_sex Bundled name tables only: when `TRUE` (default),
#'   append a `p_female` column computed from the first-name-by-sex
#'   table (`first` field only, compound-first cascade), exactly as
#'   [predict_names()]. Not applicable when `name_dict` is supplied —
#'   the categories are then read off the dictionary, and sex is just
#'   another grouping (see the `first_names_sex` example).
#' @param progress If `TRUE` (default), prints a one-line text progress
#'   bar to `stderr` while the unique name values are resolved through
#'   the matching cascade, showing percent complete, elapsed time, and
#'   an estimated time remaining. Pass `FALSE` to suppress (e.g. inside
#'   non-interactive scripts or when capturing output). The bar is only
#'   drawn for the serial path (`n_cores == 1L`); in the parallel path
#'   it is replaced by a single start / finish status line.
#' @param n_cores Number of worker processes used to resolve the unique
#'   name values. Default `1L` (serial). When `n_cores > 1L`, the
#'   unique values are split into chunks and resolved via
#'   `parallel::mclapply` (fork-based, so the bundled lookup tables are
#'   shared via copy-on-write). On Windows fork is unavailable, so
#'   values above 1 are ignored and resolution runs serially. Because
#'   the cascade only runs once per unique value, extra cores help
#'   mainly when the input has many distinct names.
#' @return A data frame with `nrow(data)` rows and one `p_<category>`
#'   column per category (in the canonical order described above),
#'   summing to 1 per row when any evidence matched and `NA_real_`
#'   otherwise. With the bundled name tables (`name_dict = NULL`) and
#'   `include_sex = TRUE`, a `p_female` column is appended
#'   (`P(female | first)`; `1 - p_female` gives `P(male)`; `NA_real_`
#'   when no first-name input matched the sex table) — the same
#'   7-column shape as [predict_names()].
#' @seealso [predict_names()] (per-row equivalent),
#'   [predict_race()] (single-record detail), [geo_prior()].
#' @examples
#' df <- data.frame(
#'   first = c("Maria",  "John",  "Mary Ann"),
#'   last  = c("Garcia", "Smith", "Johnson"),
#'   zcta  = c("30307",  "10001", "94110"),
#'   stringsAsFactors = FALSE
#' )
#' ## Bundled tables: same output as predict_names() (race + p_female),
#' ## orders of magnitude faster.
#' predict_demog(df)
#'
#' ## Sex-only categories, via the bundled sex table as a dictionary.
#' predict_demog(data.frame(first = c("Michael", "Maria Jose")),
#'               name_dict = list(first = first_names_sex))
#'
#' ## Fully custom categories: toy two-category dictionaries.
#' nd <- data.frame(name = c("ALICE", "BOB"),
#'                  urban = c(0.8, 0.3), rural = c(0.2, 0.7))
#' gd <- data.frame(geoid = c("A1", "B2"),
#'                  urban = c(0.9, 0.2), rural = c(0.1, 0.8))
#' predict_demog(data.frame(first = c("Alice", "Bob"),
#'                          geoid = c("A1", "B2")),
#'               name_dict = nd, geo_dict = gd,
#'               prior = c(urban = 0.5, rural = 0.5))
#' @export
predict_demog <- function(data,
                          name_dict = NULL,
                          geo_dict = NULL,
                          prior = NULL,
                          include_extra = FALSE,
                          geography_type = c("cvap", "vap", "pop"),
                          geo_smooth = 1,
                          block_fallback = TRUE,
                          block_shrink = 10,
                          include_sex = TRUE,
                          progress = TRUE,
                          n_cores = 1L) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  geography_type <- match.arg(geography_type)
  geo_smooth <- check_geo_smooth(geo_smooth)
  block_shrink <- check_block_shrink(block_shrink)
  n_cores <- resolve_n_cores(n_cores, nrow(data))

  ## ---- Column detection (case-insensitive, as in predict_names) ----
  recognized_names <- c("first", "middle", "last", "maiden")
  recognized_geo   <- c("geoid", "block", "block_group", "tract", "zcta")
  lower_names <- tolower(names(data))
  match_idx <- match(c(recognized_names, recognized_geo), lower_names)
  col_map <- stats::setNames(names(data)[match_idx],
                             c(recognized_names, recognized_geo))

  name_cols <- recognized_names[!is.na(col_map[recognized_names])]
  geo_present <- recognized_geo[!is.na(col_map[recognized_geo])]
  if ("geoid" %in% geo_present && is.null(geo_dict)) {
    stop("A `geoid` column requires `geo_dict` (the bundled tables are ",
         "keyed by `zcta`, `tract`, or `block_group`).", call. = FALSE)
  }
  if (is.null(geo_dict)) geo_present <- setdiff(geo_present, "geoid")
  geo_level <- if (length(geo_present)) geo_present[1] else NA_character_

  if (length(name_cols) == 0L && is.na(geo_level)) {
    stop("No recognized columns in `data`. Expected any of ",
         "(case-insensitive): ",
         paste(c(recognized_names, "zcta", "tract", "block_group", "block",
                 if (!is.null(geo_dict)) "geoid"), collapse = ", "), ".",
         call. = FALSE)
  }

  ## ---- Resolve dictionaries, category grouping, and lookup chains ----
  if (!is.null(name_dict) && isTRUE(include_extra)) {
    warning("`include_extra` is ignored when `name_dict` is supplied.",
            call. = FALSE)
    include_extra <- FALSE
  }

  if (is.null(name_dict)) {
    name_groups <- race_groups()
    given_tabs   <- c("first", "last")
    surname_tabs <- c("last", "first")
    if (isTRUE(include_extra)) {
      given_tabs   <- expand_extra_tables(given_tabs)
      surname_tabs <- expand_extra_tables(surname_tabs)
    }
    given_specs   <- lapply(given_tabs, demog_spec_bundled)
    surname_specs <- lapply(surname_tabs, demog_spec_bundled)
    dict_first <- NULL
    dict_last  <- NULL
  } else {
    if (is.data.frame(name_dict)) {
      name_dict <- list(first = name_dict, last = name_dict)
    }
    if (!is.list(name_dict) ||
        !any(c("first", "last") %in% names(name_dict))) {
      stop("`name_dict` must be a data.frame or a list with elements ",
           "`first` and/or `last`.", call. = FALSE)
    }
    dict_first <- name_dict[["first"]]
    dict_last  <- name_dict[["last"]]
    g_first <- if (!is.null(dict_first)) check_name_dict(dict_first, "name_dict$first")
    g_last  <- if (!is.null(dict_last))  check_name_dict(dict_last,  "name_dict$last")
    if (!is.null(g_first) && !is.null(g_last)) {
      if (!setequal(g_first, g_last)) {
        stop("`name_dict$first` and `name_dict$last` must have the same ",
             "category columns (got: ", paste(g_first, collapse = ", "),
             " vs ", paste(g_last, collapse = ", "), ").", call. = FALSE)
      }
    }
    name_groups <- if (!is.null(g_first)) g_first else g_last
    spec_first <- if (!is.null(dict_first)) {
      demog_spec_custom(dict_first, name_groups, is_last = FALSE,
                        what = "name_dict$first")
    }
    spec_last <- if (!is.null(dict_last)) {
      demog_spec_custom(dict_last, name_groups, is_last = TRUE,
                        what = "name_dict$last")
    }
    given_specs   <- Filter(Negate(is.null), list(spec_first, spec_last))
    surname_specs <- Filter(Negate(is.null), list(spec_last, spec_first))
  }

  ## Geography table and the canonical grouping.
  use_geo <- !is.na(geo_level)
  if (use_geo && !is.null(geo_dict)) {
    if (!is.data.frame(geo_dict) || !"geoid" %in% names(geo_dict)) {
      stop("`geo_dict` must be a data.frame with a `geoid` column.",
           call. = FALSE)
    }
    geo_groups <- setdiff(names(geo_dict), c("geoid", "total"))
    geo_groups <- geo_groups[vapply(geo_dict[geo_groups], is.numeric,
                                    logical(1))]
    if (length(geo_groups) < 2L) {
      stop("`geo_dict` must have at least two numeric category columns ",
           "besides `geoid` (and optionally `total`).", call. = FALSE)
    }
  } else if (use_geo) {
    geo_groups <- race_groups()
  } else {
    geo_groups <- NULL
  }

  if (length(name_cols) > 0L && use_geo &&
      !setequal(name_groups, geo_groups)) {
    stop("The name dictionary categories (",
         paste(name_groups, collapse = ", "),
         ") and the geography categories (",
         paste(geo_groups, collapse = ", "),
         ") must match. Supply compatible `name_dict` / `geo_dict` ",
         "tables, or drop the mismatched component.", call. = FALSE)
  }
  groups <- if (length(name_cols) > 0L) name_groups else geo_groups
  n_groups <- length(groups)

  ## ---- Marginal prior over `groups` ----
  prior_uniform <- FALSE
  if (!is.null(prior)) {
    pv <- prior[groups]
    if (any(is.na(pv)) || sum(pv) <= 0) {
      stop("`prior` must be a named numeric vector covering every ",
           "category (", paste(groups, collapse = ", "),
           ") and summing to a positive value.", call. = FALSE)
    }
    priorv <- as.numeric(pv) / sum(pv)
  } else if (is.null(name_dict) && identical(groups, race_groups())) {
    priorv <- as.numeric(attr(table_df("last"), "prior")[groups])
  } else {
    derive_from_dict <- function(df) {
      if (is.null(df)) return(NULL)
      ap <- attr(df, "prior")
      if (!is.null(ap) && all(groups %in% names(ap))) {
        return(as.numeric(ap[groups]) / sum(as.numeric(ap[groups])))
      }
      f <- df[["frequency"]]
      if (!is.null(f) && any(is.finite(f) & f > 0)) {
        w <- ifelse(is.finite(f) & f > 0, f, 0)
        pm <- as.matrix(df[, groups, drop = FALSE])
        v <- colSums(pm * w, na.rm = TRUE)
        if (sum(v) > 0) return(as.numeric(v) / sum(v))
      }
      NULL
    }
    priorv <- derive_from_dict(dict_last)
    if (is.null(priorv)) priorv <- derive_from_dict(dict_first)
    if (is.null(priorv) && use_geo && !is.null(geo_dict) &&
        !is.null(geo_dict[["total"]])) {
      w <- geo_dict[["total"]]
      w <- ifelse(is.finite(w) & w > 0, w, 0)
      pm <- as.matrix(geo_dict[, groups, drop = FALSE])
      rs <- rowSums(pm)
      ok <- is.finite(rs) & rs > 0
      v <- colSums((pm / rs)[ok, , drop = FALSE] * w[ok], na.rm = TRUE)
      if (sum(v) > 0) priorv <- as.numeric(v) / sum(v)
    }
    if (is.null(priorv)) {
      priorv <- rep(1 / n_groups, n_groups)
      prior_uniform <- TRUE
    }
  }
  names(priorv) <- groups

  ## ---- Per-field values ("" marks an empty cell) ----
  n <- nrow(data)
  col_values <- function(key) {
    col <- col_map[[key]]
    if (is.na(col)) return(rep("", n))
    v <- as.character(data[[col]])
    v[is.na(v)] <- ""
    v
  }
  fv <- if ("first"  %in% name_cols) col_values("first")  else rep("", n)
  mv <- if ("middle" %in% name_cols) col_values("middle") else rep("", n)
  lv <- if ("last"   %in% name_cols) col_values("last")   else rep("", n)
  xv <- if ("maiden" %in% name_cols) col_values("maiden") else rep("", n)

  ## ---- Resolve unique name values through the cascade ----
  ## Sex is computed on the bundled path only, from the `first` field
  ## alone, mirroring predict_names() / predict_race().
  sex_enabled <- isTRUE(include_sex) && is.null(name_dict)
  given_u   <- setdiff(unique(c(fv, mv)), "")
  surname_u <- setdiff(unique(c(lv, xv)), "")
  sex_u     <- if (sex_enabled) setdiff(unique(fv), "") else character(0)

  use_parallel <- n_cores > 1L
  total_units  <- length(given_u) + length(surname_u) + length(sex_u)
  tick <- NULL
  show_progress <- isTRUE(progress) && total_units > 0L && !use_parallel
  if (show_progress) {
    pb <- make_progress_bar(total_units)
    pb$start()
    tick <- pb$tick
  }
  if (use_parallel && isTRUE(progress) && total_units > 0L) {
    cat(sprintf(
      "predict_demog: resolving %d unique name values on %d cores...\n",
      total_units, n_cores
    ), file = stderr())
  }
  res_start <- Sys.time()

  gres <- resolve_uniques(given_u,   resolve_given_value,   given_specs,
                          n_groups, n_cores = n_cores, tick = tick)
  sres <- resolve_uniques(surname_u, resolve_surname_value, surname_specs,
                          n_groups, n_cores = n_cores, tick = tick)
  if (sex_enabled) {
    sex_grp <- attr(table_df("first_sex"), "groups")
    xres <- resolve_uniques(sex_u, resolve_given_value,
                            list(demog_spec_bundled("first_sex")),
                            length(sex_grp), n_cores = n_cores, tick = tick)
  }

  if (show_progress) pb$done()
  if (use_parallel && isTRUE(progress) && total_units > 0L) {
    cat(sprintf(
      "predict_demog: done in %.1fs.\n",
      as.numeric(difftime(Sys.time(), res_start, units = "secs"))
    ), file = stderr())
  }

  fidx <- match(fv, given_u)
  midx <- match(mv, given_u)
  lidx <- match(lv, surname_u)
  xidx <- match(xv, surname_u)

  ## Maiden replaces last whenever the maiden cell has any token,
  ## matched or not — same rule as predict_race().
  maiden_has <- !is.na(xidx) & sres$has_tokens[ifelse(is.na(xidx), 1L, xidx)]
  sidx <- lidx
  sidx[maiden_has] <- xidx[maiden_has]

  ## ---- Combine evidence: prod over fields, divide by prior^(k-1) ----
  P <- matrix(1, nrow = n, ncol = n_groups)
  K <- integer(n)
  fold_field <- function(idx, res) {
    ok <- !is.na(idx)
    if (any(ok)) {
      P[ok, ] <<- P[ok, , drop = FALSE] * res$prod[idx[ok], , drop = FALSE]
      K[ok]   <<- K[ok] + res$k[idx[ok]]
    }
  }
  fold_field(fidx, gres)   # multiplication order matches predict_race():
  fold_field(midx, gres)   # first, then middle, then the surname source
  fold_field(sidx, sres)

  matched <- K >= 1L
  npost <- posterior_from_prod(P, K, priorv)

  ## ---- Geography fold ----
  out <- npost
  geo_name_fold <- FALSE
  if (use_geo) {
    gv <- col_values(geo_level)
    geo_u <- setdiff(unique(gv), "")
    block_native <- FALSE       # bundled block-level lookup in effect
    if (!is.null(geo_dict)) {
      tbl_ids <- as.character(geo_dict$geoid)
      tbl_pm  <- as.matrix(geo_dict[, groups, drop = FALSE])
      tbl_tot <- geo_dict[["total"]]
      ## No `total` column means no scale to smooth against.
      smooth_target <- if (!is.null(tbl_tot)) {
        geo_marginal(geo_dict, groups, tbl_tot)
      }
      keys <- geo_u
    } else {
      lookup_level <- geo_level
      if (geo_level == "block") {
        keys <- vapply(geo_u, normalize_block, character(1),
                       USE.NAMES = FALSE)
        if (geography_type == "cvap") {
          ## No block-level CVAP exists (citizenship is not collected in
          ## the decennial census); every block row uses its parent
          ## block group's CVAP proportions instead.
          if (length(geo_u)) {
            message("predict_demog(): no block-level CVAP table exists ",
                    "(citizenship is not collected in the decennial ",
                    "census); using each block's parent block group ",
                    "from `geo_bg_cvap` for the geography component.")
          }
          lookup_level <- "block_group"
          keys <- ifelse(is.na(keys), NA_character_,
                         substr(keys, 1L, 12L))
        } else {
          block_native <- TRUE
        }
      } else {
        normalizer <- switch(geo_level,
                             zcta        = normalize_zcta,
                             tract       = normalize_tract,
                             block_group = normalize_block_group)
        keys <- vapply(geo_u, normalizer, character(1), USE.NAMES = FALSE)
      }
      tbl <- geo_table(lookup_level, geography_type)
      tbl_ids <- tbl$geoid
      tbl_pm  <- as.matrix(tbl[, groups, drop = FALSE])
      tbl_tot <- tbl$total
      smooth_target <- geo_national(lookup_level, geography_type)[groups]
    }
    storage.mode(tbl_pm) <- "double"
    ridx <- match(keys, tbl_ids)
    upm <- matrix(NA_real_, nrow = length(geo_u), ncol = n_groups)
    okr <- !is.na(ridx)
    upm[okr, ] <- tbl_pm[ridx[okr], , drop = FALSE]

    ## Per-unique-value smoothing scale and shrinkage target, so
    ## block-group fallback rows can carry their own table's total and
    ## marginal alongside the main table's.
    utot <- rep(0, length(geo_u))
    if (!is.null(tbl_tot)) {
      utot[okr] <- suppressWarnings(as.numeric(tbl_tot)[ridx[okr]])
      utot[!is.finite(utot) | utot <= 0] <- 0
    }
    tgtm <- if (!is.null(smooth_target)) {
      matrix(as.numeric(smooth_target), nrow = length(geo_u),
             ncol = n_groups, byrow = TRUE)
    }

    ## ---- Block -> block-group fallback (bundled VAP path only) ----
    ## `geo_block_vap` covers populated blocks only; a valid 15-digit
    ## GEOID that misses is a zero-VAP block (or a nonexistent one).
    ublock_miss <- logical(length(geo_u))
    ufell_back  <- logical(length(geo_u))
    if (block_native) {
      ublock_miss <- !okr & !is.na(keys)
      if (any(ublock_miss) && isTRUE(block_fallback)) {
        bg <- geo_table("block_group", geography_type)
        bgi <- match(substr(keys[ublock_miss], 1L, 12L), bg$geoid)
        bghit <- !is.na(bgi)
        if (any(bghit)) {
          rows_fb <- which(ublock_miss)[bghit]
          bg_pm <- as.matrix(bg[, groups, drop = FALSE])
          storage.mode(bg_pm) <- "double"
          upm[rows_fb, ] <- bg_pm[bgi[bghit], , drop = FALSE]
          bt <- suppressWarnings(as.numeric(bg$total)[bgi[bghit]])
          bt[!is.finite(bt) | bt <= 0] <- 0
          utot[rows_fb] <- bt
          if (!is.null(tgtm)) {
            bg_target <- geo_national("block_group", geography_type)[groups]
            tgtm[rows_fb, ] <- matrix(as.numeric(bg_target),
                                      nrow = length(rows_fb),
                                      ncol = n_groups, byrow = TRUE)
          }
          ufell_back[rows_fb] <- TRUE
        }
      }
    }

    ## ---- Dirichlet blend of block counts with the parent block group ----
    ## Populated-block rows only (bundled VAP path): add `block_shrink`
    ## pseudo-people drawn from the parent block group's composition to
    ## the integer counts, and raise the smoothing scale to match --
    ## arithmetic identical to geo_prior()'s block_shrink.
    if (block_native && block_shrink > 0 && any(okr)) {
      bg_sh <- geo_table("block_group", geography_type)
      rows_ok <- which(okr)
      sgi <- match(substr(keys[rows_ok], 1L, 12L), bg_sh$geoid)
      sgh <- !is.na(sgi)
      if (any(sgh)) {
        bg_shm <- as.matrix(bg_sh[, groups, drop = FALSE])
        storage.mode(bg_shm) <- "double"
        bp  <- bg_shm[sgi[sgh], , drop = FALSE]
        brs <- rowSums(bp)
        usable <- is.finite(brs) & brs > 0
        if (any(usable)) {
          rows_sh <- rows_ok[sgh][usable]
          upm[rows_sh, ] <- upm[rows_sh, , drop = FALSE] +
            block_shrink * (bp[usable, , drop = FALSE] / brs[usable])
          utot[rows_sh] <- utot[rows_sh] + block_shrink
        }
      }
    }

    rs <- rowSums(upm)
    valid <- is.finite(rs) & rs > 0   # geo_prior(): NA/zero rows miss
    upm[valid, ] <- upm[valid, , drop = FALSE] / rs[valid]
    upm[!valid, ] <- NA_real_

    ## Pseudo-count shrinkage toward the table's marginal, arithmetic
    ## identical to smooth_geo_probs() so predict_race() parity holds.
    if (geo_smooth > 0 && !is.null(tgtm) && !is.null(tbl_tot) &&
        any(valid)) {
      vi <- which(valid)
      upm[vi, ] <- (utot[vi] * upm[vi, , drop = FALSE] +
                      geo_smooth * tgtm[vi, , drop = FALSE]) /
        (utot[vi] + geo_smooth)
    }

    gmap <- match(gv, geo_u)

    ## ---- Report rows not matched to a populated block ----
    if (block_native && any(ublock_miss)) {
      gm1 <- ifelse(is.na(gmap), 1L, gmap)
      has_id <- !is.na(gmap)
      rmiss <- has_id & ublock_miss[gm1]
      n_miss <- sum(rmiss)
      if (n_miss > 0L) {
        n_block_rows <- sum(has_id)
        if (isTRUE(block_fallback)) {
          n_used <- sum(rmiss & ufell_back[gm1] & valid[gm1])
          msg <- sprintf(
            paste0("predict_demog(): %s of %s `block` row(s) were not ",
                   "matched to a populated 2020 census block; falling ",
                   "back to their block group's proportions for the ",
                   "geography component (disable with ",
                   "`block_fallback = FALSE`)."),
            format(n_miss, big.mark = ","),
            format(n_block_rows, big.mark = ","))
          if (n_used < n_miss) {
            msg <- paste0(msg, sprintf(
              paste0(" For %s of those the block group had no usable ",
                     "row either, so no geography component was ",
                     "applied."),
              format(n_miss - n_used, big.mark = ",")))
          }
        } else {
          msg <- sprintf(
            paste0("predict_demog(): %s of %s `block` row(s) were not ",
                   "matched to a populated 2020 census block; ",
                   "`block_fallback = FALSE`, so no geography component ",
                   "was applied to those rows."),
            format(n_miss, big.mark = ","),
            format(n_block_rows, big.mark = ","))
        }
        message(msg)
      }
    }
    geo_row <- !is.na(gmap) & valid[ifelse(is.na(gmap), 1L, gmap)]
    geo_name_fold <- any(geo_row & matched)
    if (any(geo_row)) {
      rows <- which(geo_row)
      base <- npost[rows, , drop = FALSE]
      noname <- !matched[rows]
      if (any(noname)) {
        base[noname, ] <- matrix(priorv, nrow = sum(noname),
                                 ncol = n_groups, byrow = TRUE)
      }
      gm <- upm[gmap[rows], , drop = FALSE]
      num <- sweep(base * gm, 2L, ifelse(priorv == 0, 1, priorv), "/")
      num[, priorv == 0] <- 0            # combine_name_geo(): zero prior wins
      z <- rowSums(num, na.rm = TRUE)
      bad <- !is.finite(z) | z == 0
      res <- num / z
      res[bad, ] <- NA_real_
      out[rows, ] <- res
    }
  }

  if (prior_uniform && (any(K > 1L) || geo_name_fold)) {
    warning("No prior could be derived from the supplied dictionaries; ",
            "a uniform prior over the categories was used. Supply ",
            "`prior` to control the marginal P(category).", call. = FALSE)
  }

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- paste0("p_", groups)

  ## ---- Sex posterior over the unique first-name values ----
  if (sex_enabled) {
    sex_prior <- as.numeric(attr(table_df("first_sex"), "prior")[sex_grp])
    spost <- posterior_from_prod(xres$prod, xres$k, sex_prior)
    fem <- spost[, match("female", sex_grp)]
    out$p_female <- fem[match(fv, sex_u)]
  }
  out
}
