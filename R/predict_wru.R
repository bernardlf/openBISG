## wru-replication mode: surname-only BISG in wru's five categories, fed
## entirely from the bundled geography tables (no Census API download).
##
## wru's BISG computes P(R | G, S) proportional to P(G | R) P(R | S),
## where P(G | R) = N_gr / N_state,r is taken from Census data downloaded
## per state at the chosen geography level. Writing P(G | R) =
## P(R | G) P(G) / P(R) shows this is the openBISG fold
## P(R | S) P(R | G) / P(R) with the STATE's racial composition (at the
## same level and population basis) as the marginal P(R). predict_wru()
## therefore wraps predict_demog() with:
##   * a surname-only name dictionary in wru's five categories,
##     collapsed from [last_names] BEFORE the fold, as wru collapses the
##     Census surname list: whi = white, bla = black, his = hispanic,
##     asi = aapi, oth = aian + nh_multi;
##   * a geography dictionary collapsed the same way from the bundled
##     table for the requested level (block / block group / tract);
##   * prior = the per-state composition summed from that table;
##   * geo_smooth = 0 (wru folds raw counts) and no block fallback;
##   * the total-population basis by default, matching wru's Census
##     downloads (P.L. 94-171 Table P2).

WRU_GROUPS <- c("whi", "bla", "his", "asi", "oth")

## Collapse a bundled six-group table (name or geography) to wru's five
## categories. Works for proportion rows and count rows alike.
collapse_to_wru <- function(tbl, id_col) {
  out <- data.frame(
    tbl[[id_col]],
    whi = tbl$white,
    bla = tbl$black,
    his = tbl$hispanic,
    asi = tbl$aapi,
    oth = tbl$aian + tbl$nh_multi,
    stringsAsFactors = FALSE
  )
  names(out)[1] <- id_col
  out
}

## Cached wru-collapsed surname dictionary. include_extra = TRUE appends
## the voter-file supplement rows for names absent from the Census list,
## mirroring predict_names(include_extra = TRUE).
wru_name_dict <- function(include_extra) {
  key <- paste0("wru_name_dict_", isTRUE(include_extra))
  cached <- get0(key, envir = .openBISG_caches, inherits = FALSE,
                 ifnotfound = NULL)
  if (!is.null(cached)) return(cached)
  base <- collapse_to_wru(table_df("last"), "name")
  if (isTRUE(include_extra)) {
    extra <- collapse_to_wru(table_df("last_extra"), "name")
    extra <- extra[!(extra$name %in% base$name), , drop = FALSE]
    base <- rbind(base, extra)
  }
  assign(key, base, envir = .openBISG_caches)
  base
}

## Cached wru-collapsed geography dictionary for one (level, type), plus
## the per-state five-category counts used to build state priors. For
## the proportion-valued tables the counts are share * total; rows with
## a missing or non-positive total contribute nothing to the state sums
## (and miss the fold, as in geo_prior()).
wru_geo_dict <- function(level, type) {
  key <- paste0("wru_geo_dict_", level, "_", type)
  cached <- get0(key, envir = .openBISG_caches, inherits = FALSE,
                 ifnotfound = NULL)
  if (!is.null(cached)) return(cached)
  tbl <- geo_table(level, type)
  gd <- collapse_to_wru(tbl, "geoid")
  gd$total <- tbl$total
  pm <- as.matrix(gd[, WRU_GROUPS])
  storage.mode(pm) <- "double"
  rs <- rowSums(pm)
  w  <- suppressWarnings(as.numeric(gd$total))
  ok <- is.finite(w) & w > 0 & is.finite(rs) & rs > 0
  cnts <- pm[ok, , drop = FALSE] / rs[ok] * w[ok]
  state_counts <- rowsum(cnts, substr(gd$geoid[ok], 1L, 2L))
  out <- list(dict = gd, state_counts = state_counts)
  assign(key, out, envir = .openBISG_caches)
  out
}

#' wru-style BISG from the bundled geography tables
#'
#' Replicates the estimation of the `wru` package's standard BISG
#' (Imai and Khanna 2016) — surname times Census geography, in wru's
#' five racial categories — without downloading Census data: the
#' geography component comes from the bundled openBISG tables at the
#' Census Block, Block Group, or Tract level.
#'
#' `wru` computes \eqn{P(R \mid G, S) \propto P(G \mid R) P(R \mid S)},
#' with \eqn{P(G \mid R)} taken from Census counts downloaded for the
#' voter's state at the chosen geography level. Since
#' \eqn{P(G \mid R) = P(R \mid G) P(G) / P(R)}, that is algebraically
#' the openBISG fold \eqn{P(R \mid S) P(R \mid G) / P(R)} with the
#' state's racial composition — at the same level and population basis
#' — as the marginal \eqn{P(R)}. `predict_wru()` runs exactly that
#' fold, state by state, entirely from the bundled tables.
#'
#' Fidelity to `wru`:
#' \itemize{
#'   \item The six openBISG groups are collapsed to wru's five
#'     **before** the fold, as wru collapses the Census surname list:
#'     `whi` = white, `bla` = black, `his` = hispanic, `asi` = aapi,
#'     `oth` = aian + nh_multi.
#'   \item Raw counts are folded (`geo_smooth = 0` by default), so — as
#'     in wru — a zero Census cell forces that group's posterior to
#'     zero. Pass `geo_smooth > 0` to depart from wru here.
#'   \item There is no block-group fallback and no `block_shrink`:
#'     rows whose geography misses the table carry the surname-only
#'     posterior (reported once per call via [message()]).
#'   \item Rows whose surname finds no dictionary entry carry the
#'     geography-only posterior, wru's `impute.missing` analog.
#' }
#'
#' Known departures: surnames are cleaned and compound-matched by
#' openBISG's cascade rather than wru's string handling, and no age /
#' sex / party conditioning is applied (wru's basic BISG mode). The
#' default population basis matches wru exactly (`"pop"`, all ages);
#' pass `"vap"` to use the electorate-focused basis the rest of
#' openBISG defaults to.
#'
#' @param data A data.frame with a surname column named `last` or
#'   `surname` (case-insensitive; `last` wins if both are present) and
#'   one geography column named `block`, `block_group`, or `tract`
#'   (most specific wins). Geography IDs may carry the Census Summary
#'   File prefixes; they are normalized as in [geo_prior()].
#' @param geography_type `"pop"` (default) — total population of all
#'   ages, **wru's exact default basis** (P.L. 94-171 Table P2) —
#'   `"vap"` (2020 Decennial voting-age population, the basis the rest
#'   of openBISG defaults to), or `"cvap"` (citizen voting-age
#'   population; block-group / tract only).
#' @param include_extra When `TRUE`, surnames absent from the Census
#'   2020 list fall back to the Rosenman, Olivella, and Imai (2023)
#'   voter-file table (collapsed to the five categories), as in
#'   [predict_names()]. Default `FALSE` — the Census list only, as in
#'   wru.
#' @param geo_smooth Pseudo-count smoothing of the geography component
#'   toward the table-wide marginal, as in [predict_demog()]. Default
#'   `0` for wru fidelity (raw counts).
#' @param progress,n_cores Forwarded to [predict_demog()].
#' @return A data.frame with `nrow(data)` rows and columns `p_whi`,
#'   `p_bla`, `p_his`, `p_asi`, `p_oth`, summing to 1 per row when any
#'   evidence matched and `NA_real_` otherwise. Corresponds to wru's
#'   `pred.whi` ... `pred.oth`.
#' @seealso [predict_demog()] for the full six-category openBISG
#'   estimator, [geo_prior()] for the bundled geography tables.
#' @references Imai, K. and Khanna, K. (2016). Improving Ecological
#'   Inference by Predicting Individual Ethnicity from Voter
#'   Registration Records. Political Analysis 24(2), 263–272.
#' @examples
#' df <- data.frame(last  = c("Garcia", "Smith"),
#'                  block = rep(geo_block_vap$geoid[1], 2))
#' predict_wru(df, progress = FALSE)
#' @export
predict_wru <- function(data,
                        geography_type = c("pop", "vap", "cvap"),
                        include_extra = FALSE,
                        geo_smooth = 0,
                        progress = TRUE,
                        n_cores = 1L) {
  geography_type <- match.arg(geography_type)
  geo_smooth <- check_geo_smooth(geo_smooth)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  lower <- tolower(names(data))
  last_col <- names(data)[match(c("last", "surname"), lower)]
  last_col <- last_col[!is.na(last_col)][1]
  if (is.na(last_col) || is.null(last_col)) {
    stop("`data` needs a surname column named `last` or `surname` ",
         "(case-insensitive).", call. = FALSE)
  }
  geo_levels <- c("block", "block_group", "tract")   # most specific first
  geo_col <- names(data)[match(geo_levels, lower)]
  level <- geo_levels[!is.na(geo_col)][1]
  geo_col <- geo_col[!is.na(geo_col)][1]
  if (is.na(level) || is.null(level)) {
    stop("predict_wru() replicates wru's Census-geography BISG and ",
         "needs a `block`, `block_group`, or `tract` column. For ZIP ",
         "/ ZCTA geography use predict_demog().", call. = FALSE)
  }
  if (level == "block" && geography_type == "cvap") {
    stop("No block-level CVAP table exists (citizenship is not ",
         "collected in the decennial census). Use ",
         "`geography_type = \"vap\"` at the block level, or the ",
         "block_group / tract levels for CVAP.", call. = FALSE)
  }

  normalizer <- switch(level,
                       block       = normalize_block,
                       block_group = normalize_block_group,
                       tract       = normalize_tract)
  gv <- data[[geo_col]]
  if (is.factor(gv)) gv <- as.character(gv)
  gv <- as.character(gv)
  keys <- rep(NA_character_, length(gv))
  has_val <- !is.na(gv) & nzchar(gv)
  keys[has_val] <- vapply(gv[has_val], normalizer, character(1),
                          USE.NAMES = FALSE)

  nd <- wru_name_dict(include_extra)
  gt <- wru_geo_dict(level, geography_type)

  ## Report geography values that will not fold (wru mode: no fallback).
  n_geo <- sum(has_val)
  n_miss <- sum(has_val & !(keys %in% gt$dict$geoid))
  if (n_miss > 0L) {
    message(sprintf(
      paste0("predict_wru(): %s of %s `%s` row(s) did not match the ",
             "bundled %s table; those rows carry the surname-only ",
             "posterior (wru mode has no block-group fallback)."),
      format(n_miss, big.mark = ","), format(n_geo, big.mark = ","),
      level, level))
  }

  ## One predict_demog() batch per state, each with that state's
  ## composition as the marginal P(R). Rows without a usable geography
  ## ID ride along with the largest batch (the marginal only matters
  ## for rows where evidence is combined).
  state <- substr(keys, 1L, 2L)
  known <- !is.na(state) & state %in% rownames(gt$state_counts)
  batch_of <- rep(NA_character_, nrow(data))
  batch_of[known] <- state[known]
  if (any(!known)) {
    host <- if (any(known)) names(which.max(table(batch_of[known])))
            else rownames(gt$state_counts)[1]
    batch_of[!known] <- host
  }

  out <- matrix(NA_real_, nrow(data), length(WRU_GROUPS))
  for (s in unique(batch_of)) {
    idx <- which(batch_of == s)
    cnts <- gt$state_counts[s, ]
    prior_s <- stats::setNames(as.numeric(cnts) / sum(cnts), WRU_GROUPS)
    df_s <- data.frame(last = data[[last_col]][idx],
                       geoid = keys[idx],
                       stringsAsFactors = FALSE)
    gd_s <- gt$dict[substr(gt$dict$geoid, 1L, 2L) == s, , drop = FALSE]
    res <- predict_demog(df_s,
                         name_dict = list(last = nd),
                         geo_dict = gd_s,
                         prior = prior_s,
                         geo_smooth = geo_smooth,
                         progress = progress,
                         n_cores = n_cores)
    out[idx, ] <- as.matrix(res[, paste0("p_", WRU_GROUPS)])
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- paste0("p_", WRU_GROUPS)
  out
}
