#' Race / Hispanic-origin group keys and human-readable labels
#'
#' The six 2020 Census race / Hispanic-origin categories used by the
#' lookup tables, in the order used throughout the package.
#'
#' @return `race_groups()` is a length-6 character vector of short keys;
#'   `race_group_labels()` is the same length and named by those keys, with
#'   the full Census labels as values.
#' @examples
#' race_groups()
#' race_group_labels()
#' @export
race_groups <- function() {
  c("white", "black", "aian", "aapi", "nh_multi", "hispanic")
}

#' @rdname race_groups
#' @export
race_group_labels <- function() {
  c(
    white    = "Non-Hispanic White alone",
    black    = "Non-Hispanic Black or African American alone",
    aian     = "Non-Hispanic American Indian / Alaska Native alone",
    aapi     = "Non-Hispanic Asian / Native Hawaiian / Pacific Islander alone",
    nh_multi = "Non-Hispanic two or more races",
    hispanic = "Hispanic or Latino origin"
  )
}

#' Sex group keys and labels
#'
#' @return `sex_groups()` returns `c("male", "female")`;
#'   `sex_group_labels()` returns `c(male = "Male", female = "Female")`.
#' @examples
#' sex_groups()
#' sex_group_labels()
#' @export
sex_groups <- function() c("male", "female")

#' @rdname sex_groups
#' @export
sex_group_labels <- function() c(male = "Male", female = "Female")

## Combine N >= 1 same-shape posterior vectors via the Naive-Bayes formula
##
##   P(g | n1, ..., nk) ∝ Π P(g | n_i) / P(g)^(k - 1)
##
## With k = 1 this is just P(g | n1) (no division). With k = 0 returns NULL
## so callers can decide what to do.
combine_bayes_n <- function(prob_list, prior) {
  k <- length(prob_list)
  if (k == 0L) return(NULL)
  prod_probs <- Reduce(`*`, prob_list)
  if (k == 1L) {
    res <- prod_probs
  } else {
    res <- ifelse(prior == 0, 0, prod_probs / (prior^(k - 1L)))
  }
  z <- sum(res, na.rm = TRUE)
  if (!is.finite(z) || z == 0) {
    return(stats::setNames(rep(NA_real_, length(prod_probs)), names(prod_probs)))
  }
  res / z
}

## Look up surname-style tokens (multi-token, no compound retry).
lookup_surname_tokens <- function(tokens, tables, include_extra = FALSE) {
  if (length(tokens) == 0L) return(list())
  hits <- lapply(tokens, function(t) lookup_with_fallback(
    t, tables = tables, include_extra = include_extra
  ))
  stats::setNames(hits, tokens)
}

#' Predict race / Hispanic-origin (and sex) probabilities from one or
#' more given names and surnames
#'
#' Each input field is tokenized on whitespace (a length-one string is
#' split; a character vector is treated as already-tokenized).
#'
#' **Given-name fields (`first`, `middle`)** use a "compound first,
#' tokens otherwise" cascade implemented by [lookup_compound_or_tokens()]:
#' the entire field is first looked up as a single name (the cascade in
#' [lookup_name()] strips internal spaces, so `"MARY ANN"` → `MARYANN`,
#' `"MARIA JOSE"` → `MARIAJOSE`); only if that misses does it fall back
#' to per-token lookup. This preserves the way the Census tabulated
#' compound first names — which carry their own (often very different)
#' race and sex distributions from the parts.
#'
#' **Surname fields (`last`, `maiden`)** are looked up per-token (no
#' compound retry). When `maiden` is non-empty its tokens **replace**
#' the `last` tokens in the combined estimate; the `last` lookups are
#' still reported under `$tokens$last` but `$surname_used` is set to
#' `"maiden"`.
#'
#' Each given-name token / lookup falls back to the last-name table on
#' miss; each surname token falls back to the first-name table. Tokens
#' absent from both tables are reported but excluded from the combined
#' estimate.
#'
#' Combination across the `k` matched evidence pieces uses Naive Bayes:
#'
#'   `P(R | n1, ..., nk) ∝ Π P(R | n_i) / P(R)^(k - 1)`
#'
#' under the assumption that all `k` name tokens are conditionally
#' independent given race. With `k = 1` (e.g. a single first name, or a
#' compound that matched as one token) the formula reduces to
#' `P(R | n1)` — the per-name proportion read directly from the
#' dictionary.
#'
#' Sex is predicted from the **first-name field only** (middle names
#' are excluded, see [predict_sex()]), using the same compound-first
#' cascade against the sex table — so
#' `predict_race(first = "Maria Jose")` reads the sex probability
#' directly from the `MARIAJOSE` row instead of (mis-) combining the
#' male `JOSE` row with the female `MARIA` row.
#'
#' @param first Given names (whitespace-separated string or vector).
#' @param middle Middle names. Looked up like `first`.
#' @param last Surnames. Tokenized; per-token cross-table fallback.
#' @param maiden Maiden name. Same handling as `last`. When provided
#'   (non-empty), takes the place of `last` in the combined estimate.
#' @param include_extra If `TRUE`, names that miss every Census 2020
#'   table fall back to the Rosenman, Olivella, and Imai (2023)
#'   NotInCensus2020 voter-file tables (`first_names_extra` /
#'   `last_names_extra`) before being recorded as a miss. The Rosenman
#'   tables only contain names absent from Census 2020, so a Census hit
#'   always takes precedence. The Rosenman source ships only 5 race
#'   columns (white, black, AAPI, OTHER, Hispanic, where OTHER lumps
#'   AIAN, two-or-more, and other); for combination with the rest of
#'   the package, OTHER is split between AIAN and `nh_multi`
#'   proportionally to the Census prior at build time. Default `FALSE`.
#'   Sex prediction is unaffected — the Rosenman dataset has no sex
#'   information, so the `first_names_sex` table is the only source.
#' @param zcta,tract,block_group,block Optional geography identifier. At
#'   most one may be supplied. When given, the name posterior is folded
#'   together with the geographic prior under conditional independence
#'   given race (BISG / BIFSG):
#'   \deqn{P(R | \mathrm{name}, G) \propto P(R | \mathrm{name})\,P(R | G)\,/\,P(R)}.
#'   See [geo_prior()] for accepted formats and the bundled tables.
#'   `block` takes a 15-digit 2020 Census Block GEOID; block-level data
#'   are VAP only, and a block that is not among the populated 2020
#'   blocks falls back to its parent block group (see [geo_prior()],
#'   whose messages this function inherits).
#' @param geography_type `"cvap"` (default), `"vap"`, or `"pop"` —
#'   picks which bundled geography table feeds the prior when `zcta` /
#'   `tract` / `block_group` / `block` is supplied. There is no
#'   block-level CVAP table, so `block` with `"cvap"` uses the block's
#'   parent block group. `"pop"` is the total population of all ages
#'   (P.L. 94-171 Table P2, the basis used by the `wru` package);
#'   it covers tract / block group / block only — see [geo_prior()].
#' @param block_fallback Forwarded to [geo_prior()]: when `TRUE`
#'   (default), a `block` GEOID not among the populated 2020 blocks
#'   falls back to its parent block group's row; when `FALSE` the
#'   geography lookup is recorded as not found. Only consulted for
#'   `block` lookups.
#' @param block_shrink Forwarded to [geo_prior()]: pseudo-count, in
#'   people, of the parent block group's composition blended into a
#'   block's VAP counts before normalizing (default `10`; `0` restores
#'   the raw 0.7.0 block counts). Only consulted for `block` lookups.
#' @param geo_smooth Pseudo-count, in people, used to shrink the bundled
#'   geographic prior toward the national marginal of the same table
#'   before it is folded in (default `1`). This keeps an exact zero in
#'   `P(R | G)` — common at block-group scale, where 49% of CVAP rows
#'   estimate no Asian / NHPI citizens — from forcing that group's
#'   posterior to zero regardless of the name evidence. See
#'   [geo_prior()] for the formula. Set to `0` to fold in the published
#'   shares unchanged. Ignored when `geography_probs` is supplied: a
#'   caller-supplied prior is used exactly as given.
#' @param geography_probs Optional length-6 named numeric vector of
#'   `P(R | G)` (in [race_groups()] order). Lets callers plug in a
#'   prior from any external source without going through
#'   [geo_prior()] — e.g. a tract-level prior built from a different
#'   ACS vintage, or a tabulated school-attendance-area prior.
#'   Mutually exclusive with `zcta` / `tract` / `block_group`.
#' @return A list with:
#'   \describe{
#'     \item{groups, group_labels}{Race-group keys and labels.}
#'     \item{tokens}{`list(first, middle, last, maiden)`. Each is a
#'       named list (names = the lookup keys, which are the joined
#'       compound when matched as one token, or the individual tokens
#'       otherwise) of hits or `NULL`.}
#'     \item{surname_used}{`"maiden"` if any maiden token was supplied,
#'       `"last"` if last tokens were supplied without maiden, or
#'       `NULL` if neither.}
#'     \item{combined}{`list(probs = ..., n = k)` — or `NULL` if no
#'       evidence matched.}
#'     \item{geography}{`NULL` when no geography was supplied,
#'       otherwise a list describing the lookup: `level`, `type`, `key`,
#'       `total`, `source`, `found`, the `geo_smooth` pseudo-count that
#'       was applied, the (smoothed) `probs` fed to the fold, and
#'       `combined` — the BISG posterior `P(R | name, G)`. When a
#'       `block` lookup was rerouted to its parent block group, `level`
#'       is `"block_group"` and `fallback_from` is `"block"`.}
#'     \item{sex}{`list(probs = ..., tokens, n)` — or `NULL` if no
#'       first-name input was provided.}
#'   }
#' @seealso [lookup_name()], [lookup_with_fallback()],
#'   [lookup_compound_or_tokens()], [tokenize_names()], [predict_sex()].
#' @examples
#' ## Single name + surname.
#' predict_race(first = "Maria", last = "Garcia")
#'
#' ## Compound first reads MARIAJOSE directly.
#' predict_race(first = "Maria Jose", last = "Garcia")
#'
#' ## Maiden replaces last in the combined estimate.
#' predict_race(first = "Maria", last = "Smith", maiden = "Garcia")
#'
#' ## BISG: fold a ZIP-level prior into the name posterior.
#' predict_race(first = "Maria", last = "Garcia", zcta = "30307")
#'
#' ## Geography only: collapses to P(R | G).
#' predict_race(zcta = "00601")
#' @export
predict_race <- function(first = NULL, middle = NULL,
                         last = NULL, maiden = NULL,
                         include_extra = FALSE,
                         zcta = NULL, tract = NULL, block_group = NULL,
                         block = NULL,
                         geography_type = c("cvap", "vap", "pop"),
                         geo_smooth = 1,
                         block_fallback = TRUE,
                         block_shrink = 10,
                         geography_probs = NULL) {
  geography_type <- match.arg(geography_type)
  geo_smooth <- check_geo_smooth(geo_smooth)
  block_shrink <- check_block_shrink(block_shrink)
  first_tokens  <- tokenize_names(first)
  middle_tokens <- tokenize_names(middle)
  last_tokens   <- tokenize_names(last)
  maiden_tokens <- tokenize_names(maiden)

  total_tokens <- length(first_tokens) + length(middle_tokens) +
    length(last_tokens) + length(maiden_tokens)
  geo_input_given <- !is.null(zcta) || !is.null(tract) ||
    !is.null(block_group) || !is.null(block) || !is.null(geography_probs)
  if (total_tokens == 0L && !geo_input_given) {
    stop("Provide at least one given-name or surname token in `first`, ",
         "`middle`, `last`, or `maiden`, or a geography (`zcta`, ",
         "`tract`, `block_group`, `block`, or `geography_probs`).",
         call. = FALSE)
  }

  ## Race lookups.
  first_hits  <- lookup_compound_or_tokens(first_tokens,
                                           tables = c("first", "last"),
                                           include_extra = include_extra)
  middle_hits <- lookup_compound_or_tokens(middle_tokens,
                                           tables = c("first", "last"),
                                           include_extra = include_extra)
  last_hits   <- lookup_surname_tokens(last_tokens,
                                       tables = c("last", "first"),
                                       include_extra = include_extra)
  maiden_hits <- lookup_surname_tokens(maiden_tokens,
                                       tables = c("last", "first"),
                                       include_extra = include_extra)

  ## Pick which surname source feeds the combination.
  if (length(maiden_tokens) > 0L) {
    surname_used <- "maiden"
    surname_hits <- maiden_hits
  } else if (length(last_tokens) > 0L) {
    surname_used <- "last"
    surname_hits <- last_hits
  } else {
    surname_used <- NULL
    surname_hits <- list()
  }

  matched <- c(
    Filter(Negate(is.null), first_hits),
    Filter(Negate(is.null), middle_hits),
    Filter(Negate(is.null), surname_hits)
  )

  out <- list(
    groups       = race_groups(),
    group_labels = race_group_labels(),
    tokens       = list(first  = first_hits,
                        middle = middle_hits,
                        last   = last_hits,
                        maiden = maiden_hits),
    surname_used = surname_used,
    combined     = NULL,
    geography    = NULL,
    sex          = NULL
  )

  prior <- attr(table_df("last"), "prior")
  if (length(matched) >= 1L) {
    prob_list <- lapply(matched, `[[`, "probs")
    out$combined <- list(
      probs = combine_bayes_n(prob_list, prior),
      n     = length(matched)
    )
  }

  ## Geography prior. At most one of zcta / tract / block_group / block /
  ## geography_probs.
  geo_supplied <- !vapply(
    list(zcta, tract, block_group, block, geography_probs),
    is.null, logical(1)
  )
  if (sum(geo_supplied) > 1L) {
    stop("Provide at most one of `zcta`, `tract`, `block_group`, ",
         "`block`, or `geography_probs`.", call. = FALSE)
  }
  geo_probs <- NULL
  geo_meta  <- list(level = NULL, type = geography_type, total = NA_integer_,
                    key = NULL, source = NULL, found = FALSE,
                    geo_smooth = geo_smooth)
  if (!is.null(geography_probs)) {
    g <- geography_probs[race_groups()]
    if (any(is.na(g)) || sum(g, na.rm = TRUE) <= 0) {
      stop("`geography_probs` must be a length-6 named numeric vector ",
           "(names = race_groups()) summing to a positive value.",
           call. = FALSE)
    }
    geo_probs <- g / sum(g)
    geo_meta$source     <- "user"
    geo_meta$level      <- "user"
    geo_meta$found      <- TRUE
    geo_meta$geo_smooth <- 0      # caller-supplied priors are used as given
  } else if (any(geo_supplied[1:4])) {
    p <- geo_prior(zcta = zcta, tract = tract, block_group = block_group,
                   block = block,
                   type = geography_type, geo_smooth = geo_smooth,
                   block_fallback = block_fallback,
                   block_shrink = block_shrink)
    geo_meta$key <- if (!is.null(zcta)) as.character(zcta)
                    else if (!is.null(tract)) as.character(tract)
                    else if (!is.null(block_group)) as.character(block_group)
                    else as.character(block)
    geo_meta$source <- "bundled"
    if (!is.null(p)) {
      geo_probs       <- p
      geo_meta$level  <- attr(p, "level")
      geo_meta$total  <- attr(p, "total")
      geo_meta$found  <- TRUE
      geo_meta$fallback_from <- attr(p, "fallback_from")
      geo_meta$block_shrink  <- attr(p, "block_shrink")
    } else {
      geo_meta$level  <- if (!is.null(zcta)) "zcta"
                         else if (!is.null(tract)) "tract"
                         else if (!is.null(block_group)) "block_group"
                         else "block"
      geo_meta$found  <- FALSE
    }
  }

  if (!is.null(geo_probs)) {
    name_probs <- if (!is.null(out$combined)) out$combined$probs else prior
    geo_meta$probs    <- geo_probs
    geo_meta$combined <- combine_name_geo(name_probs, geo_probs, prior)
    out$geography     <- geo_meta
  } else if (geo_meta$found || !is.null(geo_meta$key) || !is.null(geo_meta$source)) {
    ## record the unmatched lookup so callers can see what happened
    out$geography <- geo_meta
  }

  ## Sex: compound-first cascade against the sex table, using ONLY the
  ## first-name field. Middle names are intentionally excluded — they
  ## carry too much cross-sex noise (e.g. a religious or family middle
  ## name often doesn't match the bearer's sex), so combining them into
  ## P(sex) tends to mislead. Surnames have no sex info in this dataset.
  ## `include_extra` is ignored here: the Rosenman, Olivella, and Imai (2023)
  ## dataset publishes only race probabilities, no sex, so there is no
  ## extra sex table to consult.
  if (length(first_tokens) > 0L) {
    first_sex_hits <- lookup_compound_or_tokens(first_tokens,
                                                tables = "first_sex")
    sex_matched <- Filter(Negate(is.null), first_sex_hits)
    sex_tokens <- list(first = first_sex_hits)
    if (length(sex_matched) >= 1L) {
      sex_prior <- attr(table_df("first_sex"), "prior")
      sex_prob_list <- lapply(sex_matched, `[[`, "probs")
      out$sex <- list(
        probs  = combine_bayes_n(sex_prob_list, sex_prior),
        tokens = sex_tokens,
        n      = length(sex_matched)
      )
    } else {
      out$sex <- list(probs = NULL, tokens = sex_tokens, n = 0L)
    }
  }

  out
}

#' Predict sex probabilities from a first name
#'
#' Wrapper that returns just the `sex` component of [predict_race()].
#' Only the `first` field feeds the sex estimate — middle names are
#' deliberately excluded because cross-sex middle names are common
#' enough (e.g. religious or family-traditional middle names) that
#' folding them into P(sex) tends to mislead.
#'
#' @param first Given name(s) entered as the first name (string or
#'   vector). The compound-first cascade applies, so a string like
#'   `"Maria Jose"` matches the `MARIAJOSE` row directly.
#' @return The `sex` element described in [predict_race()], or `NULL`
#'   if no first-name input was provided.
#' @examples
#' predict_sex("Michael")
#' predict_sex("Maria")
#' predict_sex("Maria Jose")  # P(female) ~ 0.996 from MARIAJOSE row
#' @export
predict_sex <- function(first = NULL) {
  if (length(tokenize_names(first)) == 0L) return(NULL)
  predict_race(first = first)$sex
}

#' Vectorized BISG / BIFSG prediction over a data frame
#'
#' Auto-detects which input columns are present in `data` and returns a
#' fixed-shape data frame of per-row probabilities. Recognized columns
#' (any subset, named exactly):
#' \itemize{
#'   \item name fields: `first`, `middle`, `last`, `maiden`
#'   \item geography fields: `zcta`, `tract`, `block_group`, `block`
#' }
#' If multiple geography columns are present the most specific one is
#' used (`block` > `block_group` > `tract` > `zcta`). If no recognized
#' columns are present, an error is raised.
#'
#' With a `block` column, per-row [geo_prior()] messages are suppressed
#' and summarized once per call instead: rows whose block GEOID is not
#' among the populated 2020 blocks fall back to their parent block
#' group (see `block_fallback`), and with `geography_type = "cvap"`
#' every block row uses its parent block group's CVAP proportions
#' (there is no block-level CVAP table).
#'
#' Per-row behavior matches [predict_race()] exactly: compound-first
#' cascade for given-name fields; per-token cascade for surname fields;
#' maiden tokens replace last in the combined estimate; sex computed
#' from the `first` field only. Race probabilities hold the
#' BISG-combined posterior `P(R | name, G)` when a geography column is
#' present and matched, falling back to the name-only posterior
#' otherwise. Rows where nothing matched contain `NA_real_`.
#'
#' For a single per-call prediction or to inspect the per-token / sex
#' detail, use [predict_race()] and [predict_sex()].
#'
#' @param data A data frame with any subset of the recognized columns
#'   listed above.
#' @param include_extra Forwarded to [predict_race()] — when `TRUE`,
#'   names absent from Census 2020 are looked up against the
#'   Rosenman, Olivella, and Imai (2023) NotInCensus2020 voter-file
#'   tables. Default `FALSE`.
#' @param geography_type `"cvap"` (default), `"vap"`, or `"pop"` —
#'   selects the bundled geography table used when a geography column
#'   is detected. `"pop"` is the total population of all ages, wru's
#'   basis — see [geo_prior()].
#' @param geo_smooth Forwarded to [predict_race()] — the pseudo-count
#'   used to shrink the geographic prior toward the national marginal
#'   before folding, which keeps sampling zeros in `P(R | G)` from
#'   zeroing out a group the names point to. Default `1`; `0` disables.
#' @param block_shrink Forwarded to [predict_race()]: pseudo-count, in
#'   people, of the parent block group's composition blended into each
#'   block's VAP counts before normalizing (default `10`; `0` restores
#'   the raw 0.7.0 block counts). Only consulted for a `block` column.
#' @param block_fallback Forwarded to [predict_race()]: when `TRUE`
#'   (default), rows whose `block` GEOID is not among the populated
#'   2020 blocks fall back to their parent block group's proportions;
#'   when `FALSE` those rows get no geography component. A summary
#'   message reports how many rows were affected either way. Only
#'   consulted when a `block` column is used.
#' @param progress If `TRUE` (default), prints a one-line text progress
#'   bar to `stderr` showing percent complete, elapsed time, and an
#'   estimated time remaining. Pass `FALSE` to suppress (e.g. inside
#'   non-interactive scripts or when capturing output). The bar is only
#'   drawn for the serial path (`n_cores == 1L`); in the parallel path
#'   it is replaced by a single start / finish status line.
#' @param n_cores Number of worker processes to use. Default `1L`
#'   (serial). When `n_cores > 1L`, rows are split into `n_cores`
#'   chunks and processed via `parallel::mclapply` (fork-based, so
#'   the large bundled lookup tables are shared via copy-on-write —
#'   no per-worker export cost). On Windows fork is unavailable, so
#'   values above 1 are ignored and rows are processed serially.
#'   Capped at `min(n_cores, parallel::detectCores(), nrow(data))`.
#' @return A data frame with `nrow(data)` rows and 7 columns:
#'   `p_white`, `p_black`, `p_aian`, `p_aapi`, `p_nh_multi`,
#'   `p_hispanic` (race probabilities, summing to 1 per row when
#'   matched), and `p_female` (`P(female | first)`; `1 - p_female`
#'   gives `P(male)`). All cells are `NA_real_` for rows with no signal.
#' @seealso [predict_race()], [predict_sex()], [geo_prior()].
#' @examples
#' df <- data.frame(
#'   first = c("Maria",  "John",  "Mary Ann"),
#'   last  = c("Garcia", "Smith", "Johnson"),
#'   zcta  = c("30307",  "10001", "94110"),
#'   stringsAsFactors = FALSE
#' )
#' predict_names(df)
#'
#' \dontrun{
#' ## Parallelize a large data frame across 8 cores (Unix / macOS).
#' predict_names(big_df, n_cores = 8L)
#' }
#' @export
predict_names <- function(data,
                          include_extra = FALSE,
                          geography_type = c("cvap", "vap", "pop"),
                          geo_smooth = 1,
                          block_fallback = TRUE,
                          block_shrink = 10,
                          progress = TRUE,
                          n_cores = 1L) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  geography_type <- match.arg(geography_type)
  geo_smooth <- check_geo_smooth(geo_smooth)
  block_shrink <- check_block_shrink(block_shrink)

  recognized_names <- c("first", "middle", "last", "maiden")
  recognized_geo   <- c("block", "block_group", "tract", "zcta")  # most specific first
  recognized_all   <- c(recognized_names, recognized_geo)

  ## Case-insensitive detection: match each recognized key against
  ## tolower(names(data)) and remember the actual (mixed-case) column
  ## name. On duplicate matches (e.g. both `First` and `FIRST`), the
  ## first occurrence wins.
  lower_names <- tolower(names(data))
  match_idx   <- match(recognized_all, lower_names)
  col_map     <- stats::setNames(names(data)[match_idx], recognized_all)

  name_cols   <- recognized_names[!is.na(col_map[recognized_names])]
  geo_present <- recognized_geo[!is.na(col_map[recognized_geo])]
  geo_col     <- if (length(geo_present)) geo_present[1] else NA_character_

  if (length(name_cols) == 0L && is.na(geo_col)) {
    stop("No recognized columns in `data`. Expected any of (case-insensitive): ",
         paste(recognized_all, collapse = ", "), ".",
         call. = FALSE)
  }

  n <- nrow(data)
  race_keys <- race_groups()
  race_out_cols <- paste0("p_", race_keys)
  out_cols <- c(race_out_cols, "p_female")

  cell <- function(col, i) {
    if (is.na(col) || !nzchar(col) || !col %in% names(data)) return(NULL)
    val <- data[[col]][i]
    if (is.factor(val)) val <- as.character(val)
    if (is.na(val) || !nzchar(as.character(val))) return(NULL)
    val
  }

  pick <- function(key) {
    if (key %in% name_cols || (!is.na(geo_col) && key == geo_col)) {
      unname(col_map[[key]])
    } else NA_character_
  }
  fc <- pick("first");  mc <- pick("middle")
  lc <- pick("last");   xc <- pick("maiden")
  zc <- pick("zcta");   tc <- pick("tract")
  bc <- pick("block_group")
  kc <- pick("block")
  block_active <- !is.na(kc)

  ## Process a single row into a length-10 numeric vector: race probs in
  ## race_keys order + p_female, then three block-lookup bookkeeping
  ## flags (had a block value; fell back to its block group; no usable
  ## geography found). Per-row geo_prior() messages are suppressed on
  ## the block path and summarized once after the loop.
  process_row <- function(i) {
    run <- function() predict_race(
      first          = cell(fc, i),
      middle         = cell(mc, i),
      last           = cell(lc, i),
      maiden         = cell(xc, i),
      include_extra  = include_extra,
      zcta           = cell(zc, i),
      tract          = cell(tc, i),
      block_group    = cell(bc, i),
      block          = cell(kc, i),
      geography_type = geography_type,
      geo_smooth     = geo_smooth,
      block_fallback = block_fallback,
      block_shrink   = block_shrink
    )
    pred <- tryCatch(
      if (block_active) suppressMessages(run()) else run(),
      error = function(e) NULL
    )
    row <- c(rep(NA_real_, 7L), 0, 0, 0)
    if (block_active && !is.null(cell(kc, i))) {
      row[8] <- 1
      g <- if (!is.null(pred)) pred$geography
      if (!is.null(g)) {
        if (isTRUE(g$found) && identical(g$fallback_from, "block")) {
          row[9] <- 1
        }
        if (!isTRUE(g$found)) row[10] <- 1
      }
    }
    if (is.null(pred)) return(row)
    race_probs <- NULL
    if (!is.null(pred$geography) && !is.null(pred$geography$combined)) {
      race_probs <- pred$geography$combined
    } else if (!is.null(pred$combined)) {
      race_probs <- pred$combined$probs
    }
    if (!is.null(race_probs)) row[1:6] <- race_probs[race_keys]
    if (!is.null(pred$sex) && !is.null(pred$sex$probs)) {
      row[7] <- pred$sex$probs[["female"]]
    }
    row
  }

  ## Process a chunk of rows into an (length(idx) x 10) numeric matrix.
  process_chunk <- function(idx) {
    m <- matrix(NA_real_, nrow = length(idx), ncol = 10L)
    for (j in seq_along(idx)) m[j, ] <- process_row(idx[j])
    m
  }

  n_cores <- resolve_n_cores(n_cores, n)
  use_parallel <- n_cores > 1L && n > 1L

  show_progress <- isTRUE(progress) && n > 0L && !use_parallel
  if (show_progress) {
    pb <- make_progress_bar(n)
    pb$start()
  }

  out_mat <- matrix(NA_real_, nrow = n, ncol = 10L)

  if (use_parallel) {
    if (isTRUE(progress)) {
      cat(sprintf("predict_names: processing %d rows on %d cores...\n",
                  n, n_cores),
          file = stderr())
    }
    par_start <- Sys.time()
    chunks <- split(seq_len(n), cut(seq_len(n), n_cores, labels = FALSE))
    results <- parallel::mclapply(chunks, process_chunk, mc.cores = n_cores)
    out_mat <- do.call(rbind, results)
    if (isTRUE(progress)) {
      cat(sprintf("predict_names: done in %.1fs (%.0f rows/s).\n",
                  as.numeric(difftime(Sys.time(), par_start, units = "secs")),
                  n / max(1e-9,
                          as.numeric(difftime(Sys.time(), par_start,
                                              units = "secs")))),
          file = stderr())
    }
  } else {
    for (i in seq_len(n)) {
      out_mat[i, ] <- process_row(i)
      if (show_progress) pb$tick()
    }
    if (show_progress) pb$done()
  }

  ## ---- One summary message for the block-lookup bookkeeping ----
  if (block_active) {
    n_b  <- sum(out_mat[, 8], na.rm = TRUE)   # rows with a block value
    n_fb <- sum(out_mat[, 9], na.rm = TRUE)   # fell back to block group
    n_ng <- sum(out_mat[, 10], na.rm = TRUE)  # no usable geography found
    if (geography_type == "cvap" && n_b > 0) {
      message("predict_names(): no block-level CVAP table exists ",
              "(citizenship is not collected in the decennial census); ",
              "the ", format(n_b, big.mark = ","), " `block` row(s) used ",
              "their parent block group from `geo_bg_cvap` for the ",
              "geography component.")
    } else if (geography_type == "vap") {
      if (isTRUE(block_fallback) && n_fb > 0) {
        msg <- sprintf(
          paste0("predict_names(): %s of %s `block` row(s) were not ",
                 "matched to a populated 2020 census block; falling back ",
                 "to their block group's proportions for the geography ",
                 "component (disable with `block_fallback = FALSE`)."),
          format(n_fb, big.mark = ","), format(n_b, big.mark = ","))
        if (n_ng > 0) {
          msg <- paste0(msg, sprintf(
            " A further %s row(s) had no usable geography match at all.",
            format(n_ng, big.mark = ",")))
        }
        message(msg)
      } else if (!isTRUE(block_fallback) && n_ng > 0) {
        message(sprintf(
          paste0("predict_names(): %s of %s `block` row(s) were not ",
                 "matched to a populated 2020 census block; ",
                 "`block_fallback = FALSE`, so no geography component ",
                 "was applied to those rows."),
          format(n_ng, big.mark = ","), format(n_b, big.mark = ",")))
      }
    }
  }

  out <- out_mat[, 1:7, drop = FALSE]
  colnames(out) <- out_cols
  as.data.frame(out, stringsAsFactors = FALSE)
}
