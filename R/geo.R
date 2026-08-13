## Geography-aware priors for race / Hispanic-origin prediction.
##
## The package ships seven lazy-loaded data frames built from the
## 2020-2024 ACS Citizen Voting Age Population (CVAP) Special Tabulation,
## the 2020 Decennial Demographic and Housing Characteristics File
## (DHC) Table P11 (VAP), and the 2020 Decennial P.L. 94-171
## Redistricting Data (block-level VAP):
##
##   geo_zcta_cvap   geo_zcta_vap
##   geo_tract_cvap  geo_tract_vap
##   geo_bg_cvap     geo_bg_vap
##                   geo_block_vap
##
## The ZCTA / tract / block-group tables have columns geoid, total,
## white, black, aian, aapi, nh_multi, hispanic — proportions per row
## sum to 1 (NA when total = 0). geo_block_vap stores integer COUNTS
## instead of proportions (rows sum to `total` exactly) and only the
## 5.7M blocks with total > 0 — every consumer below row-normalizes
## before use, so the two schemas fold identically. There is no
## geo_block_cvap: citizenship is not collected in the decennial census
## and the CVAP Special Tabulation stops at block groups.
##
## See data-raw/build_geo.R and data-raw/build_geo_block.R for
## derivation.

geo_levels <- function() c("zcta", "tract", "block_group", "block")
geo_types  <- function() c("cvap", "vap")

geo_table <- function(level, type) {
  level <- match.arg(level, geo_levels())
  type  <- match.arg(type,  geo_types())
  if (level == "block" && type == "cvap") {
    stop("No block-level CVAP table exists: citizenship is not collected ",
         "in the decennial census and the CVAP Special Tabulation stops ",
         "at block groups. Use type = \"vap\" at block level, or look up ",
         "the block's parent block group.", call. = FALSE)
  }
  obj <- switch(paste(level, type, sep = "_"),
                zcta_cvap         = "geo_zcta_cvap",
                zcta_vap          = "geo_zcta_vap",
                tract_cvap        = "geo_tract_cvap",
                tract_vap         = "geo_tract_vap",
                block_group_cvap  = "geo_bg_cvap",
                block_group_vap   = "geo_bg_vap",
                block_vap         = "geo_block_vap")
  get(obj, envir = asNamespace("openBISG"))
}

## ---------------------------------------------------------------------
## Pseudo-count smoothing of a geographic prior.
##
## The BISG fold multiplies P(R | name) by P(R | G) / P(R), so a cell of
## P(R | G) that is exactly zero drives the posterior for that group to
## zero no matter how strong the name evidence is. In the bundled tables
## a zero cell is almost always a *sampling* zero rather than a
## structural one: the CVAP Special Tabulation is an ACS estimate, and at
## block-group scale 49% of rows estimate zero Asian / NHPI citizens and
## 86% estimate zero AIAN. Taking those at face value silently rewrites a
## confident name signal into whichever surviving group has the smallest
## marginal prior (usually `nh_multi`).
##
## `smooth_geo_probs()` shrinks the observed composition toward the
## national marginal of the same table with a pseudo-count of `alpha`
## people -- a Dirichlet(alpha * national) prior on the geography's
## composition, with the published shares as the data:
##
##   p_smooth = (total * p_geo + alpha * p_national) / (total + alpha)
##
## With the default alpha = 1 this shifts a well-populated cell by well
## under a tenth of a percentage point, but replaces an exact zero with a
## small positive share -- so geography still weighs heavily against the
## group, just not infinitely.
smooth_geo_probs <- function(p, total, alpha, target) {
  if (!is.finite(alpha) || alpha <= 0) return(p)
  s <- sum(p)
  if (!is.finite(s) || s <= 0) return(p)
  p <- p / s
  n <- if (length(total) == 1L && is.finite(total) && total > 0) {
    as.numeric(total)
  } else 0
  (n * p + alpha * target) / (n + alpha)
}

## Validate a `geo_smooth` argument. NULL means "off".
check_geo_smooth <- function(x) {
  if (is.null(x)) return(0)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`geo_smooth` must be a single non-negative, finite number ",
         "(0 disables smoothing).", call. = FALSE)
  }
  as.numeric(x)
}

## Validate a `block_shrink` argument. NULL means "off".
check_block_shrink <- function(x) {
  if (is.null(x)) return(0)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`block_shrink` must be a single non-negative, finite number ",
         "(0 disables the block-group blend).", call. = FALSE)
  }
  as.numeric(x)
}

## Population-weighted marginal composition of a geography table --
## the shrinkage target used by `smooth_geo_probs()`. Rows with a
## missing / non-positive total or missing shares are skipped. Returns
## NULL when no row qualifies. Row-normalizes before weighting, so it
## is exact for both the proportion tables and the count-valued
## geo_block_vap (counts / rowsum × total = counts).
geo_marginal <- function(tbl, groups, totals) {
  if (is.null(totals)) return(NULL)
  w  <- suppressWarnings(as.numeric(totals))
  pm <- as.matrix(tbl[, groups, drop = FALSE])
  storage.mode(pm) <- "double"
  rs <- rowSums(pm)
  ok <- is.finite(w) & w > 0 & is.finite(rs) & rs > 0
  if (!any(ok)) return(NULL)
  v <- colSums((pm[ok, , drop = FALSE] / rs[ok]) * w[ok])
  if (!is.finite(sum(v)) || sum(v) <= 0) return(NULL)
  stats::setNames(as.numeric(v / sum(v)), groups)
}

## National marginal for one bundled table, cached per (level, type) so
## the 240k-row block-group tables — and the 5.7M-row block table — are
## only swept once per session.
geo_national <- function(level, type) {
  key <- paste0("geo_national_", level, "_", type)
  cached <- get0(key, envir = .openBISG_caches, inherits = FALSE,
                 ifnotfound = NULL)
  if (!is.null(cached)) return(cached)
  tbl <- geo_table(level, type)
  out <- geo_marginal(tbl, race_groups(), tbl$total)
  if (is.null(out)) out <- attr(table_df("last"), "prior")[race_groups()]
  assign(key, out, envir = .openBISG_caches)
  out
}

normalize_zcta <- function(z) {
  z <- as.character(z)
  if (is.na(z) || !nzchar(z)) return(NA_character_)
  z <- gsub("[^0-9]", "", z)
  if (!nchar(z)) return(NA_character_)
  ## ZCTAs are 5-digit; pad short numeric ZIPs with leading zeros.
  formatC(as.integer(z), width = 5, format = "d", flag = "0")
}

normalize_tract <- function(t) {
  t <- as.character(t)
  if (is.na(t) || !nzchar(t)) return(NA_character_)
  ## Strip the "1400000US" Summary File prefix before dropping non-digits.
  t <- sub("^\\s*\\d{7}US", "", t)
  t <- gsub("[^0-9]", "", t)
  ## Census tract GEOIDs are 11 digits: state(2) + county(3) + tract(6).
  if (nchar(t) == 11L) return(t)
  NA_character_
}

normalize_block_group <- function(b) {
  b <- as.character(b)
  if (is.na(b) || !nzchar(b)) return(NA_character_)
  ## Strip the "1500000US" Summary File prefix before dropping non-digits.
  b <- sub("^\\s*\\d{7}US", "", b)
  b <- gsub("[^0-9]", "", b)
  ## Block group GEOIDs are 12 digits: state(2) + county(3) + tract(6) + bg(1).
  if (nchar(b) == 12L) return(b)
  NA_character_
}

normalize_block <- function(b) {
  b <- as.character(b)
  if (is.na(b) || !nzchar(b)) return(NA_character_)
  ## Strip the "7500000US" Summary File prefix before dropping non-digits.
  b <- sub("^\\s*\\d{7}US", "", b)
  b <- gsub("[^0-9]", "", b)
  ## Block GEOIDs are 15 digits: state(2) + county(3) + tract(6) + block(4).
  ## The block-group digit is the first block digit, so the parent block
  ## group is always substr(geoid, 1, 12).
  if (nchar(b) == 15L) return(b)
  NA_character_
}

#' Geography-level race / Hispanic-origin prior
#'
#' Look up the share of each race / Hispanic-origin group for the citizen
#' voting age population (CVAP, 2020-2024 ACS Special Tabulation) or the
#' voting age population (VAP, 2020 Decennial DHC Table P11 / P.L. 94-171
#' block counts) at a ZIP / ZCTA, Census Tract, Census Block Group, or
#' Census Block.
#'
#' Exactly one of `zcta`, `tract`, `block_group`, or `block` must be
#' supplied. The returned vector is the geographic prior `P(R | G)` used
#' by [predict_race()] and [predict_names()] when geography is provided.
#' If none of the four is supplied, returns the population-level prior
#' attached to [last_names] (the same prior that drives the Naive-Bayes
#' name combination).
#'
#' Sub-five-digit ZIPs are zero-padded to five digits ("601" -> "00601").
#' Tract GEOIDs may be supplied either as 11-digit FIPS strings
#' ("01001020100") or as the Census Summary File prefixed form
#' ("1400000US01001020100"). Block-group GEOIDs are 12 digits, optionally
#' with the "1500000US" prefix. Block GEOIDs are 15 digits, optionally
#' with the "7500000US" prefix.
#'
#' @section Block-level lookups:
#' The block table ([geo_block_vap]) is built from the 2020 Decennial
#' P.L. 94-171 Redistricting Data and covers the 5,704,969 blocks with
#' any voting-age population (50 states, DC, and Puerto Rico). Two
#' situations reroute a block lookup to the block's parent block group
#' (always `substr(geoid, 1, 12)`):
#' \itemize{
#'   \item `type = "cvap"` — no block-level CVAP table exists
#'     (citizenship is not collected in the decennial census), so the
#'     block-group CVAP row is used instead.
#'   \item The 15-digit GEOID is not among the populated 2020 blocks
#'     (a zero-VAP block, or an ID that does not exist). When
#'     `block_fallback = TRUE` (default) the block-group VAP row is
#'     used; when `FALSE` the lookup returns `NULL`.
#' }
#' Both reroutes emit a [message()] (suppressible with
#' [suppressMessages()]) and mark the result with attributes
#' `level = "block_group"` and `fallback_from = "block"`.
#'
#' @section Block-count shrinkage toward the block group:
#' A census block is small enough that its complete-count VAP row is
#' often degenerate for locally rare groups: a block of 40 adults with
#' zero recorded Hispanic residents pins the Hispanic share at (nearly)
#' zero even when the surrounding block group is 10% Hispanic and a
#' Hispanic family has since moved in. Validation against self-reported
#' race on the 2026 Georgia voter file shows this zero-own-count
#' situation accounts for essentially all of the block prior's
#' disadvantage versus the block-group prior among Hispanic and Asian
#' voters, while blocks with five or more own-group adults *beat* the
#' block group.
#'
#' `block_shrink` therefore blends the block's integer counts with
#' `block_shrink` pseudo-people drawn from the parent block group's
#' composition before normalizing — a Dirichlet prior with the block
#' group as the base measure:
#' \deqn{p_{\mathrm{blend}} = \frac{\mathrm{counts} +
#'   \lambda \, p_{\mathrm{bg}}}{\mathrm{total} + \lambda}.}
#' The default `lambda = 10` leaves well-populated blocks essentially
#' unchanged while pulling degenerate ones toward their block group;
#' `geo_smooth` is then applied on top with the blended scale
#' `total + lambda`. Set `block_shrink = 0` for the 0.7.0 behavior
#' (raw block counts). Only consulted for `block` lookups with
#' `type = "vap"`; the blend is skipped (silently) when the parent
#' block group has no usable row.
#'
#' @section Zero cells and smoothing:
#' The bundled tables contain a great many exact zeros at fine
#' geographies — in `geo_bg_cvap`, 49% of block groups report zero
#' Asian / NHPI citizens age 18+ and 86% report zero AIAN. These are
#' overwhelmingly *sampling* zeros: the CVAP Special Tabulation is an
#' ACS estimate, and a group with no one in the sample for a block
#' group is published as zero even where the true count is not.
#'
#' Passed through the BISG fold unchanged, a zero cell is fatal:
#' `P(R | name, G)` is proportional to `P(R | name) P(R | G) / P(R)`, so
#' a zero in `P(R | G)` forces that group's posterior to exactly zero
#' regardless of how decisive the name is, and the displaced mass lands
#' on whichever surviving group has the smallest marginal prior.
#'
#' `geo_smooth` therefore shrinks the looked-up composition toward the
#' population-weighted national marginal of the same table, with a
#' pseudo-count of `geo_smooth` people:
#' \deqn{p_{\mathrm{smooth}} = \frac{\mathrm{total} \times p_G +
#'   \alpha \, p_{\mathrm{national}}}{\mathrm{total} + \alpha}.}
#' At the default `geo_smooth = 1` — one pseudo-person drawn from the
#' national distribution — a populated cell moves by well under a tenth
#' of a percentage point, while a zero cell becomes small but positive.
#' Pass `geo_smooth = 0` for the raw published shares.
#'
#' @param zcta 5-digit ZCTA / ZIP (string or integer). Default `NULL`.
#' @param tract 11-digit Census Tract FIPS (string), or the
#'   Summary File "1400000US..." form. Default `NULL`.
#' @param block_group 12-digit Block Group FIPS (string), or the
#'   Summary File "1500000US..." form. Default `NULL`.
#' @param block 15-digit Census Block FIPS (string), or the Summary
#'   File "7500000US..." form. Default `NULL`. VAP only — see
#'   **Block-level lookups**.
#' @param type `"cvap"` (default) or `"vap"`. Picks which population the
#'   prior is computed over. CVAP excludes non-citizens; VAP is everyone
#'   age 18+. CVAP is appropriate for predictions about likely voters
#'   (e.g. matching against a voter file). VAP is appropriate when the
#'   bearer's citizenship status is unknown.
#' @param geo_smooth Pseudo-count, in people, used to shrink the
#'   looked-up composition toward the national marginal of the same
#'   table — see **Zero cells and smoothing**. Default `1`. Set to `0`
#'   to return the published shares unchanged.
#' @param block_fallback When `TRUE` (default), a `block` GEOID that is
#'   not among the populated 2020 blocks falls back to its parent block
#'   group's row (with a message). When `FALSE` such a lookup returns
#'   `NULL`. Only consulted for `block` lookups.
#' @param block_shrink Pseudo-count, in people, of the parent block
#'   group's composition blended into a block's VAP counts before
#'   normalizing — see **Block-count shrinkage toward the block
#'   group**. Default `10`. Set to `0` for the raw block counts
#'   (the 0.7.0 behavior). Only consulted for `block` lookups.
#' @return A length-6 named numeric vector of proportions in
#'   [race_groups()] order, summing to 1. Has the attribute `total`
#'   (the CVAP or VAP count for that geography), `level` (`"zcta"` /
#'   `"tract"` / `"block_group"` / `"block"` / `"national"` — the table
#'   actually used, so a rerouted block lookup reports
#'   `"block_group"`), `type` (`"cvap"` / `"vap"`), `geo_smooth` (the
#'   pseudo-count applied), and — only when a block lookup was rerouted
#'   to its block group — `fallback_from = "block"`. Block-level
#'   results also carry `block_shrink` — the pseudo-count actually
#'   blended in (`0` when disabled or when the parent block group had
#'   no usable row). Returns `NULL` if the geography ID isn't in the
#'   bundled table.
#' @seealso [predict_race()], [last_names].
#' @examples
#' geo_prior(zcta = "00601")              # ZCTA in Puerto Rico
#' geo_prior(tract = "01001020100")       # tract in Autauga County, AL
#' geo_prior(zcta = 30307, type = "vap")  # Atlanta-area ZIP, VAP basis
#' geo_prior(block = geo_block_vap$geoid[1], type = "vap")  # one block
#'
#' ## Half of `geo_bg_cvap` estimates zero Asian / NHPI citizens age
#' ## 18+. Smoothing replaces the fatal exact zero with a small share.
#' bg <- geo_bg_cvap$geoid[geo_bg_cvap$aapi == 0][1]
#' geo_prior(block_group = bg, geo_smooth = 0)[["aapi"]]
#' geo_prior(block_group = bg)[["aapi"]]
#' @export
geo_prior <- function(zcta = NULL, tract = NULL, block_group = NULL,
                      block = NULL,
                      type = c("cvap", "vap"), geo_smooth = 1,
                      block_fallback = TRUE, block_shrink = 10) {
  type <- match.arg(type)
  geo_smooth <- check_geo_smooth(geo_smooth)
  block_shrink <- check_block_shrink(block_shrink)
  supplied <- !vapply(list(zcta, tract, block_group, block),
                      is.null, logical(1))
  if (sum(supplied) > 1L) {
    stop("Provide at most one of `zcta`, `tract`, `block_group`, or ",
         "`block`.", call. = FALSE)
  }
  if (!any(supplied)) {
    prior <- attr(table_df("last"), "prior")
    out <- prior[race_groups()]
    attr(out, "total")      <- NA_integer_
    attr(out, "level")      <- "national"
    attr(out, "type")       <- type
    ## Already a national marginal — shrinking it toward one is a no-op.
    attr(out, "geo_smooth") <- 0
    return(out)
  }

  if (!is.null(zcta)) {
    level <- "zcta"
    key   <- normalize_zcta(zcta)
  } else if (!is.null(tract)) {
    level <- "tract"
    key   <- normalize_tract(tract)
  } else if (!is.null(block_group)) {
    level <- "block_group"
    key   <- normalize_block_group(block_group)
  } else {
    level <- "block"
    key   <- normalize_block(block)
  }
  if (is.na(key)) return(NULL)

  fallback_from <- NULL
  if (level == "block" && type == "cvap") {
    ## No block-level CVAP exists; use the parent block group's CVAP row.
    message("geo_prior(): no block-level CVAP table exists (citizenship ",
            "is not collected in the decennial census); using the ",
            "block's parent block group ", substr(key, 1L, 12L),
            " from `geo_bg_cvap` instead.")
    fallback_from <- "block"
    level <- "block_group"
    key   <- substr(key, 1L, 12L)
  }

  tbl <- geo_table(level, type)
  hit <- tbl[tbl$geoid == key, , drop = FALSE]
  if (nrow(hit) == 0L) {
    if (level != "block" || !isTRUE(block_fallback)) return(NULL)
    ## Valid 15-digit GEOID, but not a populated 2020 block: fall back
    ## to the parent block group's VAP proportions.
    message("geo_prior(): block ", key, " is not among the populated ",
            "2020 census blocks; falling back to its block group ",
            substr(key, 1L, 12L), " for the geography prior ",
            "(disable with `block_fallback = FALSE`).")
    fallback_from <- "block"
    level <- "block_group"
    key   <- substr(key, 1L, 12L)
    tbl <- geo_table(level, type)
    hit <- tbl[tbl$geoid == key, , drop = FALSE]
    if (nrow(hit) == 0L) return(NULL)
  }
  out <- unlist(hit[1, race_groups()])
  out <- stats::setNames(as.numeric(out), race_groups())
  if (any(is.na(out)) || sum(out, na.rm = TRUE) == 0) return(NULL)
  total <- as.integer(hit$total[1])
  n_eff <- as.numeric(total)
  shrink_applied <- 0
  if (level == "block") {
    ## geo_block_vap stores integer counts (rows sum to `total` exactly,
    ## and total > 0 for every shipped row). Blend `block_shrink`
    ## pseudo-people drawn from the parent block group's composition
    ## into the counts before normalizing -- see **Block-count
    ## shrinkage toward the block group**.
    if (block_shrink > 0) {
      bg_tbl <- geo_table("block_group", type)
      bg_hit <- bg_tbl[bg_tbl$geoid == substr(key, 1L, 12L), ,
                       drop = FALSE]
      if (nrow(bg_hit) == 1L) {
        bg_p <- suppressWarnings(
          as.numeric(unlist(bg_hit[1, race_groups()])))
        if (!any(is.na(bg_p)) && sum(bg_p) > 0) {
          out <- out + block_shrink * (bg_p / sum(bg_p))
          n_eff <- n_eff + block_shrink
          shrink_applied <- block_shrink
        }
      }
    }
    out <- out / sum(out)
  }
  if (geo_smooth > 0) {
    out <- smooth_geo_probs(out, n_eff, geo_smooth, geo_national(level, type))
  }
  attr(out, "total")      <- total
  attr(out, "level")      <- level
  attr(out, "type")       <- type
  attr(out, "geo_smooth") <- geo_smooth
  if (level == "block") attr(out, "block_shrink") <- shrink_applied
  if (!is.null(fallback_from)) attr(out, "fallback_from") <- fallback_from
  out
}

## Combine a name-conditional posterior with a geographic prior under
## conditional independence given race (BISG / BIFSG):
##
##   P(R | name, G) ∝ P(R | name) × P(R | G) / P(R)
##
## `name_probs`, `geo_probs`, and `prior` are length-6 named numeric
## vectors over [race_groups()] in the same order. Returns a length-6
## named vector summing to 1 (or `NULL` when the combination collapses
## to all zeros).
combine_name_geo <- function(name_probs, geo_probs, prior) {
  ## Avoid 0/0; treat zero priors as making that group impossible.
  num <- name_probs * geo_probs
  res <- ifelse(prior == 0, 0, num / prior)
  z <- sum(res, na.rm = TRUE)
  if (!is.finite(z) || z == 0) {
    return(stats::setNames(rep(NA_real_, length(name_probs)),
                           names(name_probs)))
  }
  res / z
}
