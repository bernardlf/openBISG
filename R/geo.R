## Geography-aware priors for race / Hispanic-origin prediction.
##
## The package ships six lazy-loaded data frames built from the
## 2020-2024 ACS Citizen Voting Age Population (CVAP) Special Tabulation
## and the 2020 Decennial Demographic and Housing Characteristics File
## (DHC) Table P11 (VAP):
##
##   geo_zcta_cvap   geo_zcta_vap
##   geo_tract_cvap  geo_tract_vap
##   geo_bg_cvap     geo_bg_vap
##
## Each has columns geoid, total, white, black, aian, aapi, nh_multi,
## hispanic — proportions per row sum to 1 (NA when total = 0).
##
## See data-raw/build_geo.R for derivation.

geo_levels <- function() c("zcta", "tract", "block_group")
geo_types  <- function() c("cvap", "vap")

geo_table <- function(level, type) {
  level <- match.arg(level, geo_levels())
  type  <- match.arg(type,  geo_types())
  obj <- switch(paste(level, type, sep = "_"),
                zcta_cvap         = "geo_zcta_cvap",
                zcta_vap          = "geo_zcta_vap",
                tract_cvap        = "geo_tract_cvap",
                tract_vap         = "geo_tract_vap",
                block_group_cvap  = "geo_bg_cvap",
                block_group_vap   = "geo_bg_vap")
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

## Population-weighted marginal composition of a geography table --
## the shrinkage target used by `smooth_geo_probs()`. Rows with a
## missing / non-positive total or missing shares are skipped. Returns
## NULL when no row qualifies.
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
## the 240k-row block-group tables are only swept once per session.
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

#' Geography-level race / Hispanic-origin prior
#'
#' Look up the share of each race / Hispanic-origin group for the citizen
#' voting age population (CVAP, 2020-2024 ACS Special Tabulation) or the
#' voting age population (VAP, 2020 Decennial DHC Table P11) at a ZIP /
#' ZCTA, Census Tract, or Census Block Group.
#'
#' Exactly one of `zcta`, `tract`, or `block_group` must be supplied. The
#' returned vector is the geographic prior `P(R | G)` used by
#' [predict_race()] and [predict_names()] when geography is provided. If
#' none of the three is supplied, returns the population-level prior
#' attached to [last_names] (the same prior that drives the Naive-Bayes
#' name combination).
#'
#' Sub-five-digit ZIPs are zero-padded to five digits ("601" -> "00601").
#' Tract GEOIDs may be supplied either as 11-digit FIPS strings
#' ("01001020100") or as the Census Summary File prefixed form
#' ("1400000US01001020100"). Block-group GEOIDs are 12 digits, optionally
#' with the "1500000US" prefix.
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
#' @param type `"cvap"` (default) or `"vap"`. Picks which population the
#'   prior is computed over. CVAP excludes non-citizens; VAP is everyone
#'   age 18+. CVAP is appropriate for predictions about likely voters
#'   (e.g. matching against a voter file). VAP is appropriate when the
#'   bearer's citizenship status is unknown.
#' @param geo_smooth Pseudo-count, in people, used to shrink the
#'   looked-up composition toward the national marginal of the same
#'   table — see **Zero cells and smoothing**. Default `1`. Set to `0`
#'   to return the published shares unchanged.
#' @return A length-6 named numeric vector of proportions in
#'   [race_groups()] order, summing to 1. Has the attribute `total`
#'   (the CVAP or VAP count for that geography), `level` (`"zcta"` /
#'   `"tract"` / `"block_group"` / `"national"`), `type`
#'   (`"cvap"` / `"vap"`), and `geo_smooth` (the pseudo-count applied).
#'   Returns `NULL` if the geography ID isn't in the bundled table.
#' @seealso [predict_race()], [last_names].
#' @examples
#' geo_prior(zcta = "00601")              # ZCTA in Puerto Rico
#' geo_prior(tract = "01001020100")       # tract in Autauga County, AL
#' geo_prior(zcta = 30307, type = "vap")  # Atlanta-area ZIP, VAP basis
#'
#' ## Half of `geo_bg_cvap` estimates zero Asian / NHPI citizens age
#' ## 18+. Smoothing replaces the fatal exact zero with a small share.
#' bg <- geo_bg_cvap$geoid[geo_bg_cvap$aapi == 0][1]
#' geo_prior(block_group = bg, geo_smooth = 0)[["aapi"]]
#' geo_prior(block_group = bg)[["aapi"]]
#' @export
geo_prior <- function(zcta = NULL, tract = NULL, block_group = NULL,
                      type = c("cvap", "vap"), geo_smooth = 1) {
  type <- match.arg(type)
  geo_smooth <- check_geo_smooth(geo_smooth)
  supplied <- !vapply(list(zcta, tract, block_group), is.null, logical(1))
  if (sum(supplied) > 1L) {
    stop("Provide at most one of `zcta`, `tract`, or `block_group`.",
         call. = FALSE)
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
  } else {
    level <- "block_group"
    key   <- normalize_block_group(block_group)
  }
  if (is.na(key)) return(NULL)

  tbl <- geo_table(level, type)
  hit <- tbl[tbl$geoid == key, , drop = FALSE]
  if (nrow(hit) == 0L) return(NULL)
  out <- unlist(hit[1, race_groups()])
  out <- stats::setNames(as.numeric(out), race_groups())
  if (any(is.na(out)) || sum(out, na.rm = TRUE) == 0) return(NULL)
  total <- as.integer(hit$total[1])
  if (geo_smooth > 0) {
    out <- smooth_geo_probs(out, total, geo_smooth, geo_national(level, type))
  }
  attr(out, "total")      <- total
  attr(out, "level")      <- level
  attr(out, "type")       <- type
  attr(out, "geo_smooth") <- geo_smooth
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
