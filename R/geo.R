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
  t <- gsub("[^0-9]", "", t)
  ## Census tract GEOIDs are 11 digits: state(2) + county(3) + tract(6).
  if (nchar(t) == 11L) return(t)
  if (nchar(t) > 11L) {
    ## Strip the typical "1400000US" Summary File prefix.
    if (grepl("^\\d{7}US", as.character(t))) {
      return(sub("^\\d{7}US", "", as.character(t)))
    }
  }
  NA_character_
}

normalize_block_group <- function(b) {
  b <- as.character(b)
  if (is.na(b) || !nzchar(b)) return(NA_character_)
  b <- gsub("[^0-9]", "", b)
  ## Block group GEOIDs are 12 digits: state(2) + county(3) + tract(6) + bg(1).
  if (nchar(b) == 12L) return(b)
  if (nchar(b) > 12L && grepl("^\\d{7}US", as.character(b))) {
    return(sub("^\\d{7}US", "", as.character(b)))
  }
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
#' @return A length-6 named numeric vector of proportions in
#'   [race_groups()] order, summing to 1. Has the attribute `total`
#'   (the CVAP or VAP count for that geography), `level` (`"zcta"` /
#'   `"tract"` / `"block_group"` / `"national"`), and `type`
#'   (`"cvap"` / `"vap"`). Returns `NULL` if the geography ID isn't in
#'   the bundled table.
#' @seealso [predict_race()], [last_names].
#' @examples
#' geo_prior(zcta = "00601")              # ZCTA in Puerto Rico
#' geo_prior(tract = "01001020100")       # tract in Autauga County, AL
#' geo_prior(zcta = 30307, type = "vap")  # Atlanta-area ZIP, VAP basis
#' @export
geo_prior <- function(zcta = NULL, tract = NULL, block_group = NULL,
                      type = c("cvap", "vap")) {
  type <- match.arg(type)
  supplied <- !vapply(list(zcta, tract, block_group), is.null, logical(1))
  if (sum(supplied) > 1L) {
    stop("Provide at most one of `zcta`, `tract`, or `block_group`.",
         call. = FALSE)
  }
  if (!any(supplied)) {
    prior <- attr(table_df("last"), "prior")
    out <- prior[race_groups()]
    attr(out, "total") <- NA_integer_
    attr(out, "level") <- "national"
    attr(out, "type")  <- type
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
  attr(out, "total") <- as.integer(hit$total[1])
  attr(out, "level") <- level
  attr(out, "type")  <- type
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
