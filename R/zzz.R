## Suppress R CMD check NOTEs about lazy-loaded data referenced in package
## code by their bare names.
utils::globalVariables(c(
  "first_names", "last_names", "first_names_sex",
  "first_names_extra", "last_names_extra"
))

#' openBISG: Open Bayesian Improved Surname Geocoding for R
#'
#' Lookup tables and a Naive-Bayes / BISG / BIFSG probability calculator
#' built from the U.S. Census Bureau's 2020 Decennial Census frequently
#' occurring first and last names tabulations and the 2020-2024 ACS
#' Citizen Voting Age Population (CVAP) Special Tabulation. Given any
#' combination of first name, last name, and geography (ZIP / ZCTA,
#' Census Tract, or Block Group), returns the probability that the
#' bearer belongs to each of six race / Hispanic-origin groups, plus a
#' probability of sex from the first-name-by-sex table.
#'
#' @section Main entry points:
#' \describe{
#'   \item{[predict_race()]}{Single-call prediction with full per-token /
#'     geography metadata.}
#'   \item{[predict_sex()]}{Single-call sex probability from a first name.}
#'   \item{[predict_names()]}{Batch predictions over a data frame
#'     with auto-detected name and geography columns (race + sex).}
#'   \item{[predict_demog()]}{Vectorized batch engine — same model,
#'     orders of magnitude faster, with optional user-supplied name
#'     dictionaries, geography tables, and category groupings.}
#'   \item{[geo_prior()]}{Geographic prior `P(R | G)` from the bundled
#'     CVAP / VAP tables.}
#'   \item{[run_app()]}{Launch the bundled Shiny lookup app.}
#' }
#'
#' @section Bundled datasets:
#' [first_names], [last_names], [first_names_sex] (Census 2020),
#' [first_names_extra], [last_names_extra] (Rosenman, Olivella, and Imai
#' 2023 voter-file additions), and the six geographic priors:
#' [geo_zcta_cvap], [geo_tract_cvap], [geo_bg_cvap] (2020-2024 ACS
#' Special Tabulation), [geo_zcta_vap], [geo_tract_vap], [geo_bg_vap]
#' (2020 Decennial DHC Table P11).
#'
#' @keywords internal
#' @aliases openBISG-package
"_PACKAGE"
