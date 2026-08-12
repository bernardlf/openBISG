#' First-name proportions by race / Hispanic origin (2020 Census)
#'
#' Per-name proportion table derived from the U.S. Census Bureau's 2020
#' Decennial Census frequently occurring first names tabulation
#' (Comenetz, 2026; brief C2020BR-13). One row per first name with a
#' frequency of at least 100 in the 2020 Census; the trailing
#' `ALL OTHER NAMES` row is excluded from the lookup but its contribution is
#' folded into the `prior` attribute.
#'
#' @format A data frame with 53,615 rows and 8 columns:
#' \describe{
#'   \item{name}{Uppercase, NFD-stripped first name (length-one ASCII).}
#'   \item{frequency}{Census-published `FREQUENCY (COUNT)`.}
#'   \item{white,black,aian,aapi,nh_multi,hispanic}{Proportion of
#'     people with this first name belonging to each of the six
#'     race / Hispanic-origin groups. Rows sum to 1.}
#' }
#' Two attributes are attached:
#' \describe{
#'   \item{prior}{Length-6 named numeric vector — the frequency-weighted
#'     marginal `P(group)` across all rows including `ALL OTHER NAMES`.}
#'   \item{groups}{Length-6 character vector of column keys, in
#'     [race_groups()] order.}
#' }
#' @source U.S. Census Bureau, 2020 Census Frequently Occurring Names,
#'   <https://www.census.gov/topics/population/genealogy/data/2020_names.html>.
#' @seealso [last_names], [first_names_sex].
"first_names"

#' Last-name proportions by race / Hispanic origin (2020 Census)
#'
#' Per-name proportion table derived from the U.S. Census Bureau's 2020
#' Decennial Census frequently occurring last names tabulation
#' (Comenetz, 2026; brief C2020BR-14). One row per last name with a
#' frequency of at least 100 in the 2020 Census.
#'
#' @inherit first_names format
#' @source U.S. Census Bureau, 2020 Census Frequently Occurring Names,
#'   <https://www.census.gov/topics/population/genealogy/data/2020_names.html>.
#' @seealso [first_names], [first_names_sex].
"last_names"

#' First-name proportions by sex (2020 Census)
#'
#' Per-name proportion of male / female bearers, from the 2020 Census
#' first-name-by-sex table that accompanies the race/Hispanic-origin tables.
#'
#' @format A data frame with 53,615 rows and 4 columns: `name`, `frequency`,
#'   `male`, `female`. Same `prior` and `groups` attributes as
#'   [first_names].
#' @source U.S. Census Bureau, 2020 Census Frequently Occurring Names.
#' @seealso [first_names], [last_names].
"first_names_sex"

#' Rosenman, Olivella, and Imai (2023) first-name additions not in Census 2020
#'
#' Voter-file first-name race / Hispanic-origin probabilities for the
#' 85,213 names that appear in `first_nameRaceProbs.rData` but **not**
#' in [first_names]. Used as a fallback when [predict_race()] /
#' [predict_names()] / [lookup_with_fallback()] are called with
#' `include_extra = TRUE` and every Census 2020 table missed.
#'
#' Rosenman ships only 5 race columns — white, black, AAPI, OTHER
#' (which lumps AIAN, two-or-more, and other), and Hispanic. To stay
#' compatible with the 6-column Census tables and the Naive-Bayes
#' combination unchanged, the OTHER bucket is split between `aian` and
#' `nh_multi` proportionally to the Census prior at build time:
#' `aian = OTHER * P(aian) / (P(aian) + P(nh_multi))` and
#' `nh_multi = OTHER * P(nh_multi) / (P(aian) + P(nh_multi))`.
#'
#' @format A data frame with 85,213 rows and 8 columns:
#' \describe{
#'   \item{name}{Uppercase, NFD-stripped first name (length-one ASCII).}
#'   \item{frequency}{`NA_integer_` — Rosenman publishes only the
#'     conditional probabilities, not name-level frequencies.}
#'   \item{white,black,aian,aapi,nh_multi,hispanic}{Proportion of
#'     people with this first name belonging to each of the six
#'     race / Hispanic-origin groups. Rows sum to 1.}
#' }
#' Two attributes are attached:
#' \describe{
#'   \item{prior}{The Census-2020 first-name prior, copied from
#'     [first_names] (Rosenman has no frequencies of its own to derive
#'     a prior from, and the Naive-Bayes formula uses the
#'     [last_names] prior throughout regardless).}
#'   \item{groups}{Length-6 character vector of column keys, in
#'     [race_groups()] order.}
#' }
#' @source Rosenman, E. T. R., Olivella, S., & Imai, K. (2023). Race
#'   and ethnicity data for first, middle, and surnames. *Scientific
#'   Data* 10, 299.
#'   <https://doi.org/10.7910/DVN/YL2OXB> (Harvard Dataverse, V1).
#' @seealso [first_names], [last_names_extra].
"first_names_extra"

#' Rosenman, Olivella, and Imai (2023) last-name additions not in Census 2020
#'
#' Voter-file surname race / Hispanic-origin probabilities for the
#' 195,571 surnames that appear in `last_nameRaceProbs.rData` but
#' **not** in [last_names]. Same source format and OTHER → AIAN /
#' nh_multi expansion as [first_names_extra].
#'
#' @inherit first_names_extra format
#' @source Rosenman, E. T. R., Olivella, S., & Imai, K. (2023). Race
#'   and ethnicity data for first, middle, and surnames. *Scientific
#'   Data* 10, 299.
#'   <https://doi.org/10.7910/DVN/YL2OXB> (Harvard Dataverse, V1).
#' @seealso [last_names], [first_names_extra].
"last_names_extra"

#' Race / Hispanic-origin proportions for the citizen voting age
#' population (CVAP) by ZCTA / Census Tract / Block Group
#'
#' Bundled geographic priors built from the 2020-2024 American
#' Community Survey 5-year **Citizen Voting Age Population (CVAP)
#' Special Tabulation**. Rows are one geography each (one of ZIP Code
#' Tabulation Area, Census Tract, or Census Block Group) and columns
#' are the share of CVAP in each of the six [race_groups()] groups
#' (proportions sum to 1; rows where CVAP is zero have all six
#' proportions set to `NA_real_`). The `total` column is the count of
#' citizens age 18 and over that the proportions are computed over.
#'
#' The CVAP Special Tabulation is published with 13 race lines per
#' geography. The mapping into the package's six-group schema is:
#' \describe{
#'   \item{white}{"White Alone"}
#'   \item{black}{"Black or African American Alone"}
#'   \item{aian}{"American Indian or Alaska Native Alone"}
#'   \item{aapi}{"Asian Alone" + "Native Hawaiian or Other Pacific
#'     Islander Alone"}
#'   \item{nh_multi}{the four published two-race lines (AIAN+White,
#'     Asian+White, Black+White, AIAN+Black) plus "Remainder of Two
#'     or More Race Responses"}
#'   \item{hispanic}{"Hispanic or Latino"}
#' }
#'
#' The Special Tab does not publish ZCTA tables, so `geo_zcta_cvap`
#' is built by area-weighted apportionment of `geo_tract_cvap` through
#' the official Census Bureau **2020 ZCTA-to-Tract relationship file**
#' (assumes uniform population density within each tract). Tract and
#' BG values come straight from the Special Tab.
#'
#' @format Each is a data frame with columns:
#' \describe{
#'   \item{geoid}{Geography identifier — 5-digit ZCTA for
#'     `geo_zcta_cvap`, 11-digit tract FIPS (state(2) + county(3) +
#'     tract(6)) for `geo_tract_cvap`, 12-digit BG FIPS (... + bg(1))
#'     for `geo_bg_cvap`.}
#'   \item{total}{CVAP count for the geography (integer).}
#'   \item{white,black,aian,aapi,nh_multi,hispanic}{Proportion of CVAP
#'     in each group; rows sum to 1 (or all `NA_real_` when total = 0).}
#' }
#' @source U.S. Census Bureau, 2020-2024 American Community Survey
#'   5-Year Citizen Voting Age Population (CVAP) Special Tabulation.
#'   <https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.html>.
#' @seealso [geo_prior()], [predict_race()], [geo_zcta_vap].
"geo_zcta_cvap"

#' @rdname geo_zcta_cvap
"geo_tract_cvap"

#' @rdname geo_zcta_cvap
"geo_bg_cvap"

#' Race / Hispanic-origin proportions for the voting age population
#' (VAP) by ZCTA / Census Tract / Block Group
#'
#' Bundled geographic priors built from the 2020 Decennial Census
#' Demographic and Housing Characteristics File (DHC), Table P11
#' "Hispanic or Latino, and Not Hispanic or Latino By Race For The
#' Population 18 Years and Over". Same row schema as the CVAP-based
#' tables ([geo_zcta_cvap] etc.) but the denominator is the full
#' age-18-plus population including non-citizens. Use this when the
#' bearer's citizenship status is unknown.
#'
#' Built from the public Census API (no key required at this scale).
#' The mapping from DHC P11 variables to the six-group schema is:
#' `white = P11_005N`, `black = P11_006N`, `aian = P11_007N`,
#' `aapi = P11_008N + P11_009N`, `nh_multi = P11_010N + P11_011N`
#' (NH "Some Other Race alone" + NH two or more races, lumped to keep
#' the schema closed under six categories), `hispanic = P11_002N`.
#'
#' Reference date is April 1, 2020; the CVAP Special Tab uses the
#' 2020-2024 ACS five-year window. The two are slightly out of phase
#' but P11 is the canonical authoritative source for VAP-by-race at
#' small geographies and falls within the CVAP window.
#'
#' Puerto Rico is not included in the bundled VAP tables — the DHC
#' P11 table is not published for PR (Census uses a separate
#' demographic profile product for PR, with different table numbers).
#' The CVAP-based tables \emph{do} include PR.
#'
#' @inherit geo_zcta_cvap format
#' @source U.S. Census Bureau, 2020 Decennial Demographic and Housing
#'   Characteristics File (DHC), Table P11.
#'   <https://api.census.gov/data/2020/dec/dhc>.
#' @seealso [geo_prior()], [predict_race()], [geo_zcta_cvap].
"geo_zcta_vap"

#' @rdname geo_zcta_vap
"geo_tract_vap"

#' @rdname geo_zcta_vap
"geo_bg_vap"

#' Voting age population counts by race / Hispanic origin for every
#' populated 2020 Census Block
#'
#' Block-level geographic prior built from the 2020 Decennial Census
#' **P.L. 94-171 Redistricting Data** (Table P4, "Hispanic or Latino,
#' and Not Hispanic or Latino by Race for the Population 18 Years and
#' Over"). One row per 2020 census block with a nonzero voting-age
#' population — 5,704,969 of the 8,174,955 tabulated blocks across the
#' 50 states, DC, and Puerto Rico (which the DHC-based VAP tables lack).
#'
#' Unlike the other bundled geography tables, this one stores **integer
#' counts, not proportions**: the six group columns sum exactly to
#' `total` on every row. Counts compress far better than derived
#' floating-point shares (this table is by far the largest shipped),
#' and every consumer — [geo_prior()], [predict_race()],
#' [predict_names()], [predict_demog()] — row-normalizes to proportions
#' on the fly, so the two schemas produce identical results.
#'
#' The mapping from P4 categories to the six-group schema matches the
#' DHC P11 mapping used for [geo_zcta_vap]: `white` / `black` / `aian`
#' are the not-Hispanic alone counts, `aapi` is not-Hispanic Asian +
#' NHPI, `nh_multi` is not-Hispanic Some Other Race alone + Two or More
#' Races, and `hispanic` is Hispanic or Latino of any race.
#'
#' There is no CVAP companion table: citizenship is not collected in
#' the decennial census, and the CVAP Special Tabulation's smallest
#' geography is the block group. Block lookups with `type = "cvap"`
#' therefore use the block's parent block group (always
#' `substr(geoid, 1, 12)`), as do lookups for blocks absent from this
#' table (zero-VAP blocks) — see [geo_prior()] and the
#' `block_fallback` arguments.
#'
#' @format A data frame with 5,704,969 rows and 8 columns:
#' \describe{
#'   \item{geoid}{15-digit block FIPS: state(2) + county(3) + tract(6)
#'     + block(4). The first block digit is the block-group digit.}
#'   \item{total}{VAP count for the block (integer, always positive).}
#'   \item{white,black,aian,aapi,nh_multi,hispanic}{Integer VAP counts
#'     per group; rows sum exactly to `total`.}
#' }
#' @source U.S. Census Bureau, 2020 Census P.L. 94-171 Redistricting
#'   Data Summary Files.
#'   <https://www.census.gov/programs-surveys/decennial-census/about/rdo/summary-files.html>.
#' @seealso [geo_prior()], [predict_demog()], [geo_zcta_vap].
"geo_block_vap"
