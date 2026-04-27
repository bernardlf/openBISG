## Build the geographic-prior datasets shipped with the package.
##
## Outputs six lazy-loaded data frames in data/, all with the same shape
## (one row per geography, columns geoid + total + the six race / Hispanic
## groups as proportions summing to 1):
##
##   geo_zcta_cvap   ZCTA × CVAP   (citizens 18+, 2020-2024 ACS Special Tabulation)
##   geo_zcta_vap    ZCTA × VAP    (everyone 18+, 2020 Decennial DHC table P11)
##   geo_tract_cvap  Tract × CVAP  (CVAP Special Tab, tract level)
##   geo_tract_vap   Tract × VAP   (DHC P11)
##   geo_bg_cvap     BG × CVAP     (CVAP Special Tab, block-group level)
##   geo_bg_vap      BG × VAP      (DHC P11)
##
## CVAP @ tract / BG comes directly from the special tabulation CSVs (which
## the user dropped in CVAP_2020-2024_ACS_csv_files/). CVAP @ ZCTA is built
## by area-weighted apportionment of tract CVAP through the official Census
## 2020 ZCTA-to-Tract relationship file (the special tab does not publish
## ZCTA, and the Census API does not expose 2020-2024 ACS CVAP-by-race at
## ZCTA either).
##
## VAP comes from the 2020 Decennial Demographic and Housing Characteristics
## File (DHC), Table P11 — same race × Hispanic breakdown as the CVAP file
## but for the full 18+ population (citizens + non-citizens). The decennial
## reference date (April 1, 2020) is not exactly the 2020-2024 ACS midpoint
## but is the canonical authoritative source for VAP-by-race-by-Hispanic at
## small areas, and falls within the CVAP special-tab time window.
##
## Run from inside r-pkg/openBISG/ via:
##   Rscript data-raw/build_geo.R
##
## Pulls ~52 state queries from the Census API for tract VAP and another
## ~52 for BG VAP. No API key is required, but the CENSUS_API_KEY environment
## variable will be honored if set.

suppressPackageStartupMessages({
  library(curl)
  library(jsonlite)
})

repo_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
pkg_root  <- normalizePath(".",                   mustWork = TRUE)

cvap_dir  <- file.path(repo_root, "CVAP_2020-2024_ACS_csv_files")
data_dir  <- file.path(pkg_root, "data")
cache_dir <- file.path(pkg_root, "data-raw", "geo_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

GROUPS <- c("white", "black", "aian", "aapi", "nh_multi", "hispanic")

api_key <- Sys.getenv("CENSUS_API_KEY", unset = "")

## ----- helpers ------------------------------------------------------------

cache_get <- function(url, file_name) {
  path <- file.path(cache_dir, file_name)
  if (file.exists(path) && file.size(path) > 0L) return(path)
  message("  GET ", url)
  curl::curl_download(url, path, quiet = TRUE)
  Sys.sleep(0.05)
  path
}

api_url <- function(path, vars, geo_for, geo_in = NULL) {
  base <- "https://api.census.gov/data/"
  q <- paste0(base, path,
              "?get=", paste(vars, collapse = ","),
              "&for=", utils::URLencode(geo_for, reserved = TRUE))
  if (!is.null(geo_in))
    q <- paste0(q, "&in=", utils::URLencode(geo_in, reserved = TRUE))
  if (nzchar(api_key)) q <- paste0(q, "&key=", api_key)
  q
}

read_api_json <- function(url, file_name) {
  path <- cache_get(url, file_name)
  raw <- readChar(path, file.info(path)$size, useBytes = TRUE)
  jsonlite::fromJSON(raw, simplifyVector = FALSE)
}

api_to_df <- function(payload) {
  if (length(payload) < 2L) return(data.frame())
  hdr <- unlist(payload[[1]])
  rows <- payload[-1]
  mat <- do.call(rbind, lapply(rows, function(r) unlist(r)))
  out <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(out) <- hdr
  out
}

## ----- 1. CVAP @ tract / block group from the special tab ----------------

read_cvap_special <- function(path) {
  message("Reading ", basename(path), " (this is the slow part) ...")
  df <- utils::read.csv(path, stringsAsFactors = FALSE,
                        colClasses = c(geoname = "character",
                                       lntitle = "character",
                                       geoid   = "character",
                                       lnnumber = "integer",
                                       cit_est  = "integer",
                                       cit_moe  = "integer",
                                       cvap_est = "integer",
                                       cvap_moe = "integer"))
  df
}

mapping_cvap <- list(
  white    = "White Alone",
  black    = "Black or African American Alone",
  aian     = "American Indian or Alaska Native Alone",
  hispanic = "Hispanic or Latino"
)

aapi_lns <- c("Asian Alone",
              "Native Hawaiian or Other Pacific Islander Alone")

nh_multi_lns <- c("American Indian or Alaska Native and White",
                  "Asian and White",
                  "Black or African American and White",
                  "American Indian or Alaska Native and Black or African American",
                  "Remainder of Two or More Race Responses")

cvap_to_six <- function(df) {
  ## Pivot the 13-row-per-geography long file into one row per geography
  ## with six count columns plus a `total` column, then convert to props.
  pick <- function(line_titles)
    stats::aggregate(cvap_est ~ geoid,
                     data = subset(df, lntitle %in% line_titles),
                     FUN  = sum, na.rm = TRUE)

  white    <- pick(mapping_cvap$white);    names(white)[2]    <- "white"
  black    <- pick(mapping_cvap$black);    names(black)[2]    <- "black"
  aian     <- pick(mapping_cvap$aian);     names(aian)[2]     <- "aian"
  aapi     <- pick(aapi_lns);              names(aapi)[2]     <- "aapi"
  nhm      <- pick(nh_multi_lns);          names(nhm)[2]      <- "nh_multi"
  hisp     <- pick(mapping_cvap$hispanic); names(hisp)[2]     <- "hispanic"
  tot      <- subset(df, lntitle == "Total", select = c("geoid", "cvap_est"))
  names(tot)[2] <- "total"

  out <- Reduce(function(a, b) merge(a, b, by = "geoid", all = TRUE),
                list(tot, white, black, aian, aapi, nhm, hisp))
  out[is.na(out)] <- 0L
  out$geoid <- sub("^\\d{7}US", "", out$geoid)  # strip 1400000US / 1500000US prefix
  ## Convert counts -> proportions; 0/0 -> NA so callers know the geo is empty.
  for (g in GROUPS) {
    out[[g]] <- ifelse(out$total > 0, out[[g]] / out$total, NA_real_)
  }
  out[, c("geoid", "total", GROUPS)]
}

build_cvap_tract <- function() {
  df <- read_cvap_special(file.path(cvap_dir, "Tract.csv"))
  out <- cvap_to_six(df)
  message("  ", nrow(out), " tracts")
  out
}

build_cvap_bg <- function() {
  df <- read_cvap_special(file.path(cvap_dir, "BlockGr.csv"))
  out <- cvap_to_six(df)
  message("  ", nrow(out), " block groups")
  out
}

## ----- 2. VAP @ ZCTA / tract / BG from DHC P11 ---------------------------

DHC_P11_VARS <- c("P11_001N",                                # total
                  "P11_002N",                                # Hispanic
                  "P11_005N", "P11_006N", "P11_007N",        # NH white/black/aian
                  "P11_008N", "P11_009N",                    # NH asian, NHPI
                  "P11_010N", "P11_011N")                    # NH SOR alone, NH 2+

p11_to_six <- function(df) {
  num <- function(x) suppressWarnings(as.numeric(x))
  total <- num(df$P11_001N)
  white <- num(df$P11_005N)
  black <- num(df$P11_006N)
  aian  <- num(df$P11_007N)
  aapi  <- num(df$P11_008N) + num(df$P11_009N)
  nhm   <- num(df$P11_010N) + num(df$P11_011N)
  hisp  <- num(df$P11_002N)
  out <- data.frame(total = total, white = white, black = black,
                    aian = aian, aapi = aapi, nh_multi = nhm,
                    hispanic = hisp, stringsAsFactors = FALSE)
  for (g in GROUPS) {
    out[[g]] <- ifelse(out$total > 0, out[[g]] / out$total, NA_real_)
  }
  out
}

build_vap_zcta <- function() {
  message("Pulling DHC P11 (VAP) at ZCTA ...")
  url <- api_url("2020/dec/dhc", DHC_P11_VARS,
                 geo_for = "zip code tabulation area:*")
  payload <- read_api_json(url, "dhc_p11_zcta.json")
  df <- api_to_df(payload)
  six <- p11_to_six(df)
  out <- data.frame(geoid = df[["zip code tabulation area"]],
                    six,
                    stringsAsFactors = FALSE)
  message("  ", nrow(out), " ZCTAs")
  out[, c("geoid", "total", GROUPS)]
}

state_fips <- function() {
  ## State FIPS that the CVAP special tab covers (incl. DC + PR), but PR
  ## (72) is excluded for VAP — DHC P11 isn't published for PR; PR uses a
  ## separate dec/dhcvi data product with different tables.
  c("01","02","04","05","06","08","09","10","11","12","13","15","16","17",
    "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
    "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
    "47","48","49","50","51","53","54","55","56")
}

build_vap_tract <- function() {
  message("Pulling DHC P11 (VAP) at tract for 51 states ...")
  states <- state_fips()
  parts <- vector("list", length(states))
  for (i in seq_along(states)) {
    st <- states[i]
    url <- api_url("2020/dec/dhc", DHC_P11_VARS,
                   geo_for = "tract:*",
                   geo_in  = paste0("state:", st))
    payload <- read_api_json(url, paste0("dhc_p11_tract_", st, ".json"))
    df <- api_to_df(payload)
    six <- p11_to_six(df)
    parts[[i]] <- data.frame(
      geoid = paste0(df$state, df$county, df$tract),
      six,
      stringsAsFactors = FALSE
    )
    message("  state ", st, ": ", nrow(parts[[i]]), " tracts")
  }
  out <- do.call(rbind, parts)
  out[, c("geoid", "total", GROUPS)]
}

build_vap_bg <- function() {
  message("Pulling DHC P11 (VAP) at block group for 51 states ...")
  states <- state_fips()
  parts <- vector("list", length(states))
  for (i in seq_along(states)) {
    st <- states[i]
    url <- api_url("2020/dec/dhc", DHC_P11_VARS,
                   geo_for = "block group:*",
                   geo_in  = paste0("state:", st, " county:* tract:*"))
    payload <- read_api_json(url, paste0("dhc_p11_bg_", st, ".json"))
    df <- api_to_df(payload)
    six <- p11_to_six(df)
    parts[[i]] <- data.frame(
      geoid = paste0(df$state, df$county, df$tract,
                     df[["block group"]]),
      six,
      stringsAsFactors = FALSE
    )
    message("  state ", st, ": ", nrow(parts[[i]]), " block groups")
  }
  out <- do.call(rbind, parts)
  out[, c("geoid", "total", GROUPS)]
}

## ----- 3. CVAP @ ZCTA via tract -> ZCTA aggregation ----------------------

build_cvap_zcta <- function(tract_cvap) {
  rel_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_tract20_natl.txt"
  rel_path <- cache_get(rel_url, "zcta520_tract20_rel.txt")
  rel <- utils::read.delim(rel_path, sep = "|", stringsAsFactors = FALSE,
                           colClasses = "character")
  ## Keep ZCTA-tract pairs that overlap on land area; drop the unmatched
  ## (rows where ZCTA cols are empty are tract-only listings, and vice
  ## versa).
  rel <- rel[nzchar(rel$GEOID_ZCTA5_20) & nzchar(rel$GEOID_TRACT_20), ]
  rel$AREALAND_PART       <- as.numeric(rel$AREALAND_PART)
  rel$AREALAND_TRACT_20   <- as.numeric(rel$AREALAND_TRACT_20)
  rel$tract_share <- ifelse(rel$AREALAND_TRACT_20 > 0,
                            rel$AREALAND_PART / rel$AREALAND_TRACT_20,
                            0)

  ## Land-area apportionment: assume the tract's CVAP is uniformly
  ## distributed across its land area. Imperfect but standard practice
  ## when no finer-grained crosswalk is available; document accordingly.
  agg_input <- merge(
    rel[, c("GEOID_ZCTA5_20", "GEOID_TRACT_20", "tract_share")],
    tract_cvap, by.x = "GEOID_TRACT_20", by.y = "geoid"
  )
  ## Convert tract proportions back to counts, then apportion by share.
  ## Order matters: build the per-group counts BEFORE rescaling `total`,
  ## since the proportion->count conversion needs the unmodified total.
  count_cols <- c("total", GROUPS)
  for (g in GROUPS) {
    agg_input[[g]] <- agg_input[[g]] * agg_input$total * agg_input$tract_share
  }
  agg_input$total <- agg_input$total * agg_input$tract_share

  out <- stats::aggregate(
    agg_input[, count_cols],
    by = list(geoid = agg_input$GEOID_ZCTA5_20),
    FUN = sum, na.rm = TRUE
  )
  for (g in GROUPS) {
    out[[g]] <- ifelse(out$total > 0, out[[g]] / out$total, NA_real_)
  }
  out$total <- round(out$total)
  message("  ", nrow(out), " ZCTAs (CVAP, area-weighted from tracts)")
  out[, c("geoid", "total", GROUPS)]
}

## ----- main --------------------------------------------------------------

main <- function() {
  geo_tract_cvap <- build_cvap_tract()
  geo_bg_cvap    <- build_cvap_bg()
  geo_zcta_cvap  <- build_cvap_zcta(geo_tract_cvap)

  geo_zcta_vap   <- build_vap_zcta()
  geo_tract_vap  <- build_vap_tract()
  geo_bg_vap     <- build_vap_bg()

  save(geo_zcta_cvap,  file = file.path(data_dir, "geo_zcta_cvap.rda"),
       compress = "xz", compression_level = 9)
  save(geo_tract_cvap, file = file.path(data_dir, "geo_tract_cvap.rda"),
       compress = "xz", compression_level = 9)
  save(geo_bg_cvap,    file = file.path(data_dir, "geo_bg_cvap.rda"),
       compress = "xz", compression_level = 9)

  save(geo_zcta_vap,   file = file.path(data_dir, "geo_zcta_vap.rda"),
       compress = "xz", compression_level = 9)
  save(geo_tract_vap,  file = file.path(data_dir, "geo_tract_vap.rda"),
       compress = "xz", compression_level = 9)
  save(geo_bg_vap,     file = file.path(data_dir, "geo_bg_vap.rda"),
       compress = "xz", compression_level = 9)

  message("Wrote 6 .rda files to ", data_dir)
}

if (sys.nframe() == 0L) main()
