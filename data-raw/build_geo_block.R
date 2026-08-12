## Build the block-level geographic prior shipped with the package.
##
## Output: data/geo_block_vap.rda — one row per 2020 census block with a
## nonzero voting-age population (5,704,969 of 8,174,955 tabulated
## blocks; 50 states + DC + PR), columns geoid + total + the six race /
## Hispanic groups as integer VAP COUNTS (not proportions — counts
## compress to roughly 60% of the size of derived doubles at xz -9, and
## every consumer row-normalizes on the fly).
##
## Source: the 2020 Census P.L. 94-171 Redistricting Data Summary Files
## (Table P4 — Hispanic or Latino, and Not Hispanic or Latino by Race,
## for the Population 18 Years and Over). There is no block-level CVAP:
## citizenship is not collected in the decennial census, and the CVAP
## Special Tabulation stops at block groups.
##
## Input options, in order of preference:
##
##   1. A pre-combined PL94171_BlockLvl.RData: the output of running
##      PL94171::pl_read() + pl_subset(sumlev = "750") +
##      pl_select_standard(clean_names = TRUE) over every state's
##      <st>2020.pl.zip and rbind-ing the results (one data frame named
##      `pl2020_block` with the standard pop_*/vap_* columns). Pass its
##      path as the first command-line argument.
##
##   2. Rebuild from the per-state .pl.zip files yourself:
##
##        library(PL94171)
##        parts <- lapply(list.files(dir, pattern = "2020.pl.zip$",
##                                   full.names = TRUE), function(f) {
##          pl_select_standard(pl_subset(pl_read(f), sumlev = "750"),
##                             clean_names = TRUE)
##        })
##        pl2020_block <- do.call(rbind, parts)
##        save(pl2020_block, file = "PL94171_BlockLvl.RData")
##
## The vap_* columns follow the redistricting-standard schema: vap_white
## etc. are the not-Hispanic single-race counts, vap_other / vap_two the
## not-Hispanic Some-Other-Race-alone / Two-or-More counts, and vap_hisp
## is Hispanic of any race — the same partition as DHC P11, so the
## six-group mapping below matches build_geo.R exactly and the six
## groups sum to vap on every row (verified by the stopifnot()).
##
## Run from inside the package root via:
##   Rscript data-raw/build_geo_block.R /path/to/PL94171_BlockLvl.RData

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args) >= 1L) args[[1]] else "PL94171_BlockLvl.RData"
if (!file.exists(src)) {
  stop("Input not found: ", src,
       " — pass the path to PL94171_BlockLvl.RData (see header).")
}

load(src)  # -> pl2020_block
x <- as.data.frame(pl2020_block)
rm(pl2020_block); invisible(gc())

stopifnot(all(x$summary_level == "750"),
          all(nchar(x$GEOID) == 15L))
message(nrow(x), " blocks read; ",
        sum(x$vap > 0L), " with nonzero VAP")

x <- x[x$vap > 0L, ]
geo_block_vap <- data.frame(
  geoid    = x$GEOID,
  total    = as.integer(x$vap),
  white    = as.integer(x$vap_white),
  black    = as.integer(x$vap_black),
  aian     = as.integer(x$vap_aian),
  aapi     = as.integer(x$vap_asian + x$vap_nhpi),
  nh_multi = as.integer(x$vap_other + x$vap_two),
  hispanic = as.integer(x$vap_hisp),
  stringsAsFactors = FALSE
)
rm(x); invisible(gc())
geo_block_vap <- geo_block_vap[order(geo_block_vap$geoid), ]
row.names(geo_block_vap) <- NULL

stopifnot(
  !anyNA(geo_block_vap),
  !anyDuplicated(geo_block_vap$geoid),
  all(geo_block_vap$total > 0L),
  ## The six-group mapping must partition VAP exactly.
  all(geo_block_vap$white + geo_block_vap$black + geo_block_vap$aian +
        geo_block_vap$aapi + geo_block_vap$nh_multi +
        geo_block_vap$hispanic == geo_block_vap$total)
)

out <- file.path("data", "geo_block_vap.rda")
save(geo_block_vap, file = out, compress = "xz", compression_level = 9)
message(sprintf("wrote %s (%.1f MB, %d rows)", out,
                file.size(out) / 1e6, nrow(geo_block_vap)))
