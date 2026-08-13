## Build TOTAL-POPULATION (all ages) geographic priors at the block,
## block-group, and tract levels — the same population basis as wru's
## default Census download (P.L. 94-171 Table P2) — alongside the
## shipped VAP tables.
##
## Outputs (to the directory named by the second argument — default
## `data`, where they ship as bundled package data as of 0.8.0):
##   geo_block_pop.rda  one row per 2020 block with nonzero total
##                      population; columns geoid + total + the six
##                      race / Hispanic groups as integer COUNTS
##                      (same conventions as geo_block_vap).
##   geo_bg_pop.rda     block group level; geoid + total + the six
##                      groups as PROPORTIONS summing to 1 (NA when
##                      total = 0), matching geo_bg_vap's shape.
##   geo_tract_pop.rda  tract level, same shape as geo_tract_vap.
##
## Source: the same combined PL94171_BlockLvl.RData used by
## data-raw/build_geo_block.R (one data frame `pl2020_block`, the output
## of PL94171::pl_read() + pl_subset(sumlev = "750") +
## pl_select_standard(clean_names = TRUE) rbind-ed over every state).
## The standard schema carries pop_* columns (Table P2, all ages)
## alongside the vap_* columns (Table P4, 18+): pop_white etc. are the
## not-Hispanic single-race counts, pop_other / pop_two the not-Hispanic
## Some-Other-Race-alone / Two-or-More counts, and pop_hisp is Hispanic
## of any race — so the six-group mapping below matches build_geo_block.R
## exactly and the six groups sum to pop on every row.
##
## Block-group and tract tables are EXACT aggregations of the block
## counts (P.L. blocks nest perfectly): bg geoid = substr(geoid, 1, 12),
## tract geoid = substr(geoid, 1, 11). Unlike the VAP tables (DHC P11),
## no separate download is needed. Blocks with zero total population
## are dropped from the block table (as in geo_block_vap) but still
## contribute (zeros) to their parents, whose universe therefore covers
## every tabulated block.
##
## Run from inside the package root via:
##   Rscript data-raw/build_geo_pop.R /path/to/PL94171_BlockLvl.RData

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args) >= 1L) args[[1]] else "PL94171_BlockLvl.RData"
out_dir <- if (length(args) >= 2L) args[[2]] else "data"
if (!file.exists(src)) {
  stop("Input not found: ", src,
       " — pass the path to PL94171_BlockLvl.RData (see header).")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

load(src)  # -> pl2020_block
x <- as.data.frame(pl2020_block)
rm(pl2020_block); invisible(gc())

stopifnot(all(x$summary_level == "750"),
          all(nchar(x$GEOID) == 15L))
message(nrow(x), " blocks read; ",
        sum(x$pop > 0L), " with nonzero total population")

groups <- c("white", "black", "aian", "aapi", "nh_multi", "hispanic")
cnt <- data.frame(
  geoid    = x$GEOID,
  total    = as.integer(x$pop),
  white    = as.integer(x$pop_white),
  black    = as.integer(x$pop_black),
  aian     = as.integer(x$pop_aian),
  aapi     = as.integer(x$pop_asian + x$pop_nhpi),
  nh_multi = as.integer(x$pop_other + x$pop_two),
  hispanic = as.integer(x$pop_hisp),
  stringsAsFactors = FALSE
)
rm(x); invisible(gc())

stopifnot(
  !anyNA(cnt),
  !anyDuplicated(cnt$geoid),
  ## The six-group mapping must partition total population exactly.
  all(cnt$white + cnt$black + cnt$aian + cnt$aapi + cnt$nh_multi +
        cnt$hispanic == cnt$total)
)

## ---- Block table: nonzero-population rows, integer counts -------------
geo_block_pop <- cnt[cnt$total > 0L, , drop = FALSE]
geo_block_pop <- geo_block_pop[order(geo_block_pop$geoid), ]
row.names(geo_block_pop) <- NULL

out <- file.path(out_dir, "geo_block_pop.rda")
save(geo_block_pop, file = out, compress = "xz", compression_level = 9)
message(sprintf("wrote %s (%.1f MB, %d rows)", out,
                file.size(out) / 1e6, nrow(geo_block_pop)))

## ---- Aggregate exact block counts to parents ---------------------------
roll_up <- function(cnt, digits) {
  key <- substr(cnt$geoid, 1L, digits)
  m <- rowsum(as.matrix(cnt[, c("total", groups)]), key)
  agg <- data.frame(geoid = rownames(m), m, row.names = NULL,
                    stringsAsFactors = FALSE)
  agg <- agg[order(agg$geoid), ]
  stopifnot(all(agg$white + agg$black + agg$aian + agg$aapi +
                  agg$nh_multi + agg$hispanic == agg$total))
  ## Proportions summing to 1, NA when total = 0 — the shape of the
  ## shipped bg / tract tables.
  pm <- as.matrix(agg[, groups])
  pm <- pm / ifelse(agg$total > 0L, agg$total, NA_real_)
  out <- data.frame(geoid = agg$geoid, total = as.integer(agg$total),
                    pm, row.names = NULL, stringsAsFactors = FALSE)
  out
}

geo_bg_pop <- roll_up(cnt, 12L)
out <- file.path(out_dir, "geo_bg_pop.rda")
save(geo_bg_pop, file = out, compress = "xz", compression_level = 9)
message(sprintf("wrote %s (%.1f MB, %d rows)", out,
                file.size(out) / 1e6, nrow(geo_bg_pop)))

geo_tract_pop <- roll_up(cnt, 11L)
out <- file.path(out_dir, "geo_tract_pop.rda")
save(geo_tract_pop, file = out, compress = "xz", compression_level = 9)
message(sprintf("wrote %s (%.1f MB, %d rows)", out,
                file.size(out) / 1e6, nrow(geo_tract_pop)))

## ---- Cross-checks against the shipped VAP tables -----------------------
## Same geography universes: every populated-VAP block must appear in
## the population table (VAP > 0 implies pop > 0), and bg / tract geoid
## sets should match the DHC-based VAP tables closely.
if (file.exists(file.path("data", "geo_block_vap.rda"))) {
  e <- new.env()
  load(file.path("data", "geo_block_vap.rda"), envir = e)
  missing_blocks <- sum(!(e$geo_block_vap$geoid %in% geo_block_pop$geoid))
  message("populated-VAP blocks absent from the pop table: ",
          missing_blocks, " (expect 0)")
  load(file.path("data", "geo_bg_vap.rda"), envir = e)
  message("bg geoids: pop table ", nrow(geo_bg_pop), ", VAP table ",
          nrow(e$geo_bg_vap), "; shared ",
          sum(geo_bg_pop$geoid %in% e$geo_bg_vap$geoid))
  load(file.path("data", "geo_tract_vap.rda"), envir = e)
  message("tract geoids: pop table ", nrow(geo_tract_pop),
          ", VAP table ", nrow(e$geo_tract_vap), "; shared ",
          sum(geo_tract_pop$geoid %in% e$geo_tract_vap$geoid))
}
