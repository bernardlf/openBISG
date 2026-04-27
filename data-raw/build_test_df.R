## data-raw/build_test_df.R
##
## Generates a 10,000-row test data frame for benchmarking and
## integration-testing predict_names(). Run from inside the package
## root via:
##
##   Rscript data-raw/build_test_df.R
##
## Writes data-raw/test_df.rds. Deterministic; uses set.seed.

suppressPackageStartupMessages(library(openBISG))

set.seed(20260427L)

n <- 10000L

fn_pool <- openBISG:::table_df("first")
ln_pool <- openBISG:::table_df("last")

draw_weighted <- function(pool_names, pool_freqs, k) {
  sample(pool_names, k, replace = TRUE,
         prob = pool_freqs / sum(pool_freqs))
}

## First names. Sample one per row, then turn ~10% into compounds by
## appending a second draw separated by a space.
first <- draw_weighted(fn_pool$name, fn_pool$frequency, n)
n_compound <- round(0.10 * n)
compound_idx <- sample.int(n, size = n_compound)
extras <- draw_weighted(fn_pool$name, fn_pool$frequency, n_compound)
first[compound_idx] <- paste(first[compound_idx], extras)

## Middle names: ~10% NA, otherwise a random first name.
middle <- draw_weighted(fn_pool$name, fn_pool$frequency, n)
middle[sample.int(n, size = round(0.10 * n))] <- NA_character_

## Surnames.
last <- draw_weighted(ln_pool$name, ln_pool$frequency, n)

## Maiden: ~50% NA, otherwise a random surname.
maiden <- draw_weighted(ln_pool$name, ln_pool$frequency, n)
maiden[sample.int(n, size = round(0.50 * n))] <- NA_character_

## Geographies: weighted by population (`total`) within each table for
## realism. Sampled independently — a row's ZIP / tract / BG do not
## point to the same physical location.
sample_geo <- function(tbl) {
  ok <- !is.na(tbl$total) & tbl$total > 0L
  sample(tbl$geoid[ok], n, replace = TRUE,
         prob = tbl$total[ok] / sum(tbl$total[ok]))
}

zcta        <- sample_geo(openBISG::geo_zcta_cvap)
tract       <- sample_geo(openBISG::geo_tract_cvap)
block_group <- sample_geo(openBISG::geo_bg_cvap)

test_df <- data.frame(
  first       = first,
  middle      = middle,
  last        = last,
  maiden      = maiden,
  zcta        = zcta,
  tract       = tract,
  block_group = block_group,
  stringsAsFactors = FALSE
)

out_path <- file.path("data-raw", "test_df.rds")
saveRDS(test_df, out_path)

cat("Wrote ", out_path, "  (", nrow(test_df), " rows)\n", sep = "")
cat("\nHead:\n")
print(utils::head(test_df))
cat("\nNA counts per column:\n")
print(colSums(is.na(test_df)))
cat("\nCompound first-name share: ",
    sprintf("%.3f", mean(grepl(" ", test_df$first))), "\n", sep = "")
cat("Distinct ZCTA / tract / BG: ",
    length(unique(test_df$zcta)),  " / ",
    length(unique(test_df$tract)), " / ",
    length(unique(test_df$block_group)), "\n", sep = "")
