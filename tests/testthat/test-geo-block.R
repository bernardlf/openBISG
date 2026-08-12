## Block-level geography: direct lookups against geo_block_vap, the
## block -> block-group fallback for GEOIDs not among the populated 2020
## blocks, the CVAP reroute (no block-level CVAP exists), and the
## block_fallback switch.

## A populated block, its parent block group, and a syntactically valid
## block GEOID in the same block group that is NOT in geo_block_vap
## (i.e. behaves like a zero-VAP block).
pop_block <- function() openBISG::geo_block_vap$geoid[1]
parent_bg <- function() substr(pop_block(), 1L, 12L)
.gone_cache <- new.env(parent = emptyenv())
gone_block <- function() {
  if (!is.null(.gone_cache$id)) return(.gone_cache$id)
  ## A block GEOID is the 12-digit parent block group plus the last
  ## three digits of the block number. Pick a three-digit suffix not
  ## used by any populated block in the parent block group.
  bg <- parent_bg()
  taken <- substr(grep(paste0("^", bg), openBISG::geo_block_vap$geoid,
                       value = TRUE), 13L, 15L)
  free <- setdiff(sprintf("%03d", 999:0), taken)
  if (length(free) == 0L) {
    skip("every block GEOID under the first block group is populated")
  }
  .gone_cache$id <- paste0(bg, free[1])
  .gone_cache$id
}

test_that("geo_block_vap stores exact integer count partitions", {
  d <- openBISG::geo_block_vap
  expect_true(all(vapply(d[-1], is.integer, logical(1))))
  expect_true(all(nchar(d$geoid[1:1000]) == 15L))
  head_rows <- d[1:10000, ]
  expect_true(all(rowSums(head_rows[, race_groups()]) == head_rows$total))
  expect_true(all(head_rows$total > 0L))
})

test_that("geo_prior accepts plain and Summary-File-prefixed block GEOIDs", {
  key <- pop_block()
  plain <- geo_prior(block = key, type = "vap")
  expect_false(is.null(plain))
  expect_named(plain, race_groups())
  expect_equal(sum(plain), 1, tolerance = 1e-12)
  expect_equal(attr(plain, "level"), "block")
  expect_null(attr(plain, "fallback_from"))
  prefixed <- geo_prior(block = paste0("7500000US", key), type = "vap")
  expect_equal(prefixed, plain)
})

test_that("block counts are normalized to the published shares at geo_smooth = 0", {
  key <- pop_block()
  raw <- geo_prior(block = key, type = "vap", geo_smooth = 0)
  d <- openBISG::geo_block_vap
  row <- d[d$geoid == key, ]
  expect_equal(as.numeric(raw),
               as.numeric(unlist(row[race_groups()])) / row$total,
               tolerance = 1e-12)
  expect_equal(attr(raw, "total"), row$total)
})

test_that("block-level smoothing follows the pseudo-count formula", {
  key <- pop_block()
  raw <- geo_prior(block = key, type = "vap", geo_smooth = 0)
  nat <- openBISG:::geo_national("block", "vap")
  tot <- attr(raw, "total")
  for (alpha in c(0.5, 1, 25)) {
    sm  <- geo_prior(block = key, type = "vap", geo_smooth = alpha)
    exp <- (tot * (raw / sum(raw)) + alpha * nat) / (tot + alpha)
    expect_equal(as.numeric(sm), as.numeric(exp), tolerance = 1e-12)
  }
})

test_that("an unpopulated block falls back to its block group, with a message", {
  bad <- gone_block()
  expect_message(
    p <- geo_prior(block = bad, type = "vap"),
    "not among the populated 2020 census blocks"
  )
  expect_false(is.null(p))
  expect_equal(attr(p, "level"), "block_group")
  expect_equal(attr(p, "fallback_from"), "block")
  bg <- geo_prior(block_group = parent_bg(), type = "vap")
  expect_equal(as.numeric(p), as.numeric(bg), tolerance = 1e-12)
  expect_equal(attr(p, "total"), attr(bg, "total"))
})

test_that("block_fallback = FALSE turns the miss into a silent NULL", {
  bad <- gone_block()
  expect_silent(p <- geo_prior(block = bad, type = "vap",
                               block_fallback = FALSE))
  expect_null(p)
  ## The populated-block path is unaffected by the switch.
  expect_equal(
    geo_prior(block = pop_block(), type = "vap", block_fallback = FALSE),
    geo_prior(block = pop_block(), type = "vap")
  )
})

test_that("block lookups under CVAP reroute to the parent block group", {
  key <- pop_block()
  expect_message(
    p <- geo_prior(block = key, type = "cvap"),
    "no block-level CVAP"
  )
  expect_equal(attr(p, "level"), "block_group")
  expect_equal(attr(p, "fallback_from"), "block")
  bg <- geo_prior(block_group = parent_bg(), type = "cvap")
  expect_equal(as.numeric(p), as.numeric(bg), tolerance = 1e-12)
})

test_that("malformed block GEOIDs return NULL and input validation covers block", {
  expect_null(geo_prior(block = "12345", type = "vap"))       # too short
  expect_null(geo_prior(block = "0100102010010000", type = "vap")) # 16 digits
  expect_error(geo_prior(block = pop_block(), block_group = parent_bg()),
               "at most one")
  expect_error(openBISG:::geo_table("block", "cvap"), "No block-level CVAP")
})

test_that("predict_race folds a block prior and reports fallback metadata", {
  key <- pop_block()
  pr <- predict_race(last = "Smith", block = key, geography_type = "vap")
  expect_true(pr$geography$found)
  expect_equal(pr$geography$level, "block")
  expect_null(pr$geography$fallback_from)
  expect_equal(
    as.numeric(pr$geography$probs),
    as.numeric(geo_prior(block = key, type = "vap")),
    tolerance = 1e-12
  )

  bad <- gone_block()
  expect_message(
    prb <- predict_race(last = "Smith", block = bad, geography_type = "vap"),
    "not among the populated"
  )
  expect_true(prb$geography$found)
  expect_equal(prb$geography$level, "block_group")
  expect_equal(prb$geography$fallback_from, "block")
  bg_ref <- predict_race(last = "Smith", block_group = parent_bg(),
                         geography_type = "vap")
  expect_equal(prb$geography$combined, bg_ref$geography$combined,
               tolerance = 1e-12)

  expect_silent(
    prn <- predict_race(last = "Smith", block = bad, geography_type = "vap",
                        block_fallback = FALSE)
  )
  expect_false(prn$geography$found)
  expect_equal(prn$geography$level, "block")
})

test_that("block predictions agree across all three entry points", {
  df <- data.frame(first = c("WEI", "Maria", "John"),
                   last  = c("NGUYEN", "Garcia", "Smith"),
                   block = c(pop_block(), pop_block(), gone_block()),
                   stringsAsFactors = FALSE)
  pn <- suppressMessages(predict_names(df, geography_type = "vap",
                                       progress = FALSE))
  pd <- suppressMessages(predict_demog(df, geography_type = "vap",
                                       progress = FALSE))
  expect_equal(pd, pn, tolerance = 1e-12)
  pr <- suppressMessages(
    predict_race(first = df$first[1], last = df$last[1],
                 block = df$block[1], geography_type = "vap")
  )$geography$combined
  expect_equal(as.numeric(pn[1, paste0("p_", race_groups())]),
               as.numeric(pr), tolerance = 1e-12)
})

test_that("predict_demog reports how many rows missed populated blocks", {
  df <- data.frame(last  = c("Smith", "Garcia", "Nguyen"),
                   block = c(pop_block(), gone_block(), gone_block()),
                   stringsAsFactors = FALSE)
  expect_message(
    out <- predict_demog(df, geography_type = "vap", progress = FALSE),
    "2 of 3 `block` row\\(s\\) were not matched to a populated 2020 census block"
  )
  ## The fallback rows equal an explicit block-group lookup.
  bg_df <- data.frame(last = df$last, block_group = parent_bg(),
                      stringsAsFactors = FALSE)
  bg_out <- predict_demog(bg_df, geography_type = "vap", progress = FALSE)
  expect_equal(out[2, ], bg_out[2, ], tolerance = 1e-12,
               ignore_attr = TRUE)
  expect_equal(out[3, ], bg_out[3, ], tolerance = 1e-12,
               ignore_attr = TRUE)
})

test_that("predict_demog with block_fallback = FALSE skips the geography fold", {
  df <- data.frame(last  = c("Smith", "Garcia"),
                   block = c(pop_block(), gone_block()),
                   stringsAsFactors = FALSE)
  expect_message(
    out <- predict_demog(df, geography_type = "vap", progress = FALSE,
                         block_fallback = FALSE),
    "`block_fallback = FALSE`, so no geography component"
  )
  ## Row 2 collapses to the name-only posterior.
  name_only <- predict_demog(df["last"], progress = FALSE)
  expect_equal(as.numeric(out[2, ]), as.numeric(name_only[2, ]),
               tolerance = 1e-12)
  ## Row 1 (populated block) still gets the geography fold.
  with_geo <- suppressMessages(
    predict_demog(df, geography_type = "vap", progress = FALSE)
  )
  expect_equal(as.numeric(out[1, ]), as.numeric(with_geo[1, ]),
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(as.numeric(out[1, ]),
                                as.numeric(name_only[1, ]))))
})

test_that("predict_demog under CVAP reroutes block rows to block groups", {
  df <- data.frame(last = c("Smith", "Garcia"),
                   block = c(pop_block(), pop_block()),
                   stringsAsFactors = FALSE)
  expect_message(
    out <- predict_demog(df, progress = FALSE),   # default cvap
    "no block-level CVAP"
  )
  bg_df <- data.frame(last = df$last, block_group = parent_bg(),
                      stringsAsFactors = FALSE)
  expect_equal(out, predict_demog(bg_df, progress = FALSE),
               tolerance = 1e-12)
})

test_that("predict_names summarizes block fallbacks once per call", {
  df <- data.frame(last  = c("Smith", "Garcia"),
                   block = c(pop_block(), gone_block()),
                   stringsAsFactors = FALSE)
  expect_message(
    predict_names(df, geography_type = "vap", progress = FALSE),
    "1 of 2 `block` row\\(s\\) were not matched to a populated 2020 census block"
  )
  expect_message(
    predict_names(df, progress = FALSE),          # default cvap
    "no block-level CVAP"
  )
})

test_that("empty block cells are ignored by the bookkeeping", {
  df <- data.frame(last  = c("Smith", "Garcia"),
                   block = c(pop_block(), NA),
                   stringsAsFactors = FALSE)
  expect_silent(
    out <- predict_demog(df, geography_type = "vap", progress = FALSE)
  )
  ## Row 2 has no geography: equals the name-only posterior.
  name_only <- predict_demog(df["last"], progress = FALSE)
  expect_equal(as.numeric(out[2, ]), as.numeric(name_only[2, ]),
               tolerance = 1e-12)
})