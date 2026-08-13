## predict_wru(): wru-replication mode — five-category surname x
## geography fold from the bundled tables, state-marginal prior, raw
## counts (no smoothing), no block-group fallback.

wru5 <- function(row) {
  v <- c(whi = row[["white"]], bla = row[["black"]], his = row[["hispanic"]],
         asi = row[["aapi"]], oth = row[["aian"]] + row[["nh_multi"]])
  v / sum(v)
}

six <- c("white", "black", "aian", "aapi", "nh_multi", "hispanic")

first_block  <- function() openBISG::geo_block_vap$geoid[1]
block_state  <- function(key) substr(key, 1L, 2L)
.gone2_cache <- new.env(parent = emptyenv())
absent_block <- function() {
  if (!is.null(.gone2_cache$id)) return(.gone2_cache$id)
  bg <- substr(first_block(), 1L, 12L)
  taken <- substr(grep(paste0("^", bg), openBISG::geo_block_vap$geoid,
                       value = TRUE), 13L, 15L)
  free <- setdiff(sprintf("%03d", 999:0), taken)
  if (length(free) == 0L) skip("every block under the first bg is populated")
  .gone2_cache$id <- paste0(bg, free[1])
  .gone2_cache$id
}

state_prior_block <- function(st) {
  d <- openBISG::geo_block_vap
  cs <- colSums(d[substr(d$geoid, 1L, 2L) == st, six])
  wru5(as.list(cs))
}

test_that("predict_wru reproduces the wru fold by hand at the block level", {
  key <- first_block()
  d <- openBISG::geo_block_vap
  g5 <- wru5(as.list(d[d$geoid == key, ]))
  pr5 <- state_prior_block(block_state(key))
  ln <- openBISG::last_names
  n5 <- wru5(as.list(ln[ln$name == "GARCIA", ]))
  exp <- n5 * g5 / pr5
  exp <- exp / sum(exp)
  got <- predict_wru(data.frame(last = "Garcia", block = key,
                                stringsAsFactors = FALSE),
                     geography_type = "vap", progress = FALSE)
  expect_named(got, paste0("p_", c("whi", "bla", "his", "asi", "oth")))
  expect_equal(as.numeric(got), as.numeric(exp), tolerance = 1e-10)
})

test_that("an unmatched surname carries the geography-only posterior", {
  key <- first_block()
  d <- openBISG::geo_block_vap
  g5 <- wru5(as.list(d[d$geoid == key, ]))
  got <- predict_wru(data.frame(last = "Zzqxvqy", block = key,
                                stringsAsFactors = FALSE),
                     geography_type = "vap", progress = FALSE)
  expect_equal(as.numeric(got), as.numeric(g5), tolerance = 1e-10)
})

test_that("an unmatched block carries the surname-only posterior, with a message", {
  bad <- absent_block()
  ln <- openBISG::last_names
  n5 <- wru5(as.list(ln[ln$name == "GARCIA", ]))
  expect_message(
    got <- predict_wru(data.frame(last = "Garcia", block = bad,
                                  stringsAsFactors = FALSE),
                       geography_type = "vap", progress = FALSE),
    "did not match the bundled block table"
  )
  expect_equal(as.numeric(got), as.numeric(n5), tolerance = 1e-10)
})

test_that("the `surname` alias and the tract level work", {
  d <- openBISG::geo_tract_vap
  ok <- which(is.finite(d$total) & d$total > 0 &
                is.finite(rowSums(d[, six])) & rowSums(d[, six]) > 0)[1]
  key <- d$geoid[ok]
  st  <- substr(key, 1L, 2L)
  g5  <- wru5(as.list(d[ok, ]))
  sub <- d[substr(d$geoid, 1L, 2L) == st, ]
  pm  <- as.matrix(sub[, six])
  w   <- suppressWarnings(as.numeric(sub$total))
  rs  <- rowSums(pm)
  use <- is.finite(w) & w > 0 & is.finite(rs) & rs > 0
  cs  <- colSums(pm[use, , drop = FALSE] / rs[use] * w[use])
  pr5 <- wru5(as.list(cs))
  ln <- openBISG::last_names
  n5 <- wru5(as.list(ln[ln$name == "NGUYEN", ]))
  exp <- n5 * g5 / pr5
  exp <- exp / sum(exp)
  got <- predict_wru(data.frame(surname = "Nguyen", tract = key,
                                stringsAsFactors = FALSE),
                     geography_type = "vap", progress = FALSE)
  expect_equal(as.numeric(got), as.numeric(exp), tolerance = 1e-10)
})

test_that("multi-state input equals per-state calls", {
  d <- openBISG::geo_block_vap
  k1 <- d$geoid[1]
  st1 <- substr(k1, 1L, 2L)
  k2 <- d$geoid[match(TRUE, substr(d$geoid, 1L, 2L) != st1)]
  df <- data.frame(last = c("Garcia", "Washington"),
                   block = c(k1, k2), stringsAsFactors = FALSE)
  both <- predict_wru(df, progress = FALSE)
  one <- predict_wru(df[1, , drop = FALSE], progress = FALSE)
  two <- predict_wru(df[2, , drop = FALSE], progress = FALSE)
  expect_equal(as.numeric(both[1, ]), as.numeric(one), tolerance = 1e-12)
  expect_equal(as.numeric(both[2, ]), as.numeric(two), tolerance = 1e-12)
  expect_equal(unname(rowSums(both)), c(1, 1), tolerance = 1e-12)
})

test_that("input validation and wru-mode restrictions", {
  key <- first_block()
  expect_error(predict_wru(data.frame(x = 1)), "surname column")
  expect_error(predict_wru(data.frame(last = "Smith")),
               "block.*block_group.*tract")
  expect_error(predict_wru(data.frame(last = "Smith", block = key,
                                      stringsAsFactors = FALSE),
                           geography_type = "cvap"),
               "No block-level CVAP")
  expect_error(predict_wru("nope"), "data.frame")
})

test_that("geo_smooth departs from raw-count wru behavior only when asked", {
  ## A block-group row with a zero cell: smoothing lifts the zero.
  d <- openBISG::geo_bg_vap
  ok <- which(is.finite(d$total) & d$total > 0 & !is.na(d$aapi) &
                (d$aapi + d$aian + d$nh_multi) == 0 & !is.na(d$white) &
                d$white > 0)[1]
  skip_if(is.na(ok), "no zero-oth/asi block group found")
  key <- d$geoid[ok]
  df <- data.frame(last = "Nguyen", block_group = key,
                   stringsAsFactors = FALSE)
  raw <- predict_wru(df, geography_type = "vap", progress = FALSE)
  expect_equal(raw$p_asi, 0)   # wru behavior: zero cell wins
  sm <- predict_wru(df, geography_type = "vap", geo_smooth = 1,
                    progress = FALSE)
  expect_gt(sm$p_asi, 0)
})

test_that("the default basis is pop — wru's own", {
  key <- first_block()
  df <- data.frame(last = "Garcia", block = key, stringsAsFactors = FALSE)
  expect_equal(predict_wru(df, progress = FALSE),
               predict_wru(df, geography_type = "pop", progress = FALSE),
               tolerance = 1e-12)
  ## pop and vap priors genuinely differ for this block
  expect_false(isTRUE(all.equal(
    predict_wru(df, progress = FALSE),
    predict_wru(df, geography_type = "vap", progress = FALSE))))
})
