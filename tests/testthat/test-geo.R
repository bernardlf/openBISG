test_that("geo_prior accepts plain and Summary-File-prefixed tract GEOIDs", {
  plain <- geo_prior(tract = "01001020100")
  expect_false(is.null(plain))
  expect_named(plain, race_groups())
  expect_equal(sum(plain), 1, tolerance = 1e-6)
  prefixed <- geo_prior(tract = "1400000US01001020100")
  expect_equal(prefixed, plain)
})

test_that("geo_prior accepts plain and Summary-File-prefixed block-group GEOIDs", {
  plain <- geo_prior(block_group = "010010201001")
  expect_false(is.null(plain))
  prefixed <- geo_prior(block_group = "1500000US010010201001")
  expect_equal(prefixed, plain)
})

test_that("geo_prior zero-pads short ZIPs and rejects malformed IDs", {
  expect_equal(geo_prior(zcta = 601), geo_prior(zcta = "00601"))
  expect_null(geo_prior(tract = "12345"))          # too short
  expect_null(geo_prior(block_group = "12345"))    # too short
  expect_null(geo_prior(zcta = "99999"))           # well-formed, not in table
})

test_that("geo_prior errors when more than one geography is supplied", {
  expect_error(geo_prior(zcta = "30307", tract = "01001020100"),
               "at most one")
})

test_that("geo_prior with no geography returns the national prior", {
  out <- geo_prior()
  expect_named(out, race_groups())
  expect_equal(attr(out, "level"), "national")
  expect_equal(sum(out), 1, tolerance = 1e-6)
})

## ---- Pseudo-count smoothing of the geographic prior -----------------

## Half of `geo_bg_cvap` estimates no Asian / NHPI citizens age 18+, so
## the pathology these tests cover is a property of the table rather
## than of any one row. Pick a qualifying block group by that property
## instead of hard-coding a GEOID.
zero_aapi_bg <- function() {
  d <- openBISG::geo_bg_cvap
  d$geoid[which(d$aapi == 0 & d$total > 100)][1]
}

test_that("geo_smooth = 0 reproduces the published shares", {
  key <- zero_aapi_bg()
  raw <- geo_prior(block_group = key, geo_smooth = 0)
  tbl <- openBISG::geo_bg_cvap
  row <- tbl[tbl$geoid == key, race_groups()]
  expect_equal(as.numeric(raw), as.numeric(unlist(row)), tolerance = 1e-12)
  expect_equal(attr(raw, "geo_smooth"), 0)
  expect_equal(raw[["aapi"]], 0)
})

test_that("geo_smooth lifts sampling zeros without disturbing populated cells", {
  key <- zero_aapi_bg()
  raw <- geo_prior(block_group = key, geo_smooth = 0)
  sm  <- geo_prior(block_group = key)
  expect_equal(attr(sm, "geo_smooth"), 1)
  expect_named(sm, race_groups())
  expect_equal(sum(sm), 1, tolerance = 1e-12)
  ## Zero cells become small but positive; the fold can no longer
  ## annihilate a group the names point to.
  expect_gt(sm[["aapi"]], 0)
  expect_gt(sm[["aian"]], 0)
  expect_lt(sm[["aapi"]], 1e-2)
  ## A populated cell barely moves under a one-person pseudo-count.
  expect_equal(sm[["white"]], raw[["white"]], tolerance = 1e-2)
})

test_that("geo_smooth follows the pseudo-count formula and scales with alpha", {
  key <- zero_aapi_bg()
  raw <- geo_prior(block_group = key, geo_smooth = 0)
  nat <- openBISG:::geo_national("block_group", "cvap")
  tot <- attr(geo_prior(block_group = key), "total")
  for (alpha in c(0.5, 1, 25)) {
    sm  <- geo_prior(block_group = key, geo_smooth = alpha)
    exp <- (tot * (raw / sum(raw)) + alpha * nat) / (tot + alpha)
    expect_equal(as.numeric(sm), as.numeric(exp), tolerance = 1e-12)
  }
  ## A larger pseudo-count pulls further toward the national marginal.
  a1  <- geo_prior(block_group = key, geo_smooth = 1)[["aapi"]]
  a25 <- geo_prior(block_group = key, geo_smooth = 25)[["aapi"]]
  expect_gt(a25, a1)
  expect_lt(a25, nat[["aapi"]])
})

test_that("geo_smooth is validated", {
  expect_error(geo_prior(zcta = "30307", geo_smooth = -1), "non-negative")
  expect_error(geo_prior(zcta = "30307", geo_smooth = c(1, 2)), "single")
  expect_error(geo_prior(zcta = "30307", geo_smooth = "1"), "non-negative")
  expect_error(geo_prior(zcta = "30307", geo_smooth = NA_real_), "non-negative")
  expect_error(predict_race(last = "Smith", zcta = "30307", geo_smooth = -1),
               "non-negative")
  expect_error(predict_names(data.frame(last = "Smith", zcta = "30307"),
                             geo_smooth = -1, progress = FALSE),
               "non-negative")
  expect_error(predict_demog(data.frame(last = "Smith", zcta = "30307"),
                             geo_smooth = -1, progress = FALSE),
               "non-negative")
})

test_that("a sampling zero no longer annihilates decisive name evidence", {
  ## Regression: three given / surname tokens that are each ~95-98%
  ## AAPI in the Census tables, in a block group whose published CVAP
  ## has aapi = 0. The zero drove P(aapi) to exactly 0 and dumped the
  ## displaced mass on nh_multi -- the surviving group with the
  ## smallest marginal prior, and so the largest 1/P(R) boost.
  args <- list(first = "WEI", middle = "MINH", last = "NGUYEN",
               block_group = zero_aapi_bg())
  name_only <- do.call(predict_race, args[1:3])$combined$probs
  expect_gt(name_only[["aapi"]], 0.99)

  old <- do.call(predict_race, c(args, geo_smooth = 0))$geography$combined
  expect_equal(old[["aapi"]], 0)
  expect_gt(old[["nh_multi"]], 0.9)

  new <- do.call(predict_race, args)$geography$combined
  expect_gt(new[["aapi"]], 0.99)
  expect_lt(new[["nh_multi"]], 0.01)
  expect_equal(sum(new), 1, tolerance = 1e-12)
})

test_that("smoothing is applied identically by all three entry points", {
  df <- data.frame(first = c("WEI", "Maria", "John"),
                   middle = c("MINH", "", ""),
                   last  = c("NGUYEN", "Garcia", "Smith"),
                   block_group = c(zero_aapi_bg(), "010010201001",
                                   "010010201001"),
                   stringsAsFactors = FALSE)
  for (alpha in c(0, 1, 10)) {
    pn <- predict_names(df, geo_smooth = alpha, progress = FALSE)
    pd <- predict_demog(df, geo_smooth = alpha, progress = FALSE)
    expect_equal(pd, pn, tolerance = 1e-12)
    ## predict_race() is the per-row reference for both.
    pr <- predict_race(first = df$first[1], middle = df$middle[1],
                       last = df$last[1], block_group = df$block_group[1],
                       geo_smooth = alpha)$geography$combined
    expect_equal(as.numeric(pn[1, paste0("p_", race_groups())]),
                 as.numeric(pr), tolerance = 1e-12)
  }
})

test_that("geography_probs is used exactly as supplied, unsmoothed", {
  g <- c(white = 0.4, black = 0.2, aian = 0.01,
         aapi = 0.1, nh_multi = 0.04, hispanic = 0.25)
  a <- predict_race(last = "Smith", geography_probs = g, geo_smooth = 1)
  b <- predict_race(last = "Smith", geography_probs = g, geo_smooth = 0)
  expect_equal(a$geography$combined, b$geography$combined, tolerance = 1e-12)
  expect_equal(a$geography$probs, g / sum(g), tolerance = 1e-12)
  expect_equal(a$geography$geo_smooth, 0)
})

test_that("a geo_dict without a total column is left unsmoothed", {
  nd <- data.frame(name = c("ALICE", "BOB"),
                   urban = c(0.8, 0.3), rural = c(0.2, 0.7),
                   stringsAsFactors = FALSE)
  gd <- data.frame(geoid = c("A1", "B2"),
                   urban = c(0.9, 0.2), rural = c(0.1, 0.8),
                   stringsAsFactors = FALSE)
  df <- data.frame(first = c("Alice", "Bob"), geoid = c("A1", "B2"),
                   stringsAsFactors = FALSE)
  pr <- c(urban = 0.5, rural = 0.5)
  expect_equal(
    predict_demog(df, name_dict = nd, geo_dict = gd, prior = pr,
                  progress = FALSE),
    predict_demog(df, name_dict = nd, geo_dict = gd, prior = pr,
                  geo_smooth = 0, progress = FALSE),
    tolerance = 1e-12
  )
})
