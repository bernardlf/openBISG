## geography_type = "pop": total-population (all ages, P.L. 94-171
## Table P2) priors at the tract / block-group / block levels, bundled
## as of 0.8.0 and built by data-raw/build_geo_pop.R.

six <- c("white", "black", "aian", "aapi", "nh_multi", "hispanic")

test_that("zcta + pop errors are informative", {
  expect_error(openBISG:::geo_table("zcta", "pop"),
               "No ZCTA-level total-population table")
})

test_that("block-level pop lookups normalize the published counts", {
  d <- get("geo_block_pop", envir = asNamespace("openBISG"))
  key <- d$geoid[1]
  raw <- geo_prior(block = key, type = "pop", geo_smooth = 0,
                   block_shrink = 0)
  row <- d[d$geoid == key, ]
  expect_equal(as.numeric(raw),
               as.numeric(unlist(row[race_groups()])) / row$total,
               tolerance = 1e-12)
  expect_equal(attr(raw, "total"), row$total)
  expect_equal(attr(raw, "type"), "pop")
})

test_that("bg and tract pop lookups match the aggregated tables", {
  bg <- get("geo_bg_pop", envir = asNamespace("openBISG"))
  ok <- which(bg$total > 0)[1]
  p <- geo_prior(block_group = bg$geoid[ok], type = "pop", geo_smooth = 0)
  expect_equal(as.numeric(p), as.numeric(unlist(bg[ok, six])),
               tolerance = 1e-12)
  tr <- get("geo_tract_pop", envir = asNamespace("openBISG"))
  ok <- which(tr$total > 0)[1]
  p <- geo_prior(tract = tr$geoid[ok], type = "pop", geo_smooth = 0)
  expect_equal(as.numeric(p), as.numeric(unlist(tr[ok, six])),
               tolerance = 1e-12)
})

test_that("pop block counts are consistent with the parent aggregates", {
  d <- get("geo_block_pop", envir = asNamespace("openBISG"))
  bg <- get("geo_bg_pop", envir = asNamespace("openBISG"))
  key <- substr(d$geoid[1], 1L, 12L)
  kids <- d[substr(d$geoid, 1L, 12L) == key, ]
  agg <- bg[bg$geoid == key, ]
  expect_equal(sum(kids$total), agg$total)
  expect_equal(as.numeric(colSums(kids[six]) / sum(kids$total)),
               as.numeric(unlist(agg[six])), tolerance = 1e-12)
})

test_that("block_shrink blends the parent block-group POP composition", {
  d <- get("geo_block_pop", envir = asNamespace("openBISG"))
  key <- d$geoid[1]
  raw <- geo_prior(block = key, type = "pop", geo_smooth = 0,
                   block_shrink = 0)
  bgp <- geo_prior(block_group = substr(key, 1L, 12L), type = "pop",
                   geo_smooth = 0)
  tot <- attr(raw, "total")
  lam <- 10
  sh <- geo_prior(block = key, type = "pop", geo_smooth = 0,
                  block_shrink = lam)
  exp <- (as.numeric(raw) * tot + lam * as.numeric(bgp)) / (tot + lam)
  expect_equal(as.numeric(sh), exp, tolerance = 1e-12)
})

test_that("predict_demog and predict_wru accept geography_type = 'pop'", {
  d <- get("geo_block_pop", envir = asNamespace("openBISG"))
  key <- d$geoid[1]
  df <- data.frame(last = "Garcia", block = key, stringsAsFactors = FALSE)
  pd <- predict_demog(df, geography_type = "pop", progress = FALSE)
  pr <- predict_race(last = "Garcia", block = key, geography_type = "pop")
  expect_equal(as.numeric(pd[1, paste0("p_", race_groups())]),
               as.numeric(pr$geography$combined), tolerance = 1e-12)
  ## wru fold by hand, pop basis
  wru5 <- function(row) {
    v <- c(row[["white"]], row[["black"]], row[["hispanic"]],
           row[["aapi"]], row[["aian"]] + row[["nh_multi"]])
    v / sum(v)
  }
  g5 <- wru5(as.list(d[d$geoid == key, ]))
  st <- substr(key, 1L, 2L)
  cs <- colSums(d[substr(d$geoid, 1L, 2L) == st, six])
  pr5 <- wru5(as.list(cs))
  ln <- openBISG::last_names
  n5 <- wru5(as.list(ln[ln$name == "GARCIA", ]))
  exp <- n5 * g5 / pr5
  exp <- exp / sum(exp)
  got <- predict_wru(df, geography_type = "pop", progress = FALSE)
  expect_equal(as.numeric(got), as.numeric(exp), tolerance = 1e-10)
})
