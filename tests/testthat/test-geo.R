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
