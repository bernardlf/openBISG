test_that("normalize_name strips diacritics, trims, and uppercases", {
  expect_equal(normalize_name("  Peña  "), "PENA")
  expect_equal(normalize_name("garcía"),     "GARCIA")
  expect_equal(normalize_name("o'connor"),        "O'CONNOR")
  expect_equal(normalize_name("van der berg"),    "VAN DER BERG")
})

test_that("normalize_name handles empty and missing input", {
  expect_equal(normalize_name(""),     "")
  expect_equal(normalize_name("   "),  "")
  expect_equal(normalize_name(NA_character_), "")
  expect_equal(normalize_name(NULL),  "")
})

test_that("strip_last_name_suffix mirrors the JS rules", {
  f <- openBISG:::strip_last_name_suffix
  expect_equal(f("SMITHJR"),     "SMITH")
  expect_equal(f("SMITHII"),     "SMITH")
  expect_equal(f("SMITHIII"),    "SMITH")
  expect_equal(f("SMITHIV"),     "SMITH")
  expect_equal(f("SMITHJUNIOR"), "SMITH")
  expect_equal(f("SMITHSENIOR"), "SMITH")
  expect_equal(f("SMITHTHIRD"),  "SMITH")

  ## SR is only stripped on names of length >= 7
  expect_equal(f("WILSONSR"), "WILSON")  # 8 chars, stripped
  expect_equal(f("USR"),      "USR")     # too short, untouched

  ## Plain names untouched
  expect_equal(f("SMITH"),    "SMITH")
})
