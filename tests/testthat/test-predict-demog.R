race_cols <- paste0("p_", race_groups())
all_cols  <- c(race_cols, "p_female")

test_that("predict_demog matches predict_names on bundled tables (race + sex)", {
  df <- data.frame(
    first  = c("Maria",  "John",  "Mary Ann", "Maria ZZZQQQNOTAREALNAME",
               NA,       "Jose",   NA),
    middle = c("Jose",    NA,      NA,         NA,
               NA,        "Luis",  NA),
    last   = c("Garcia", "Smith", "Johnson",  "Garcia",
               "Wang",    NA,     NA),
    maiden = c(NA,        NA,     "Lopez",     NA,
               NA,        NA,     NA),
    zcta   = c("30307",  "10001", "94110",    NA,
               "99999",   NA,     "30307"),
    stringsAsFactors = FALSE
  )
  ref <- predict_names(df, progress = FALSE)
  out <- predict_demog(df, progress = FALSE)
  expect_equal(names(out), all_cols)
  expect_equal(nrow(out), nrow(df))
  for (col in all_cols) {
    expect_equal(out[[col]], ref[[col]], tolerance = 1e-12, label = col)
  }
})

test_that("predict_demog include_sex = FALSE drops the p_female column", {
  df <- data.frame(first = "Maria", last = "Garcia",
                   stringsAsFactors = FALSE)
  out <- predict_demog(df, include_sex = FALSE, progress = FALSE)
  expect_equal(names(out), race_cols)
})

test_that("predict_demog parallel path matches the serial path", {
  skip_on_os("windows")  # fork unavailable; n_cores falls back to 1
  df <- data.frame(
    first = c("Maria",  "John",  "Mary Ann", "Aiden",  "Sofia"),
    last  = c("Garcia", "Smith", "Johnson",  "Wong",   "Lopez"),
    zcta  = c("30307",  "10001", "94110",    "94110",  "30307"),
    stringsAsFactors = FALSE
  )
  serial   <- predict_demog(df, progress = FALSE)
  parallel <- predict_demog(df, progress = FALSE, n_cores = 2L)
  expect_equal(parallel, serial)
})

test_that("predict_demog errors on invalid n_cores", {
  df <- data.frame(first = "Maria", stringsAsFactors = FALSE)
  expect_error(predict_demog(df, n_cores = 0L,    progress = FALSE),
               "positive integer")
  expect_error(predict_demog(df, n_cores = "two", progress = FALSE),
               "positive integer")
})

test_that("predict_demog matches predict_names with tract / block_group / vap", {
  df <- data.frame(
    first       = c("Maria", "John"),
    last        = c("Garcia", "Smith"),
    tract       = c("01001020100", "01001020100"),
    stringsAsFactors = FALSE
  )
  ref <- predict_names(df, progress = FALSE, geography_type = "vap")
  out <- predict_demog(df, geography_type = "vap", progress = FALSE)
  for (col in race_cols) {
    expect_equal(out[[col]], ref[[col]], tolerance = 1e-12, label = col)
  }

  df2 <- data.frame(first = "Maria", last = "Garcia",
                    Block_Group = "010010201001", ZCTA = "30307",
                    stringsAsFactors = FALSE)
  ref2 <- predict_names(df2, progress = FALSE)
  out2 <- predict_demog(df2, progress = FALSE)
  for (col in race_cols) {
    expect_equal(out2[[col]], ref2[[col]], tolerance = 1e-12, label = col)
  }
})

test_that("predict_demog matches predict_names with include_extra", {
  fe <- openBISG:::table_df("first_extra")$name[1]
  le <- openBISG:::table_df("last_extra")$name[1]
  df <- data.frame(first = c("Maria", fe),
                   last  = c("Garcia", le),
                   stringsAsFactors = FALSE)
  ref <- predict_names(df, include_extra = TRUE, progress = FALSE)
  out <- predict_demog(df, include_extra = TRUE, progress = FALSE)
  for (col in race_cols) {
    expect_equal(out[[col]], ref[[col]], tolerance = 1e-12, label = col)
  }
})

test_that("predict_demog geography-only input returns P(R | G)", {
  df <- data.frame(zcta = c("30307", "99999"), stringsAsFactors = FALSE)
  out <- predict_demog(df, progress = FALSE)
  gp <- geo_prior(zcta = "30307")
  expect_equal(as.numeric(out[1, race_cols]), as.numeric(gp[race_groups()]),
               tolerance = 1e-12)
  ## No first-name field: p_female is NA, as in predict_names().
  expect_true(all(is.na(out[2, ])))
  expect_true(all(is.na(out$p_female)))
})

test_that("predict_demog with the bundled sex table reproduces predict_sex", {
  df <- data.frame(first = c("Michael", "Maria Jose", "ZZZQQQNOTAREALNAME"),
                   stringsAsFactors = FALSE)
  out <- predict_demog(df, name_dict = list(first = openBISG::first_names_sex),
                       progress = FALSE)
  expect_equal(names(out), c("p_male", "p_female"))
  expect_equal(out$p_female[1],
               unname(predict_sex("Michael")$probs[["female"]]),
               tolerance = 1e-12)
  expect_equal(out$p_female[2],
               unname(predict_sex("Maria Jose")$probs[["female"]]),
               tolerance = 1e-12)
  expect_true(is.na(out$p_female[3]))
})

test_that("custom name_dict + geo_dict with custom categories combine", {
  nd <- data.frame(name  = c("ALICE", "BOB"),
                   urban = c(0.8, 0.3),
                   rural = c(0.2, 0.7),
                   stringsAsFactors = FALSE)
  gd <- data.frame(geoid = c("A1", "B2"),
                   urban = c(0.9, 0.2),
                   rural = c(0.1, 0.8),
                   total = c(100L, 200L),
                   stringsAsFactors = FALSE)
  df <- data.frame(first = c("Alice", "Bob", "Carol"),
                   geoid = c("A1",    "B2",  "A1"),
                   stringsAsFactors = FALSE)
  pr <- c(urban = 0.5, rural = 0.5)
  out <- predict_demog(df, name_dict = nd, geo_dict = gd, prior = pr,
                       progress = FALSE)
  expect_equal(names(out), c("p_urban", "p_rural"))
  ## Row 1: 0.8*0.9/0.5 vs 0.2*0.1/0.5, renormalized.
  expect_equal(out$p_urban[1], (0.8 * 0.9) / (0.8 * 0.9 + 0.2 * 0.1),
               tolerance = 1e-12)
  ## Row 2: Bob in B2.
  expect_equal(out$p_urban[2], (0.3 * 0.2) / (0.3 * 0.2 + 0.7 * 0.8),
               tolerance = 1e-12)
  ## Row 3: Carol misses the dictionary -> geography-only prediction.
  expect_equal(out$p_urban[3], 0.9, tolerance = 1e-12)

  ## Without `prior`, the total-weighted geo prior is derived (no warning
  ## needed for k = 1 rows, but the fold uses it).
  out2 <- predict_demog(df, name_dict = nd, geo_dict = gd, progress = FALSE)
  expect_equal(names(out2), c("p_urban", "p_rural"))
  expect_false(any(is.na(out2$p_urban)))
})

test_that("custom name_dict alone uses its own categories; rows renormalized", {
  nd <- data.frame(name = c("ALICE", "BOB"),
                   cat  = c(2, 1),     # unnormalized rows
                   dog  = c(2, 3),
                   stringsAsFactors = FALSE)
  out <- predict_demog(data.frame(first = c("alice", "bob")),
                       name_dict = nd, progress = FALSE)
  expect_equal(names(out), c("p_cat", "p_dog"))
  expect_equal(out$p_cat, c(0.5, 0.25), tolerance = 1e-12)
})

test_that("custom dictionaries get the same matching cascade", {
  nd <- data.frame(name = c("PENA", "OCONNOR", "MARYANN"),
                   a = c(0.9, 0.2, 0.7), b = c(0.1, 0.8, 0.3),
                   stringsAsFactors = FALSE)
  out <- predict_demog(
    data.frame(first = c("PEÑA", "O'Connor", "Mary Ann")),
    name_dict = nd, progress = FALSE
  )
  expect_equal(out$p_a, c(0.9, 0.2, 0.7), tolerance = 1e-12)
})

test_that("category mismatches raise informative errors", {
  nd <- data.frame(name = "ALICE", urban = 0.8, rural = 0.2,
                   stringsAsFactors = FALSE)
  gd <- data.frame(geoid = "A1", young = 0.5, old = 0.5,
                   stringsAsFactors = FALSE)
  ## Custom name dict + bundled geography (zcta column, no geo_dict).
  expect_error(
    predict_demog(data.frame(first = "Alice", zcta = "30307"),
                  name_dict = nd),
    "categories"
  )
  ## Custom geo dict + bundled name tables.
  expect_error(
    predict_demog(data.frame(first = "Maria", geoid = "A1"),
                  geo_dict = gd),
    "categories"
  )
  ## Custom name dict + custom geo dict with different categories.
  expect_error(
    predict_demog(data.frame(first = "Alice", geoid = "A1"),
                  name_dict = nd, geo_dict = gd),
    "categories"
  )
  ## first / last dictionaries with different categories.
  expect_error(
    predict_demog(
      data.frame(first = "Alice", last = "Bob"),
      name_dict = list(
        first = nd,
        last  = data.frame(name = "BOB", young = 0.5, old = 0.5,
                           stringsAsFactors = FALSE)
      )
    ),
    "same\\s+category"
  )
})

test_that("geoid column without geo_dict errors; include_extra warns with name_dict", {
  expect_error(predict_demog(data.frame(geoid = "A1")), "geo_dict")
  nd <- data.frame(name = "ALICE", a = 0.5, b = 0.5,
                   stringsAsFactors = FALSE)
  expect_warning(
    predict_demog(data.frame(first = "Alice"), name_dict = nd,
                  include_extra = TRUE, progress = FALSE),
    "include_extra"
  )
})

test_that("uniform-prior fallback warns only when the prior matters", {
  nd <- data.frame(name = c("ALICE", "BOB"),
                   a = c(0.8, 0.3), b = c(0.2, 0.7),
                   stringsAsFactors = FALSE)
  ## Single evidence piece, no geography: prior never enters -> no warning.
  expect_silent(predict_demog(data.frame(first = "Alice"), name_dict = nd,
                              progress = FALSE))
  ## Two evidence pieces (first + last): prior enters -> warning.
  expect_warning(
    predict_demog(data.frame(first = "Alice", last = "Bob"),
                  name_dict = nd, progress = FALSE),
    "uniform prior"
  )
})

test_that("predict_demog input validation matches predict_names", {
  expect_error(predict_demog(list(a = 1)), "data.frame")
  expect_error(predict_demog(data.frame(foo = 1)), "No recognized columns")
})
