test_that("exact-match lookup hits the expected first-name row", {
  hit <- lookup_name("MICHAEL", table = "first")
  expect_type(hit, "list")
  expect_equal(hit$matched_as, "MICHAEL")
  expect_equal(hit$rule, "exact")
  expect_named(hit$probs, race_groups())
  expect_equal(sum(hit$probs), 1, tolerance = 1e-6)
  ## MICHAEL is the most common first name; sanity-check the headline value.
  expect_gt(unname(hit$probs["white"]), 0.5)
})

test_that("case-insensitive and trimmed lookup works", {
  expect_equal(lookup_name("  michael  ", "first")$matched_as, "MICHAEL")
})

test_that("diacritics are stripped before matching", {
  ## PEÑA appears in the last-name table as PENA.
  hit <- lookup_name("PEÑA", "last")
  expect_equal(hit$matched_as, "PENA")
  expect_equal(hit$rule, "exact")
})

test_that("punctuation rule fires for O'CONNOR", {
  hit <- lookup_name("O'CONNOR", "last")
  expect_equal(hit$rule, "punctuation removed")
  expect_equal(hit$matched_as, "OCONNOR")
})

test_that("generational suffix removed for last names only", {
  hit <- lookup_name("SMITH JR", "last")
  expect_equal(hit$rule, "generational suffix removed")
  expect_equal(hit$matched_as, "SMITH")
})

test_that("multi-part name falls back through the cascade", {
  ## GARCIALOPEZ exists in the table, so step 2 ("punctuation removed")
  ## fires before step 5 (segment split) gets a chance.
  hit <- lookup_name("GARCIA-LOPEZ", "last")
  expect_true(hit$rule %in% c(
    "exact",
    "punctuation removed",
    "punctuation and spaces removed",
    "first segment of multi-part name"
  ))
  expect_true(nzchar(hit$matched_as))
})

test_that("non-existent name returns NULL", {
  expect_null(lookup_name("ZZZQQQNOTAREALNAME", "first"))
  expect_null(lookup_name("",   "first"))
  expect_null(lookup_name(NA_character_, "first"))
})

test_that("tokenize_names splits whitespace but preserves hyphens", {
  expect_equal(tokenize_names("  Maria  Jose  "), c("Maria", "Jose"))
  expect_equal(tokenize_names("Maria-Jose"),       "Maria-Jose")
  expect_equal(tokenize_names(c("Maria", "Jose")), c("Maria", "Jose"))
  expect_equal(tokenize_names(NULL),               character(0))
  expect_equal(tokenize_names(""),                 character(0))
  expect_equal(tokenize_names(NA_character_),      character(0))
})

test_that("lookup_with_fallback returns the table that matched", {
  ## SCHMIDT is in the last-names table but not the first-names table.
  ## With (first, last) the fallback should hit last.
  hit <- lookup_with_fallback("SCHMIDT", tables = c("first", "last"))
  expect_equal(hit$source, "last")
  ## With (last, first) the primary should hit first.
  hit2 <- lookup_with_fallback("SCHMIDT", tables = c("last", "first"))
  expect_equal(hit2$source, "last")
  ## A name in both: primary wins.
  hit3 <- lookup_with_fallback("SMITH", tables = c("first", "last"))
  expect_equal(hit3$source, "first")
})

test_that("predict_race for first-only matches the single-name row", {
  pred <- predict_race(first = "MARIA")
  expect_named(pred, c("groups", "group_labels", "tokens", "surname_used",
                       "combined", "geography", "sex"))
  expect_named(pred$tokens, c("first", "middle", "last", "maiden"))
  expect_length(pred$tokens$first, 1)
  expect_length(pred$tokens$last,  0)
  expect_equal(pred$tokens$first[[1]]$matched_as, "MARIA")
  expect_null(pred$surname_used)
  ## With one matched token the combined probability is just the row.
  expect_equal(pred$combined$probs,
               pred$tokens$first[[1]]$probs, tolerance = 1e-12)
  expect_equal(pred$combined$n, 1L)
  expect_false(is.null(pred$sex))
  expect_equal(pred$sex$n, 1L)
})

test_that("predict_race(first = MARIA, last = GARCIA) sums to 1 and skews Hispanic", {
  pred <- predict_race(first = "MARIA", last = "GARCIA")
  expect_named(pred$combined, c("probs", "n"))
  expect_equal(pred$combined$n, 2L)
  expect_equal(pred$surname_used, "last")
  expect_equal(sum(pred$combined$probs), 1, tolerance = 1e-6)
  expect_gt(unname(pred$combined$probs["hispanic"]), 0.8)
})

test_that("compound first name 'Maria Jose' matches MARIAJOSE as one token", {
  pred <- predict_race(first = "Maria Jose")
  expect_equal(pred$combined$n, 1L)
  expect_equal(names(pred$tokens$first), "Maria Jose")
  ## MARIAJOSE is overwhelmingly female (~0.996) — the compound row
  ## carries direct sex info that the JOSE-vs-MARIA combination would
  ## have nearly cancelled to 50/50.
  expect_gt(unname(pred$sex$probs["female"]), 0.9)
})

test_that("MARYANN matches as compound from 'Mary Ann' input", {
  pred <- predict_race(first = "Mary Ann")
  expect_equal(pred$combined$n, 1L)
  expect_equal(pred$tokens$first[[1]]$matched_as, "MARYANN")
})

test_that("compound miss falls back to per-token combination", {
  ## MARYANNMARIE is not in the dictionary; should split into 3 tokens.
  pred <- predict_race(first = "Mary Ann Marie")
  expect_equal(pred$combined$n, 3L)
  expect_equal(names(pred$tokens$first), c("Mary", "Ann", "Marie"))
})

test_that("middle-name field contributes to race but is excluded from sex", {
  pred <- predict_race(first = "Maria", middle = "Jose")
  expect_equal(pred$combined$n, 2L)
  expect_equal(pred$sex$n, 1L)
  expect_named(pred$sex$tokens, "first")
  expect_gt(unname(pred$sex$probs["female"]), 0.99)
})

test_that("maiden replaces last in the combined estimate when both supplied", {
  pred <- predict_race(first = "Maria", last = "Smith", maiden = "Garcia")
  expect_equal(pred$surname_used, "maiden")
  expect_equal(pred$combined$n, 2L)  # MARIA + GARCIA, not SMITH
  expect_gt(unname(pred$combined$probs["hispanic"]), 0.9)
  ## last is still looked up and reported, just unused.
  expect_false(is.null(pred$tokens$last[["Smith"]]))
})

test_that("maiden alone (no last) feeds the combination", {
  pred <- predict_race(first = "Maria", maiden = "Garcia")
  expect_equal(pred$surname_used, "maiden")
  expect_equal(pred$combined$n, 2L)
  expect_gt(unname(pred$combined$probs["hispanic"]), 0.9)
})

test_that("last-only with no maiden uses last as the surname source", {
  pred <- predict_race(first = "Maria", last = "Garcia")
  expect_equal(pred$surname_used, "last")
})

test_that("multi-token combination uses (n-1) prior division across all 4 fields", {
  ## first=Maria, middle=Jose (separate evidence pieces — no compound),
  ## last=Garcia Lopez (two surname tokens). 4 evidence pieces total.
  pred <- predict_race(first = "Maria", middle = "Jose",
                       last  = "Garcia Lopez")
  expect_equal(pred$combined$n, 4L)
  expect_equal(sum(pred$combined$probs), 1, tolerance = 1e-6)

  ## Manual check against the formula prod(probs) / prior^(k-1).
  hits <- c(pred$tokens$first, pred$tokens$middle, pred$tokens$last)
  prob_list <- lapply(hits, `[[`, "probs")
  prior     <- attr(openBISG:::table_df("last"), "prior")
  manual    <- Reduce(`*`, prob_list) / prior^(length(hits) - 1L)
  manual    <- manual / sum(manual)
  expect_equal(unname(pred$combined$probs),
               unname(manual), tolerance = 1e-12)
})

test_that("missing tokens are reported but excluded from the combination", {
  pred <- predict_race(first = "MARIA ZZZQQQNOTAREALNAME",
                       last  = "GARCIA")
  ## The compound 'MARIA ZZZQQQNOTAREALNAME' won't match — falls back
  ## to per-token. MARIA hits, ZZZQQQ misses entirely.
  expect_length(pred$tokens$first, 2)
  expect_false(is.null(pred$tokens$first[["MARIA"]]))
  expect_null(pred$tokens$first[["ZZZQQQNOTAREALNAME"]])
  expect_equal(pred$combined$n, 2L)  # MARIA + GARCIA, not the missing one
})

test_that("predict_sex with compound first name reads MARIAJOSE row directly", {
  pred <- predict_sex("Maria Jose")
  expect_equal(pred$n, 1L)
  expect_gt(unname(pred$probs["female"]), 0.9)
})

test_that("first-name token absent from first table falls back to last table", {
  ## SCHMIDT is in the last-name table but not the first-name table.
  ## As a given-name token it should fall back to the last-name table.
  pred <- predict_race(first = "SCHMIDT")
  expect_equal(pred$tokens$first[["SCHMIDT"]]$source, "last")
})

test_that("predict_race errors when no token is provided", {
  expect_error(predict_race(),                              "at least one")
  expect_error(predict_race(first = "", last = NULL),       "at least one")
  expect_error(predict_race(first = "   ", last = "  "),    "at least one")
})

test_that("predict_sex single name returns the row directly", {
  pred <- predict_sex("Michael")
  expect_named(pred$probs, sex_groups())
  expect_gt(unname(pred$probs["male"]), 0.9)
})

test_that("predict_sex with a single first-name token equals the table row", {
  pred <- predict_sex("MICHAEL")
  expect_equal(unname(pred$probs),
               unname(pred$tokens$first[["MICHAEL"]]$probs),
               tolerance = 1e-12)
})

test_that("predict_sex with a token absent from the sex table reports n=0", {
  pred <- predict_sex("ZZZQQQNOTAREALNAME")
  expect_equal(pred$n, 0L)
  expect_null(pred$probs)
})

test_that("predict_names returns the fixed 7-column probability frame", {
  df <- data.frame(
    first  = c("Maria",     "John",  "Mary Ann"),
    middle = c("Jose",       NA,      NA),
    last   = c("Garcia",    "Smith", "Johnson"),
    maiden = c(NA,           NA,     "Lopez"),
    stringsAsFactors = FALSE
  )
  out <- predict_names(df)
  ## Output is exactly the 7 probability columns, no input passthrough.
  expect_equal(names(out),
               c(paste0("p_", race_groups()), "p_female"))
  expect_equal(nrow(out), 3L)
  ## Row 1 (Maria + Jose + Garcia, no maiden): heavily Hispanic, female.
  expect_gt(out$p_hispanic[1], 0.9)
  expect_gt(out$p_female[1],   0.9)
  ## Row 2 (John + Smith): white-leaning, male (p_female near zero).
  expect_gt(out$p_white[2],  0.5)
  expect_lt(out$p_female[2], 0.1)
  ## Row 3 (Mary Ann compound + Johnson + maiden Lopez): female compound.
  expect_gt(out$p_female[3], 0.9)
  ## Each row's race probs sum to 1.
  for (i in seq_len(nrow(out))) {
    expect_equal(sum(unlist(out[i, paste0("p_", race_groups())])), 1,
                 tolerance = 1e-6)
  }
})

test_that("predict_names works with a subset of columns (last only)", {
  df <- data.frame(last = c("Smith", "Garcia", "Wang"),
                   stringsAsFactors = FALSE)
  out <- predict_names(df)
  expect_equal(names(out),
               c(paste0("p_", race_groups()), "p_female"))
  ## With only a last name, p_female is NA (no first-name field).
  expect_true(all(is.na(out$p_female)))
  ## Race columns are populated.
  expect_false(any(is.na(out$p_hispanic)))
})

test_that("predict_names handles NA cells", {
  df <- data.frame(first = c("Maria", NA, "John"),
                   last  = c(NA,      "Garcia", NA),
                   stringsAsFactors = FALSE)
  out <- predict_names(df)
  ## Row 1: first only → race + sex populated.
  expect_false(is.na(out$p_white[1]))
  expect_false(is.na(out$p_female[1]))
  ## Row 2: last only → race populated, p_female NA.
  expect_false(is.na(out$p_hispanic[2]))
  expect_true(is.na(out$p_female[2]))
  ## Row 3: first only → race + sex populated; John skews male.
  expect_false(is.na(out$p_white[3]))
  expect_lt(out$p_female[3], 0.1)
  expect_s3_class(out, "data.frame")
})

test_that("predict_names ignores extra columns and is column-order agnostic", {
  df <- data.frame(extra = 1:2,
                   last  = c("Smith", "Garcia"),
                   first = c("John",  "Maria"),
                   stringsAsFactors = FALSE)
  out <- predict_names(df)
  expect_equal(nrow(out), 2L)
  expect_gt(out$p_white[1], 0.5)
  expect_gt(out$p_hispanic[2], 0.9)
})

test_that("predict_names column detection is case-insensitive", {
  df <- data.frame(First = c("Maria", "John"),
                   LAST  = c("Garcia", "Smith"),
                   ZCTA  = c("30307", "10001"),
                   stringsAsFactors = FALSE)
  out <- predict_names(df)
  expect_equal(names(out),
               c(paste0("p_", race_groups()), "p_female"))
  ## Same answers as the lowercase version.
  ref <- predict_names(data.frame(first = c("Maria", "John"),
                                  last  = c("Garcia", "Smith"),
                                  zcta  = c("30307", "10001"),
                                  stringsAsFactors = FALSE))
  expect_equal(out, ref)
  ## And mixed-case Block_Group beats ZCTA.
  df2 <- data.frame(First = "Maria", Last = "Garcia",
                    Block_Group = "010010201001", ZCTA = "30307",
                    stringsAsFactors = FALSE)
  out2 <- predict_names(df2)
  expect_equal(sum(out2[1, paste0("p_", race_groups())]), 1, tolerance = 1e-6)
})

test_that("predict_names errors on invalid input", {
  expect_error(predict_names(data.frame(foo = 1)),
               "No recognized columns")
  expect_error(predict_names(list(a = 1)),
               "must be a data.frame")
})

test_that("predict_names with geography folds in the BISG prior", {
  df_no_geo <- data.frame(first = "Maria", last = "Garcia",
                          stringsAsFactors = FALSE)
  df_geo    <- data.frame(first = "Maria", last = "Garcia",
                          zcta  = "30307",
                          stringsAsFactors = FALSE)
  out_no <- predict_names(df_no_geo)
  out_yes <- predict_names(df_geo)
  race_cols <- paste0("p_", race_groups())
  ## Both rows are well-formed.
  expect_equal(sum(out_no[1, race_cols]),  1, tolerance = 1e-6)
  expect_equal(sum(out_yes[1, race_cols]), 1, tolerance = 1e-6)
  ## Geography changes the posterior.
  expect_false(isTRUE(all.equal(as.numeric(out_no[1, race_cols]),
                                as.numeric(out_yes[1, race_cols]))))
})

test_that("predict_names picks the most specific geography column", {
  ## Only `tract` is recognized when both `tract` and `zcta` are present;
  ## block_group beats both. Here we just confirm tract beats zcta.
  df <- data.frame(first = "Maria", last = "Garcia",
                   tract = "01001020100", zcta = "30307",
                   stringsAsFactors = FALSE)
  out <- predict_names(df)
  expect_equal(sum(out[1, paste0("p_", race_groups())]), 1, tolerance = 1e-6)
})

## ---- Rosenman, Olivella, and Imai (2023) NotInCensus2020 fallback ----

test_that("Rosenman extra tables are present and well-formed", {
  fe <- openBISG:::table_df("first_extra")
  le <- openBISG:::table_df("last_extra")
  expect_true(nrow(fe) > 50000)
  expect_true(nrow(le) > 100000)
  expect_named(fe, c("name", "frequency", race_groups()))
  expect_named(le, c("name", "frequency", race_groups()))
  ## Each row's race probabilities sum to 1 (modulo rounding).
  expect_equal(unname(rowSums(fe[1:50, race_groups()])),
               rep(1, 50), tolerance = 1e-6)
  expect_equal(unname(rowSums(le[1:50, race_groups()])),
               rep(1, 50), tolerance = 1e-6)
  ## The Rosenman tables are by construction disjoint from the Census tables.
  expect_length(intersect(fe$name, openBISG:::table_df("first")$name), 0)
  expect_length(intersect(le$name, openBISG:::table_df("last")$name),  0)
})

test_that("include_extra = FALSE leaves a Rosenman-only name as a miss", {
  ## AABIDA is in the Rosenman first-name extras, not in Census.
  expect_null(lookup_with_fallback("AABIDA", tables = c("first", "last")))
  pred <- predict_race(first = "AABIDA")
  expect_null(pred$tokens$first[["AABIDA"]])
  expect_null(pred$combined)
})

test_that("include_extra = TRUE finds a Rosenman-only first name", {
  hit <- lookup_with_fallback("AABIDA",
                              tables = c("first", "last"),
                              include_extra = TRUE)
  expect_false(is.null(hit))
  expect_equal(hit$source, "first")
  expect_equal(hit$dataset, "rosenman")
  expect_equal(hit$rule, "exact")
  expect_named(hit$probs, race_groups())
  expect_equal(sum(hit$probs), 1, tolerance = 1e-6)

  pred <- predict_race(first = "AABIDA", include_extra = TRUE)
  expect_equal(pred$tokens$first[["AABIDA"]]$dataset, "rosenman")
  expect_equal(pred$combined$n, 1L)
  expect_equal(sum(pred$combined$probs), 1, tolerance = 1e-6)
})

test_that("Census hits still win over Rosenman extras when both are available", {
  ## MICHAEL is in the Census first-name table.
  hit <- lookup_with_fallback("MICHAEL",
                              tables = c("first", "last"),
                              include_extra = TRUE)
  expect_equal(hit$dataset, "census")
  expect_equal(hit$source,  "first")
})

test_that("predict_race(include_extra) recovers a wholly Rosenman pair", {
  ## Pick a first/last pair where both are absent from Census.
  fe <- openBISG:::table_df("first_extra")$name[1:100]
  le <- openBISG:::table_df("last_extra")$name[1:100]
  fname <- fe[1]; lname <- le[1]
  pred_off <- predict_race(first = fname, last = lname)
  expect_null(pred_off$combined)  # both miss without the extras

  pred_on <- predict_race(first = fname, last = lname, include_extra = TRUE)
  expect_equal(pred_on$combined$n, 2L)
  expect_equal(sum(pred_on$combined$probs), 1, tolerance = 1e-6)
  expect_equal(pred_on$tokens$first[[1]]$dataset, "rosenman")
  expect_equal(pred_on$tokens$last[[1]]$dataset,  "rosenman")
})

test_that("predict_names(include_extra) populates probabilities for Rosenman-only names", {
  fe <- openBISG:::table_df("first_extra")$name[1:1]
  le <- openBISG:::table_df("last_extra")$name[1:1]
  df <- data.frame(first = c("Maria", fe),
                   last  = c("Garcia", le),
                   stringsAsFactors = FALSE)
  out_off <- predict_names(df)
  out_on  <- predict_names(df, include_extra = TRUE)
  ## Census row is identical either way.
  expect_equal(out_off$p_white[1], out_on$p_white[1])
  ## Rosenman row: NA without the extras, populated with them.
  expect_true(is.na(out_off$p_white[2]))
  expect_false(is.na(out_on$p_white[2]))
  expect_equal(sum(unlist(out_on[2, paste0("p_", race_groups())])),
               1, tolerance = 1e-6)
})

test_that("include_extra has no effect on the sex prediction", {
  ## Even if include_extra = TRUE, sex still uses only first_names_sex
  ## (the Rosenman dataset has no sex info).
  pred <- predict_race(first = "AABIDA", include_extra = TRUE)
  ## AABIDA has no Census sex row, so $sex$n must be 0.
  expect_equal(pred$sex$n, 0L)
  expect_null(pred$sex$probs)
})
