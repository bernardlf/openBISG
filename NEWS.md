# openBISG 0.3.5

## Parallel `predict_names()`

- New `n_cores = 1L` argument. When set to a value > 1, rows are split
  into `n_cores` chunks and processed via `parallel::mclapply`
  (fork-based, so the ~22 MB bundled lookup tables are shared via
  copy-on-write with no per-worker export cost). Output is
  bit-for-bit identical to the serial path.
- On Windows fork is unavailable; `mclapply` silently falls back to
  serial — pass `n_cores = 1L` to suppress the warning.
- The per-row progress bar is replaced by a single start / finish
  status line in the parallel path (workers can't synchronize
  `stderr` cleanly).
- Internal refactor: per-row work is now a closure that fills a
  pre-allocated numeric matrix, so the serial path benefits too (no
  more per-row `data.frame[i, ] <- ...` assignment).

Observed speedup on a 10,000-row benchmark in a 4-core container:
`1×` (1 core, 160 s) → `1.4×` (2 cores) → `3.2×` (4 cores).

# openBISG 0.3.0

User-facing changes since `v0.2`.

## `predict_names()` rewritten

- Auto-detects which input columns are present in `data` from the
  recognized set: `first`, `middle`, `last`, `maiden`, `zcta`, `tract`,
  `block_group`. Column-name detection is **case-insensitive**, so
  `First` / `LAST` / `Block_Group` / `ZCTA` all work.
- Returns a fixed 7-column data frame: `p_white`, `p_black`, `p_aian`,
  `p_aapi`, `p_nh_multi`, `p_hispanic`, `p_female` (with
  `P(male) = 1 - p_female`). Race columns hold the BISG-combined
  posterior `P(R | name, G)` when a geography column is detected,
  otherwise the name-only posterior.
- Geography-only input (no name columns) is now supported and returns
  the `P(R | G)` row.
- When multiple geography columns are present, the most specific one
  wins: `block_group` > `tract` > `zcta`.
- New `progress = TRUE` argument: prints a one-line text progress bar
  to `stderr` showing percent complete, elapsed time, and an estimated
  time remaining. Pass `progress = FALSE` to suppress.

**Breaking change:** the previous `predict_names()` signature
(`first = "first"`, `last = "last"`, `zcta_col = "zip"`, `prefix = "p_"`,
`include_meta = ...`) is removed. Callers should rename their input
columns to the canonical names and drop the explicit-column arguments.
For per-call detail (token-level hits, surname source, geography
metadata), use `predict_race()` and `predict_sex()` — both unchanged.

## Documentation

- CRAN-style help pages generated for every exported function and
  bundled dataset (20 `.Rd` files in `man/`).
- Package-level help page: `?openBISG`.
- README updated: clarified geography inputs and source vintages,
  reorganized the **Probability model** section so the matching cascade
  sits between **Surname fields** and **Combining across *k* tokens**,
  and refreshed the `predict_names()` examples.
- `roxygen2` markdown processing enabled (`Roxygen: list(markdown = TRUE)`),
  so backticked code and `[fn()]` cross-references render correctly.
- `LazyDataCompression: xz` added to silence the *"LazyData DB without
  LazyDataCompression"* `R CMD check` WARNING.

## Other

- Added `data-raw/build_test_df.R` and a materialized
  `data-raw/test_df.rds` (10,000 rows, deterministic via
  `set.seed(20260427L)`) for benchmarking and integration testing of
  `predict_names()`.

# openBISG 0.2.0

Initial functional release. Tagged retrospectively at the merge of
PR #2.

- `predict_race(first, middle, last, maiden, zcta, tract, block_group,
  geography_type, geography_probs, include_extra)` — single-call BISG /
  BIFSG prediction with full per-token / geography metadata.
- `predict_sex(first)` — single-call sex probability with the
  compound-first cascade.
- `predict_names(data, first, middle, last, maiden, prefix, include_meta,
  include_extra, zcta_col, tract_col, block_group_col, geography_type)`
  — vectorized batch interface, explicit-column-name signature,
  appends per-row probability columns to the input frame. (Replaced in
  0.3.0 — see above.)
- `geo_prior(zcta, tract, block_group, type)` — geographic prior
  `P(R | G)` from the bundled CVAP / VAP tables.
- `lookup_name`, `lookup_with_fallback`, `lookup_compound_or_tokens`,
  `tokenize_names`, `normalize_name` — name-matching helpers.
- `race_groups`, `race_group_labels`, `sex_groups`, `sex_group_labels`
  — group keys and human-readable labels.
- `run_app()` — bundled Shiny lookup app.
- Bundled data: `first_names`, `last_names`, `first_names_sex`
  (Census 2020); `first_names_extra`, `last_names_extra` (Rosenman,
  Olivella, and Imai 2023 voter-file additions); six geographic priors:
  `geo_zcta_cvap`, `geo_tract_cvap`, `geo_bg_cvap` (2020-2024 ACS
  Special Tabulation), `geo_zcta_vap`, `geo_tract_vap`, `geo_bg_vap`
  (2020 Decennial DHC Table P11).

# openBISG 0.1

Initial pre-release.
