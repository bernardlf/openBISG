# openBISG 0.7.6

## `predict_wru()`: wru-style BISG from the bundled tables

New exported function replicating the estimation of the `wru`
package's standard BISG (Imai and Khanna 2016) — surname times Census
geography, in wru's five racial categories — without a Census API
download: the geography component comes from the bundled tables at the
Census Block, Block Group, or Tract level.

wru computes `P(R|G,S) ∝ P(G|R) P(R|S)` with `P(G|R)` from Census
counts downloaded per state; since `P(G|R) = P(R|G) P(G) / P(R)`, that
is the openBISG fold with the state's composition as the marginal
`P(R)`. `predict_wru()` runs that fold state by state: the six
openBISG groups are collapsed to wru's five **before** the fold
(`whi` = white, `bla` = black, `his` = hispanic, `asi` = aapi,
`oth` = aian + nh_multi), raw counts are folded (`geo_smooth = 0`
default — as in wru, a zero Census cell zeroes that group), there is
no block-group fallback and no `block_shrink` (rows whose geography
misses carry the surname-only posterior; unmatched surnames carry the
geography-only posterior). Input is a data frame with a `last` /
`surname` column and a `block` / `block_group` / `tract` column;
output columns `p_whi` ... `p_oth` correspond to wru's `pred.*`.

Documented departures from wru, all consequences of the bundled data:
the population basis is 2020 voting-age population (or CVAP at
block-group / tract level) rather than wru's default all-ages counts,
and surnames are cleaned/compound-matched by openBISG's cascade
rather than wru's.

# openBISG 0.7.5

## Block counts are shrunk toward the parent block group (`block_shrink`)

A census block is small enough that its complete-count VAP row is often
degenerate for locally rare groups: a block of 40 adults with zero
recorded Hispanic residents pins the Hispanic geography share at
(nearly) zero even when the surrounding block group is 10% Hispanic.
Validation against self-reported race on the 2026 Georgia voter file
(7.3M records) shows these zero-own-count blocks account for
essentially all of the block prior's disadvantage versus the
block-group prior — Hispanic voters in such blocks were misclassified
at 44.9% vs 30.1% under the block-group prior, Asian voters at 58.1%
vs 44.3% — while blocks with five or more own-group adults *beat* the
block group for every group.

Block-level lookups therefore now blend the block's integer counts
with **`block_shrink`** pseudo-people drawn from the parent block
group's composition before normalizing — a Dirichlet prior with the
block group as the base measure:

    p_blend = (counts + block_shrink * p_bg) / (total + block_shrink)

`geo_smooth` is applied on top with the blended scale
`total + block_shrink`. The new argument (on `geo_prior()`,
`predict_race()`, `predict_names()`, and `predict_demog()`) defaults
to **`10`**, which leaves well-populated blocks essentially unchanged
while pulling degenerate ones toward their block group. In the Georgia
emulation the default lowers the overall five-category error from
12.59% to 12.44% and the zero-own-count-block false negative rates by
6–26 percentage points (Hispanic 44.9% → 38.6%, Asian 58.1% → 51.2%,
White 52.1% → 37.2%, Black 76.3% → 54.5%), at a cost of +0.09pp on the
White false negative rate; the optimum is flat across
`block_shrink` values of roughly 5–20.

**This changes default block-level results.** Pass `block_shrink = 0`
to reproduce 0.7.0 output exactly. The blend applies only to `block`
lookups against the bundled VAP table (populated blocks); the
zero-VAP `block_fallback` reroute — the limit case of the blend — and
the CVAP block-group reroute are unchanged, as is a user-supplied
`geo_dict`. Block-level `geo_prior()` results carry a new
`block_shrink` attribute with the pseudo-count actually blended in
(`0` when disabled, or when the parent block group has no usable
row), and `predict_race()$geography` gains the matching
`block_shrink` field.

Note for positional callers: `block_shrink` was inserted after
`block_fallback` in `geo_prior()`, `predict_names()`, and
`predict_demog()`, and between `block_fallback` and `geography_probs`
in `predict_race()` — arguments passed by position after those points
shift by one.

# openBISG 0.7.0

## Block-level geography

The package now ships a seventh geographic prior, **`geo_block_vap`**:
voting-age population by race / Hispanic origin for every populated 2020
census block, built from the 2020 P.L. 94-171 Redistricting Data (Table
P4). It covers 5,704,969 blocks across the 50 states, DC, and — unlike
the DHC-based VAP tables — Puerto Rico. Every entry point accepts the
new geography: `geo_prior(block = ...)`, `predict_race(block = ...)`, a
`block` column in `predict_names()` / `predict_demog()` (taking
precedence over `block_group` > `tract` > `zcta`), and a Census Block
field in the Shiny app. Block GEOIDs are 15 digits, optionally with the
`"7500000US"` Summary File prefix.

Unlike the other geography tables, `geo_block_vap` stores **integer
counts rather than proportions** — counts compress to roughly 60% of
the size (the table is 20 MB, by far the largest shipped) and all
consumers row-normalize on the fly, so results are identical to a
proportion table. Note the first block-level call in a session loads
and sweeps the 5.7M-row table (a few seconds, ~600 MB of memory).

Two situations reroute a block lookup to the block's parent block group
(`substr(geoid, 1, 12)`), each reported via a suppressible `message()`
that counts the affected rows:

* **Blocks with no voting-age population.** `geo_block_vap` covers
  populated blocks only, so a valid 15-digit GEOID that misses is a
  zero-VAP block (or one that does not exist). By default such lookups
  fall back to the block group's proportions; the new
  **`block_fallback`** argument (on `geo_prior()`, `predict_race()`,
  `predict_names()`, and `predict_demog()`) disables the fallback, in
  which case those rows get no geography component.
* **`geography_type = "cvap"`.** Citizenship is not collected in the
  decennial census and the CVAP Special Tabulation stops at block
  groups, so there is no block-level CVAP table; block lookups under
  CVAP always use the parent block group's CVAP row.

`predict_race()`'s geography metadata records a reroute as
`level = "block_group"` plus `fallback_from = "block"`.

Note for positional callers: `block` was inserted after `block_group`
in the `geo_prior()` / `predict_race()` signatures, and
`block_fallback` after `geo_smooth` in all four — arguments passed by
position after those points shift by one.

# openBISG 0.6.0

## Sampling zeros in the geographic prior no longer annihilate the name evidence

BISG folds the name posterior together with geography as
`P(R | name, G) ∝ P(R | name) P(R | G) / P(R)`, so a cell of `P(R | G)`
that is exactly zero forces that group's posterior to zero no matter how
decisive the name is. The bundled CVAP tables are built from an ACS
estimate, and at block-group scale a group with nobody in the sample is
published as zero: 49% of `geo_bg_cvap` rows report no Asian / NHPI
citizens age 18+, 86% report no AIAN, and 42% of the national CVAP lives
in a block group with a zero Asian / NHPI count. Those zeros were folded
in at face value, and the displaced mass landed on whichever surviving
group had the smallest marginal prior — usually `nh_multi`, whose `P(R)`
of 0.033 gives it the largest `/P(R)` boost. A record whose name tokens
are individually 95-98% AAPI could come back `p_aapi = 0` with
`p_nh_multi` above 0.9: not merely wrong, but confidently wrong.

`geo_prior()`, `predict_race()`, `predict_names()`, and
`predict_demog()` gain a **`geo_smooth`** argument: a pseudo-count, in
people, that shrinks each looked-up composition toward the
population-weighted national marginal of the same table before it is
folded in — a Dirichlet(`geo_smooth` × national) prior on the
geography's composition,

```
p_smooth = (total * p_geo + geo_smooth * p_national) / (total + geo_smooth)
```

The default is `geo_smooth = 1`. One pseudo-person moves a populated
cell by well under a tenth of a percentage point but turns an exact zero
into a small positive share, so geography still weighs heavily against
the group without ruling it out.

**This changes default numeric output** wherever a geography is
supplied. Pass `geo_smooth = 0` to restore the previous behavior exactly.
Over a 10,000-row sample of names joined to block groups, the default
removes a hard zero from 95% of rows while changing the modal group for
only 0.24%; the median per-row change is 1e-4 and `predict_demog()`
still reproduces `predict_names()` to floating-point precision.

## Relation to `wru`'s fBISG

`geo_smooth` is the same correction `wru`'s `model = "fBISG"` applies to
the geography term. Imai, Olivella & Rosenman (2022) model the published
counts as `N_g ~ Multinomial(N_g, ζ_g)` over a geography's unknown true
composition `ζ_g`, place a `Dirichlet(α)` prior on `ζ_g`, and replace
BISG's `N_rg / Σ N_r'g` with
`(n⁻ⁱ_rg + N_rg + α_r) / Σ_r'(n⁻ⁱ_r'g + N_r'g + α_r')`. `geo_smooth` is
that estimator without the `n⁻ⁱ_rg` term. Three differences:

- **Where α points.** `wru` uses a uniform `α = 1` on every category.
  `geo_smooth` spreads its pseudo-count along the national marginal, so
  rare groups are not floored at the same level as common ones and the
  result is invariant to how finely the categories are split.
- **No pooling across records.** `n⁻ⁱ_rg` is the count of *other records
  in the input* currently assigned to race `r` in geography `g` — the
  term that lets fBISG infer that the Census undercounted a group in a
  place. `geo_smooth` has no equivalent, by design: using one would mean
  having every observation for a geographic unit in hand at the moment
  the estimate is computed, which the per-record and streaming-friendly
  interfaces here deliberately do not require. `geo_smooth` only
  declines to believe an exact zero.
- **Closed form.** fBISG requires a Gibbs sampler and is stochastic and
  `O(iter × n)`. `geo_smooth` is deterministic, adds no measurable
  runtime, and preserves the vectorized `predict_demog()` path and its
  bit-level parity with `predict_names()`.

Scope notes:

- A caller-supplied `predict_race(geography_probs = ...)` is used
  exactly as given — it carries no population count to smooth against.
- A custom `predict_demog(geo_dict = ...)` is smoothed only when the
  table has a `total` column; without one there is no scale, and it is
  silently left alone.
- Name-table zeros are **not** smoothed. Those are published Census
  counts rather than survey estimates, so a token whose count for some
  group is zero still rules that group out. 38% of first-name rows and
  62% of surname rows carry at least one zero cell.
- `geo_prior()` results gain a `geo_smooth` attribute recording the
  pseudo-count applied, and `predict_race()$geography` gains a matching
  `geo_smooth` element.

# openBISG 0.5.0

## `predict_demog()` reaches feature parity with `predict_names()`

`predict_demog()` regains the three conveniences the vectorized rewrite
had dropped relative to `predict_names()`:

- **Sex is back.** With the bundled name tables (`name_dict = NULL`),
  the output now includes a `p_female` column computed from the
  first-name-by-sex table — `first` field only, compound-first cascade,
  identical to `predict_names()` — so the default output has the same
  7-column shape as `predict_names()`. Opt out with the new
  `include_sex = FALSE`. Custom `name_dict` output is unchanged (the
  categories are read off the dictionary).
- **New `progress = TRUE` argument.** A one-line text progress bar on
  `stderr` (percent complete, elapsed, estimated remaining) tracks the
  resolution of unique name values through the matching cascade — the
  dominant cost on large inputs.
- **New `n_cores = 1L` argument.** Values above 1 split the unique
  name values into chunks resolved via `parallel::mclapply`
  (fork-based; serial on Windows). As in `predict_names()`, the
  parallel path replaces the progress bar with start / finish status
  lines, and output is identical to the serial path.

## Bug fixes

- Census Summary File prefixed GEOIDs (`"1400000US..."` tracts,
  `"1500000US..."` block groups) were documented as accepted but never
  matched: the prefix check ran after all non-digit characters
  (including `"US"`) had been stripped, so it could not fire and the
  lookup returned no match. The prefix is now stripped first, and the
  documented forms work in `geo_prior()`, `predict_race()`,
  `predict_names()`, and `predict_demog()`.
- `n_cores > 1` on Windows now falls back to serial processing in both
  `predict_names()` and `predict_demog()` instead of relying on
  `parallel::mclapply`'s platform behavior.

## Other

- License declaration corrected to `GPL (>= 3)`. The repository has
  carried the GPL-3 license text since its initial commit, but
  `DESCRIPTION` declared `MIT + file LICENSE`; the two now agree
  (matching `wru`, whose normalization cascade this package models).
- `DESCRIPTION` now points `URL` / `BugReports` at the package's own
  repository.
- Added `.Rbuildignore` covering the development-only files
  (`data-raw/`, `CLAUDE.md`, `RECOMMENDATIONS.md`) so `R CMD build`
  produces a clean tarball.
- Internal: the progress bar and `n_cores` validation are shared
  helpers used by both batch functions; the Naive-Bayes normalization
  in `predict_demog()` is factored into a single internal routine used
  for both the race and sex posteriors.
- New tests: `predict_demog()` sex-column parity with
  `predict_names()`, parallel-path equality, `include_sex = FALSE`,
  `n_cores` validation, and a new `test-geo.R` covering `geo_prior()`
  ID normalization including the prefixed GEOID forms.

# openBISG 0.4.0

## New: `predict_demog()` — vectorized engine with pluggable dictionaries

The primary race model gets a vectorized rewrite, exported as the new
function `predict_demog(data, name_dict, geo_dict, prior, include_extra,
geography_type)`. `predict_race()`, `predict_sex()`, and
`predict_names()` are unchanged — `predict_demog()` is purely additive.

- **Same probability model, orders of magnitude faster.** The
  compound-first cascade for given-name fields, per-token cascade for
  surname fields, maiden-replaces-last rule, Naive-Bayes combination
  with the *(k − 1)* prior division, and the BISG geography fold are
  all identical to `predict_names()` — the five-step matching cascade
  is literally shared code (extracted from `lookup_name()` into the
  internal `cascade_match()`, so the two paths cannot drift). But the
  computation runs the cascade only once per *unique* name value and
  does the combination as matrix algebra instead of a per-row
  `predict_race()` call. On the bundled 10,000-row benchmark fixture:
  82 s → 3.6 s (~23×, single core). On 100,000 rows with realistic
  name duplication: 3.9 s, ~25,000 rows/s — roughly 200× the
  ~120 rows/s serial `predict_names()` path.
- **Identical output.** With the bundled tables, the race columns
  reproduce `predict_names()` to 1e-12 (regression-tested against the
  per-row path, including geography, `include_extra`, maiden handling,
  compound names, NA cells, and unmatched-geography fallback).
- **User-supplied name dictionaries.** `name_dict` accepts a data
  frame (used for given names and surnames alike) or a
  `list(first =, last =)`. Dictionaries need a `name` column plus two
  or more numeric category columns (optional `frequency`); rows are
  renormalized, names normalized, and lookups use the same cascade and
  cross-table fallback as the bundled path.
- **User-supplied geography.** `geo_dict` accepts a data frame with
  `geoid` + category columns (optional `total`). A `geoid` input
  column is recognized (and matched as-is); the bundled tables remain
  the default, selected by `geography_type` with the usual ID
  normalization.
- **User-supplied category groupings.** The prediction categories are
  read off the supplied tables' columns — the engine is no longer
  wedded to the six race / Hispanic-origin groups. Any component not
  supplied falls back to the bundled Census tables and their six race
  categories; a user-supplied component must then use those same
  categories (informative error otherwise). Sex is just another
  grouping: `predict_demog(df, name_dict = list(first =
  first_names_sex))` returns `p_male` / `p_female`.
- **Explicit prior control.** New `prior` argument for the marginal
  `P(category)`; when omitted it is derived from, in order: the
  dictionary's `prior` attribute, a frequency-weighted average of the
  dictionary rows, a `total`-weighted average of `geo_dict`, the
  bundled Census prior (bundled categories only), and finally a
  uniform prior with a warning whenever the prior materially enters
  the computation.

## Other

- New test file `test-predict-demog.R`: equivalence with
  `predict_names()` to 1e-12, custom-dictionary and custom-category
  behavior, prior derivation, and the category-mismatch error paths.

# openBISG 0.3.6

Documentation release — the contents of PR #9, no code changes.

- New `RECOMMENDATIONS.md`: a code-grounded review of the package
  against `wru` and `eiCompare`'s BISG operationalization, with a
  prioritized roadmap for the lightweight / customizable / faster
  positioning.
- README: removed the documented-but-nonexistent `p_geo_matched`
  column from the Geography section and described the actual
  fallback behavior (`$geography$found = FALSE` in `predict_race()`;
  silent name-only fallback per row in `predict_names()`).
- README: replaced the misleading "Vectors work too" example with an
  explicit callout that vector inputs to a single `predict_race()`
  field are multiple tokens for **one person**, with
  `predict_names()` as the multi-row path.
- README: restructured the Quick start into a short headline example
  plus focused subsections (batch prediction, name fields, geography,
  sex, extra dictionaries) and folded the "Also exported" paragraph
  into a "Lower-level helpers" subsection.

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
