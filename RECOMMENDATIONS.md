# Recommendations: positioning openBISG as a lightweight, customizable, faster BISG alternative for academic researchers

This document reviews the package (v0.3.5) against the two tools academic
researchers currently reach for when they need BISG in R —
[`wru`](https://github.com/kosukeimai/wru) (Khanna, Imai, Rosenman, and
Olivella) and [`eiCompare`](https://github.com/RPVote/eiCompare) (which wraps
`wru` for its BISG step via `wru_predict_race_wrapper`) — and recommends
concrete changes, ordered by how much they advance the three claims in the
positioning statement: **lightweight**, **customizable**, and **faster**.

## Where openBISG already wins

These are real advantages today and should be stated prominently in the
README and any eventual paper:

1. **Fully offline, no API key.** `wru` requires a Census API key and an
   internet connection whenever `census.geo` is used; `eiCompare`'s wrapper
   returns `NULL` outright when the connection fails. openBISG bundles all
   six geographic prior tables, so a prediction on an air-gapped or
   IRB-restricted machine works identically to one on a laptop. For
   researchers working with voter files or health records under data-use
   agreements that prohibit sending data-adjacent queries to external APIs,
   this is a decisive feature — name it explicitly.
2. **2020 name dictionaries.** The 2020 Census first-name *and* last-name
   tabulations, plus the Rosenman–Olivella–Imai (2023) voter-file
   supplements as an opt-in fallback, with the newer 2020–2024 ACS CVAP
   special tabulation as the default geographic prior.
3. **A single hard dependency** (`stringi`). `wru` pulls in a substantial
   tree; `eiCompare` more so. This matters for reproducibility archives
   (e.g., Dataverse replication packages) and for teaching.
4. **First-class compound given-name handling** (`MARIA JOSE` → `MARIAJOSE`)
   and sex prediction — neither is in `eiCompare`'s BISG path, and the
   compound cascade is more complete than `wru`'s.
5. **Transparent, documented math.** The README's derivation of the
   *(k − 1)* prior-division formula is better methodological documentation
   than either competitor ships.

The recommendations below are about closing the gaps that currently prevent
the package from backing up the "faster" claim, and about adding the
customization and validation hooks academics need before they will switch.

---

## 1. Performance — the "faster" claim is currently not true at scale, and must be fixed first

**1.1 Vectorize `predict_names()` (highest priority in the whole document).**
The batch path (`R/predict.R`, `process_row`) calls the full
list-of-lists `predict_race()` machinery once per row. The package's own
benchmark (NEWS, 0.3.5) is 10,000 rows in 160 s serial — **~62 rows/s**.
`wru` is join-based (one merge of the deduplicated surname column against
the dictionary, then matrix algebra) and handles millions of rows in
seconds. Parallelizing the per-row loop (0.3.5's `mclapply`) buys 3× on 4
cores; vectorization buys 2–3 orders of magnitude. Sketch:

- Normalize and deduplicate each name column once
  (`unique()` → cascade → `match()` against `first_names$name` /
  `last_names$name`). The cascade only runs once per *unique* name —
  on a voter file, unique surnames are typically <5% of rows.
- Hold per-row evidence as an *n* × 6 probability matrix per field; the
  Naive-Bayes combination `Π P(R|nᵢ) / P(R)^(k−1)` is row-wise products
  and a vectorized division — no loop.
- Join geography once per unique `geoid` (see 1.2) and fold the prior in
  with one more element-wise multiply/renormalize.
- Keep `predict_race()` as the rich single-record API; reimplement
  `predict_names()` on the vectorized core, and add a regression test that
  the two paths agree to 1e-12 on a fixture (the existing
  `data-raw/test_df.rds` is ideal).

Once this lands, the `n_cores` machinery can probably be retired (or kept
only for the cascade step on unique names), simplifying the code and
removing the Windows `mclapply` caveat.

**1.2 Hash the geography lookup.** `geo_prior()` does a full-column linear
scan per call: `tbl[tbl$geoid == key, , drop = FALSE]` (`R/geo.R`). The
block-group table has ~240k rows, and the batch path repeats the scan for
every row of input. Use `match(key, tbl$geoid)` on a cached vector, or the
same `list2env` hash-index pattern already used for name tables in
`R/lookup.R`. This is a ~10-line change with a large payoff for
geography-heavy workloads.

**1.3 Compute the Bayes combination in log space.** `combine_bayes_n()`
multiplies raw probabilities. With several tokens and small group shares
the product can underflow; `exp(Σ log P − (k−1) log prior)` after
max-subtraction is cheap insurance and worth doing as part of 1.1.

**1.4 Publish a reproducible benchmark vignette.** The "faster than wru"
claim should be demonstrated, not asserted: a vignette that runs
`predict_names()` and `wru::predict_race()` on the same synthetic file
(e.g., 100k rows built like `data-raw/build_test_df.R`), reports rows/sec
and peak memory, and is re-runnable by reviewers. Note honestly where `wru`
is faster until 1.1 lands. Include end-to-end wall time *including* `wru`'s
census download step, since that is the latency users actually experience.

## 2. Lightweight — package size and CRAN

**2.1 Split the data to get under CRAN's 5 MB limit.** `data/` is ~21 MB
compressed; CRAN will not take that, and "lightweight" should also mean
installable via `install.packages()`. Recommended split:

- **openBISG** (core): code + the three Census name tables
  (`first_names`, `last_names`, `first_names_sex`, ~4 MB) — usable for
  name-only prediction out of the box.
- **openBISGdata** (data package, on a `drat` repo or
  [r-universe](https://r-universe.dev), with CRAN-style versioning): the
  six geographic priors and the two Rosenman tables. Core package
  `Suggests:` it and prompts with an informative error (offering a
  one-line install) the first time `geo_prior()` / `include_extra = TRUE`
  is used without it.
- Alternative: download-on-first-use into
  `tools::R_user_dir("openBISG", "data")` with a checksum, like
  `tigris`/`tidycensus` cache patterns. This keeps a single package but
  weakens the offline story for the first run — the data-package split is
  the better fit for the IRB/air-gapped audience, since a data package can
  be carried in a replication archive.

**2.2 Keep the dependency discipline.** `shiny` is already Suggests-only —
good. Resist `dplyr`/`data.table` in the vectorized rewrite (base
`match()`/matrix code is sufficient and keeps the "one dependency" pitch).
Consider whether even `stringi` could become optional (base R `iconv` +
`toupper` cover most of `normalize_name()`), but only if the NFD behavior
can be reproduced exactly — otherwise keep it.

## 3. Customizability — the hooks researchers actually need

**3.1 User-supplied name dictionaries.** Today only the geographic prior is
pluggable (`geography_probs`). Researchers routinely need to swap name
tables: state-specific voter-file dictionaries, historical censuses
(1930–1940 full-count), non-U.S. name lists, or their own
`P(race | name)` estimates. Add a `name_data = list(first = <df>, last =
<df>, first_sex = <df>)` argument (validated to the
`name / frequency / <groups>` schema with a `prior` attribute, exactly as
documented in `R/data.R`), threaded through `table_df()`. This single
argument converts the package from "the 2020 Census tool" into "the BISG
engine," which is the strongest version of the customizability claim.

**3.2 Configurable handling of unmatched names.** Currently unmatched
tokens are silently dropped from the combination (`predict.R`), which
biases batch results toward rows with dictionary-covered names — and the
all-`NA` rows from `predict_names()` force every downstream user to invent
their own policy. Add `missing = c("drop", "prior", "all_other")`:

- `"drop"` — current behavior (default, for backward compatibility);
- `"prior"` — unmatched token contributes the marginal `P(R)` (a no-op in
  the combination, but rows with *no* matches return the prior instead of
  `NA`);
- `"all_other"` — use the `ALL OTHER NAMES` residual row from the Census
  tabulations, which is `wru`'s behavior under `impute.missing` and the
  statistically defensible choice (a name absent from the ≥100-frequency
  tables is *informative*). The residual row is already folded into the
  `prior` attribute at build time (`R/data.R` docs) — re-expose it as its
  own row so this option is possible.

**3.3 Configurable matching cascade.** The five-step cascade is fixed.
Expose `cascade = c("exact", "punct", "space", "suffix", "segment")` so a
researcher can, e.g., disable segment-splitting (step 5), which is the
step most likely to manufacture a false match on hyphenated surnames and
the one reviewers most often ask to sensitivity-test. Report which rule
fired per unique name in the diagnostics output (3.5) so users can audit.

**3.4 More geography levels and vintages.** `wru` supports county-level
priors; openBISG starts at ZCTA. County (and state) tables are tiny
(~3,200 / 51 rows) and cost nothing to bundle in the core package — they
also give a graceful fallback when an address can't be geocoded below
county. Longer term, accept `geo_data = <df>` (geoid + six shares) the same
way as 3.1, so users can supply 2010 vintages, custom service areas, or
school-attendance zones at the *batch* level, not just per-call
`geography_probs`.

**3.5 Richer, opt-in batch output.** `predict_names()` returns a bare
7-column frame with no per-row diagnostics. (The README briefly documented
a `p_geo_matched` column the code never produced; the README has since
been corrected — a real match indicator belongs in the `details` output
proposed here.) Add
`details = FALSE` which, when `TRUE`, appends diagnostics columns:
`geo_matched` (logical), `n_evidence` (the `k` used), `surname_used`
(`last`/`maiden`/none), `dataset` (census/rosenman), and `match_rules`.
Also add `bind = FALSE` to return `cbind(data, probs)` — every downstream
script currently has to do this manually and can silently misalign rows
after a filter.

**3.6 Clarify (or change) vector semantics in `predict_race()`.** The
implementation treats a character vector passed to a single field as
multiple *tokens for one person*, not multiple people — a researcher who
assumes rowwise vectorization gets one silently-wrong posterior instead of
*n* predictions. The README's old "Vectors work too" example invited
exactly that misreading; it has been replaced with an explicit
"vector inputs are tokens, not rows" callout pointing to
`predict_names()`. The remaining (optional) code-level improvement is to
make `predict_race()` truly rowwise-vectorized, or to warn when every
field receives an equal-length vector longer than one.

## 4. Credibility — what academics need before they will cite and switch

**4.1 Validation against ground truth, in a vignette.** The single most
persuasive artifact is a calibration study on a public file with
self-reported race (the North Carolina or Florida registered-voter files
are the standard choice; both are used in the BISG literature). Report
accuracy, calibration plots, and Brier scores side by side with `wru`
(BISG mode and fBISG mode) and `eiCompare`'s wrapper. If openBISG's
posteriors agree with `wru`'s to numerical tolerance on shared inputs,
say so — agreement with the incumbent *is* the validation. Where they
differ (compound first names, 2020 vs 2010 dictionaries, CVAP vs total
population priors), quantify the difference.

**4.2 Aggregation and diagnostic helpers (the eiCompare hand-off).**
`eiCompare` users run BISG as step one of an ecological-inference
workflow. To be a drop-in replacement for that step, add two small
functions: (a) `aggregate_probs(probs, by, weights)` — sum posterior
probabilities by geography/precinct to produce the racial composition
estimates EI consumes (probability sums, *not* argmax counts — document
why, with the thresholding-bias citations); and (b) an output shape (or
documented recipe) that feeds directly into `eiCompare`/`ei`/`eiPack`.
A vignette titled "Using openBISG with eiCompare" captures that audience
with very little code.

**4.3 If a classification helper is added, make it noisy.** Researchers
will argmax the probabilities whether or not the package helps them. A
`classify(probs, threshold = NULL)` that returns `NA` below the threshold
and documents the deterministic-assignment bias literature is safer than
leaving everyone to roll their own — but it should warn on first use per
session.

**4.4 CRAN + citation hygiene.** Before any of this is announced:

- `DESCRIPTION` `URL`/`BugReports` point at `bernardlf/namedata`, not this
  repo — fix.
- `Authors@R` is a placeholder ("openBISG contributors,
  noreply@example.com"). CRAN requires a real maintainer; academics need
  a real name to cite. Add ORCIDs.
- Add `inst/CITATION` (package citation now; the software paper later —
  JOSS is a good fit for exactly this "faster, lighter reimplementation
  with validation" story, and gives users a DOI to cite).
- Verify redistribution licensing of the Rosenman–Olivella–Imai Dataverse
  tables (the Dataverse deposit's terms govern whether the derived
  `_extra` tables can ship in a package; document the determination in
  `data-raw/`).

**4.5 CI and test gaps.** Tests cover lookup/normalize/predict well, but
nothing in `R/geo.R` (`geo_prior()` key normalization, `combine_name_geo`,
CVAP/VAP selection, the PR-missing-from-VAP edge) — add a `test-geo.R`.
Add GitHub Actions `R-CMD-check` across the OS matrix (the Windows
`mclapply` fallback is currently untested anywhere) plus a coverage badge.
Cheap, and the badges matter disproportionately for adoption.

**4.6 pkgdown site.** The README is strong; a pkgdown site with the
vignettes from 1.4, 4.1, and 4.2 makes it discoverable and is the standard
trust signal for R packages in this space.

---

## Priority order

| # | Recommendation | Claim served | Impact | Effort |
|---|---|---|---|---|
| 1 | 1.1 Vectorize `predict_names()` — *shipped in 0.4.0 as `predict_demog()` (~23–200×); `predict_names()` itself still per-row* | faster | very high | medium |
| 2 | 1.2 Hash geography lookup | faster | high | low |
| 3 | 3.2 `missing =` policy for unmatched names | customizable, correctness | high | low–medium |
| 4 | 4.4 DESCRIPTION/CITATION/licensing fixes | credibility | high | low |
| 5 | 2.1 Data-package split → CRAN | lightweight | high | medium |
| 6 | 3.5 Diagnostics columns + `bind` (the `p_geo_matched` doc mismatch is already fixed) | usability | medium | low |
| 7 | 4.1 Validation vignette vs wru on a public voter file | credibility | very high | medium–high |
| 8 | 3.1 User-supplied name dictionaries — *shipped in 0.4.0 via `predict_demog(name_dict =)`, with user-defined category groupings* | customizable | high | medium |
| 9 | 4.2 Aggregation helper + eiCompare hand-off vignette | adoption | medium | low–medium |
| 10 | 4.5 CI + `test-geo.R` | credibility | medium | low |
| 11 | 1.4 Benchmark vignette | faster | medium | low (after #1) |
| 12 | 3.4 County/state priors; batch `geo_data =` — *batch user geography shipped in 0.4.0 via `predict_demog(geo_dict =)`; bundled county/state tables still open* | customizable | medium | medium |
| 13 | 3.3 Configurable cascade | customizable | medium | low |
| 14 | 3.6 Vector-semantics: docs fixed; optional rowwise vectorization or warning | correctness | low | low |
| 15 | 1.3 Log-space combination | robustness | low | low |
| 16 | 4.3 Guarded `classify()` helper | adoption | low | low |
| 17 | 4.6 pkgdown site | adoption | medium | low |

The through-line: items 1–2 make the "faster" claim true; items 3–6 make
the package safe to recommend; items 7–9 are what convert `wru`/`eiCompare`
users; the rest compound credibility over time.
