# openBISG

Open-source Bayesian Improved Surname Geocoding (BISG) for R.
Computes per-name race / Hispanic-origin and sex probabilities from the
U.S. Census Bureau's 2020 Decennial Census frequently occurring first and
last names tabulations, optionally folded together with a geographic
prior at ZIP / ZCTA, Census Tract, or Block Group level (CVAP from the
2020-2024 ACS Special Tabulation, or VAP from 2020 DHC).

## Install

You can install **openBISG** from
[GitHub](https://github.com/bernardlf/openBISG) with:

```r
# install.packages("pak")
pak::pkg_install("bernardlf/openBISG")
```

Or, from a clone of the repo:

```r
install.packages(c("stringi", "shiny"))     # one-time
install.packages("openBISG", repos = NULL, type = "source")
```

The package ships eleven lazy-loaded data objects (~22 MB compressed):
`first_names`, `last_names`, `first_names_sex`,
`first_names_extra`, `last_names_extra` (the Rosenman, Olivella, and Imai
(2023) voter-file based additional names, opt-in via
`include_extra = TRUE`), and the six geographic priors
`geo_zcta_cvap`, `geo_tract_cvap`, `geo_bg_cvap`,
`geo_zcta_vap`, `geo_tract_vap`, `geo_bg_vap`.

## Quick start

```r
library(openBISG)

# Race / Hispanic-origin and sex probabilities from a name.
predict_race(first = "Maria", last = "Garcia")
#> $combined$probs : 6-vector P(group | MARIA, GARCIA)
#> $sex$probs      : P(female | MARIA)

# Full BISG: fold a geographic prior into the name posterior.
predict_race(first = "Maria", last = "Garcia", zcta = "30307")

# Batch: data frame in, data frame of probabilities out.
predict_names(data.frame(first = "Maria", last = "Garcia", zcta = "30307"))
```

The subsections below tour the main options. The **Probability model**
and **Geography** sections further down document the underlying math,
data sources, and accepted ID formats.

### Batch prediction over a data frame

`predict_names()` auto-detects which of the recognized columns are
present in the input (any subset of `first`, `middle`, `last`, `maiden`,
`zcta`, `tract`, `block_group`; matching is case-insensitive, so
`First`, `LAST`, `Block_Group` all work) and returns a 7-column data
frame: six race probabilities plus `p_female`
(with `P(male) = 1 - p_female`).

```r
df <- data.frame(
  first  = c("Maria",  "John",  "Mary Ann"),
  middle = c("Jose",    NA,      NA),
  last   = c("Garcia", "Smith", "Johnson"),
  maiden = c(NA,        NA,     "Lopez"),
  zcta   = c("30307",  "10001", "94110"),
  stringsAsFactors = FALSE
)
predict_names(df)
#>      p_white     p_black ... p_hispanic   p_female
#> 1 0.01400823 0.000446167 ... 0.98006158 0.99764726
#> 2 0.86265605 0.096371682 ... 0.00913621 0.00208088
#> 3 0.88144100 0.038691292 ... 0.02007915 0.99907737

# Parallelize across cores via `parallel::mclapply` (fork-based on
# Unix / macOS; silently serial on Windows). Output is bit-for-bit
# identical to the serial path; ~3x speedup on 4 cores in our bench.
predict_names(big_df, n_cores = 4L)
```

For large files, prefer `predict_demog()` below — same model, orders
of magnitude faster.

### Vectorized engine and custom dictionaries: `predict_demog()`

`predict_demog()` (new in 0.4.0) is the vectorized successor to
`predict_names()`: the same probability model and matching cascade,
but computed via deduplicated lookups and matrix algebra. On our
benchmarks it runs ~23x faster on the bundled 10,000-row fixture and
~200x faster (~25,000 rows/s) on a 100,000-row file with realistic
name duplication. With the bundled tables it reproduces the full
7-column `predict_names()` output (race + `p_female`) to 1e-12, and it
takes the same `progress` and `n_cores` arguments (a text progress bar
on the serial path; fork-based `parallel::mclapply` over the unique
name values when `n_cores > 1`).

It also generalizes the tables: supply your own name dictionary
and/or geography table and the prediction categories are read off the
supplied tables' columns — the engine is not wedded to the six race /
Hispanic-origin groups. Any component you don't supply falls back to
the bundled Census tables (and then a supplied component must use the
six bundled race categories).

```r
# Drop-in fast path for the bundled model: same 7 columns as
# predict_names(), including p_female. Suppress the sex column with
# include_sex = FALSE.
predict_demog(df)

# Large files: parallelize the cascade over unique name values.
predict_demog(big_df, n_cores = 4L)

# Sex-only categories via the bundled sex table as a custom dictionary.
predict_demog(data.frame(first = c("Michael", "Maria Jose")),
              name_dict = list(first = first_names_sex))
#> p_male, p_female

# Fully custom categories: your own name dictionary + geography table.
nd <- data.frame(name  = c("ALICE", "BOB"),
                 urban = c(0.8, 0.3), rural = c(0.2, 0.7))
gd <- data.frame(geoid = c("A1", "B2"),
                 urban = c(0.9, 0.2), rural = c(0.1, 0.8))
predict_demog(data.frame(first = c("Alice", "Bob"),
                         geoid = c("A1", "B2")),
              name_dict = nd, geo_dict = gd,
              prior = c(urban = 0.5, rural = 0.5))
#> p_urban, p_rural
```

See `?predict_demog` for dictionary schemas, the category-matching
rules, and how the marginal prior is derived when `prior` is omitted.

### Name fields

```r
# Compound given names match their own Census row: "Maria Jose" reads
# MARIAJOSE directly (one evidence piece, ~99.6% female) instead of
# combining the female MARIA row with the male JOSE row.
predict_race(first = "Maria Jose")

# Two given names split across fields → two evidence pieces for race,
# but sex uses ONLY the first-name field (middle names are excluded).
predict_race(first = "Maria", middle = "Jose")

# A maiden name replaces the last name in the combined estimate;
# $tokens$last is still populated for reference.
predict_race(first = "Maria", last = "Smith", maiden = "Garcia")
#> $surname_used = "maiden" ; combined uses MARIA + GARCIA, ignoring SMITH
```

> **Vector inputs are tokens, not rows.** Passing a character vector to
> a single field — `predict_race(first = c("Maria", "Jose"))` — treats
> the elements as multiple name tokens for **one person**, exactly like
> `first = "Maria Jose"`. It does *not* return two predictions. To
> predict for many people at once, put them in a data frame and use
> `predict_names()`.

### Geography

```r
# Pass at most one of zcta / tract / block_group per call.
predict_race(first = "Maria", last = "Garcia", zcta = "30307")
predict_race(last = "Smith",  tract = "01001020100", geography_type = "vap")

# Look up just the geographic prior P(R | G).
geo_prior(zcta = "00601")              # ZCTA in Puerto Rico (CVAP)
geo_prior(tract = "01001020100")       # tract in Autauga County, AL
geo_prior(block_group = "010010201001", type = "vap")

# Geographic priors are shrunk toward the national marginal by a
# one-person pseudo-count, so a sampling zero in the ACS estimate
# cannot zero out a group the names point to. `geo_smooth = 0`
# folds in the published shares unchanged.
predict_race(first = "Maria", last = "Garcia", zcta = "30307",
             geo_smooth = 0)
```

The default population basis is CVAP (citizens 18+); pass `"vap"` for
everyone 18+. See **Geography** below for ID formats, data sources,
how to choose between the two bases, and why `geo_smooth` defaults to
a nonzero pseudo-count.

### Sex

```r
predict_sex("Michael")     # P(male) close to 1, from the MICHAEL row
predict_sex("Maria Jose")  # P(female) ≈ 0.996 — the compound MARIAJOSE row
```

Sex is estimated from the `first` field only; middle names are
deliberately excluded (see **Probability model**).

### Names not in the Census tables

```r
# Opt-in fall-through to the Rosenman, Olivella, and Imai (2023)
# voter-file dictionaries for names absent from Census 2020.
predict_race(first = "AABIDA", include_extra = TRUE)
#> $tokens$first[["AABIDA"]]$dataset = "rosenman"

# Same flag on the batch interface.
predict_names(df, include_extra = TRUE)
```

### Lower-level helpers

The building blocks are exported too: `lookup_name()` (single-table
cascade lookup), `lookup_with_fallback()` (try a primary table then a
secondary on miss), `lookup_compound_or_tokens()` (compound-first
cascade for given-name fields), `tokenize_names()` (whitespace
tokenizer), and `normalize_name()` (NFD + uppercase). For per-call
detail (token-level hits, surname source, geography metadata), use
`predict_race()` and `predict_sex()` directly rather than the batch
interface.

## Probability model

`predict_race(first, middle, last, maiden)` accepts each field as either
a length-one whitespace-separated string or a character vector. Lookup
is **compound-first for given-name fields** and **per-token for surname
fields**, so a person reporting their name the way the Census tabulated
it gets the same answer the Census dictionaries publish.

> **A note on Asian + NHPI biracials and the `aapi` key.** The brief's
> methodology (Comenetz 2016 §3; Word et al. 2007 §4.7) constructs the
> `aapi` bin by "combining" the single-race Non-Hispanic Asian Alone
> and single-race Non-Hispanic Native Hawaiian / Pacific Islander Alone
> populations, consistent with the pre-1997 OMB single "API" race
> category. The text leaves ambiguous whether the small sub-population
> of NH respondents who report both Asian *and* NHPI (and no other
> race) is placed in `aapi` or in `nh_multi`. Per the 2020 Census P.L.
> 94-171 Summary File, Table P2, this group totals **197,918 people**
> (variable `P2_025N`) — about **1.0% of the 20,438,655 NH respondents
> whose reported races are some combination of Asian and NHPI only**
> (`P2_008N` + `P2_009N` + `P2_025N`: 19,618,719 + 622,018 + 197,918).
> Whichever bin the brief assigns them to, the population mismatch is
> well under 1% of the AAPI total and does not materially affect
> predictions for individual names. Source: U.S. Census Bureau, *2020
> Census Redistricting Data (P.L. 94-171) Summary File*, Table P2,
> accessed via <https://api.census.gov/data/2020/dec/pl> on 2026-04-25.

### Given-name fields: compound-first cascade

The Census first-name brief explicitly notes that internal spaces are
stripped during normalization (`"MICHAEL, MI CH AEL, MICHAELJR, and
MICHAEL III are all counted as MICHAEL"`), and as a result compound
forms like `MARYANN`, `ANNMARIE`, `MARYBETH`, `MARIAJOSE` show up as
their own rows. Their race and sex distributions can differ
substantially from the parts: `MARIAJOSE` is **99.6% female** despite
`JOSE` alone being 99.8% male. Naive-Bayes-combining `MARIA` and `JOSE`
would give ~50/50 (the two signals nearly cancel); reading `MARIAJOSE`
directly is much more accurate.

For each given-name field, [lookup_compound_or_tokens()] therefore:

1. Tries the entire field as one name. The cascade in [lookup_name()]
   strips internal spaces, so `"Mary Ann"` matches `MARYANN`. If the
   match did not come from the cascade's segment-split rule, the field
   contributes **one** evidence piece (`k = 1`).
2. Otherwise tokenizes on whitespace and looks up each token, with a
   fallback from the first-name table to the last-name table.

### Surname fields: per-token

`last` and `maiden` are tokenized per-token only (no compound retry),
with a fallback from the last-name table to the first-name table on
miss. When `maiden` is non-empty its tokens **replace** `last` in the
combined estimate; the last-name lookups are still recorded under
`$tokens$last` and `$surname_used` reports `"maiden"`.

Tokens absent from both tables are reported but excluded from the
combined estimate.

### Name matching cascade

Modeled on the [`wru`](https://github.com/kosukeimai/wru) 
R package (Khanna, Imai, Rosenman, and Olivella 2021; Imai and Khanna 2016). 
Steps applied to the user-supplied name in order; the first hit wins:

1. **exact** — trim, NFD-decompose + drop combining marks, uppercase
   (`PEÑA` matches `PENA`).
2. **punctuation removed** — drop non-alphanumerics except spaces
   (`O'CONNOR` → `OCONNOR`).
3. **punctuation and spaces removed** — also drop spaces
   (`VAN DER BERG` → `VANDERBERG`).
4. **generational suffix removed** (last names only) — strip `JR`, `SR`,
   `II`, `III`, `IV`, `JUNIOR`, `SENIOR`, `THIRD`. `SR` only stripped if the
   suffixed form is at least 7 characters.
5. **first / second segment of a multi-part name** — split on hyphen, comma,
   or space (`GARCIA-LOPEZ` → `GARCIA`, then `LOPEZ`).

No fuzzy / edit-distance / phonetic matching.

### Combining across *k* matched tokens

The package implements the Bayesian Improved Surname Geocoding (BISG)
formulation (Elliott et al. 2008, 2009; Fiscella and Fremont 2006;
Imai and Khanna 2016) as extended with first-name information by Voicu
(2018) and middle-name information by Imai, Olivella, and Rosenman
(2022). This is a Naive Bayes implementation under the assumption that each 
name is conditionally independent of every other given race. 
With *k* matched tokens *n*<sub>1</sub>, …, *n*<sub>k</sub>:

$$P(R \mid n_1, \ldots, n_k) \propto \frac{\prod_{i=1}^{k} P(R \mid n_i)}{P(R)^{k - 1}}$$

with *P(R)* the marginal prior — frequency-weighted across all names in
the proportion tables — exposed at `$combined$probs`. With *k* = 1 the
formula reduces to *P(R | n*<sub>1</sub>*)* (no division). The result
is renormalized to sum to 1.

Sex (`$sex$probs`) uses the same compound-first cascade against the
first-name sex table, but is computed from the **`first` field only** —
middle names are deliberately excluded. Cross-sex middle names are
common enough that combining them into P(sex) tends to mislead the
estimate. The middle name still contributes to the race combination,
just not to sex.

### Why the *(k − 1)* exponent on *P(R)*?

Starting from the published BISG-with-name-supplements formula in
Imai, Olivella, and Rosenman (2022 — the methodology companion to the
[Rosenman, Olivella, and Imai (2023) *Scientific Data* dictionaries](https://www.nature.com/articles/s41597-023-02202-2)
— building on Voicu's (2018) extension of the original BISG approach
of Elliott et al. (2008, 2009)):

$$P(R \mid F, M, S, G) \propto P(F \mid R) \cdot P(M \mid R) \cdot P(S \mid R) \cdot P(R \mid G)$$

Generalizing from {*F*, *M*, *S*} to *k* arbitrary name tokens
*n*<sub>1</sub>, …, *n*<sub>k</sub> under the same conditional-independence
assumption, and dropping geography (so *P(R | G)* collapses to *P(R)*):

$$P(R \mid n_1, \ldots, n_k) \propto \prod_{i=1}^{k} P(n_i \mid R) \cdot P(R)$$

Substituting Bayes' rule for each likelihood
*P(n*<sub>i</sub>* | R) = P(R | n*<sub>i</sub>*) · P(n*<sub>i</sub>*) / P(R)*
and dropping the *P(n*<sub>i</sub>*)* factors that are constant in *R*:

$$P(R \mid n_1, \ldots, n_k) \propto \prod_{i=1}^{k} \frac{P(R \mid n_i)}{P(R)} \cdot P(R) = \frac{\prod_{i=1}^{k} P(R \mid n_i)}{P(R)^{k - 1}}$$

Each posterior smuggles in one factor of *P(R)*; the prior in the
likelihood form covers exactly one of those, leaving *k − 1* to divide
back out:

| inputs | likelihood form | posterior form (what `$combined$probs` uses) |
|---|---|---|
| just *S* | *P(S \| R) · P(R)* | *P(R \| S)* — read directly from the CSV |
| just *F* | *P(F \| R) · P(R)* | *P(R \| F)* |
| *F* and *S* | *P(F \| R) · P(S \| R) · P(R)* | *P(R \| F) · P(R \| S) / P(R)* |
| *F*, *M*, *S* | *P(F \| R) · P(M \| R) · P(S \| R) · P(R)* | *P(R \| F) · P(R \| M) · P(R \| S) / P(R)*<sup>2</sup> |
| *k* tokens | *Π P(n*<sub>i</sub>* \| R) · P(R)* | *Π P(R \| n*<sub>i</sub>*) / P(R)*<sup>*k − 1*</sup> |

## Geography

When a geography ID is supplied, the name posterior is folded together
with the geographic prior under conditional independence given race
(BISG / BIFSG):

$$P(R \mid \mathrm{name}, G) \propto \frac{P(R \mid \mathrm{name}) \, P(R \mid G)}{P(R)}$$

`P(R)` is the population prior attached to `last_names`. With no name
input, the result collapses to `P(R | G)` directly.

### How to supply geography

Three entry points accept the same set of geography arguments. Pass
**at most one** of `zcta`, `tract`, or `block_group` per call; supplying
more than one raises an error.

| Function | Geography arguments | Population basis | Zero-cell smoothing |
|---|---|---|---|
| `geo_prior()` | `zcta=`, `tract=`, `block_group=` | `type = "cvap"` (default) or `"vap"` | `geo_smooth = 1` |
| `predict_race()` | `zcta=`, `tract=`, `block_group=`, or `geography_probs=` (length-6 named numeric in `race_groups()` order) | `geography_type = "cvap"` (default) or `"vap"` | `geo_smooth = 1` (ignored for `geography_probs`) |
| `predict_names()` | data frame columns named `zcta`, `tract`, or `block_group` (auto-detected; most specific wins if multiple) | `geography_type = "cvap"` (default) or `"vap"` | `geo_smooth = 1` |
| `predict_demog()` | same as `predict_names()`, plus `geoid` with a custom `geo_dict=` | `geography_type = "cvap"` (default) or `"vap"` | `geo_smooth = 1` (custom `geo_dict` needs a `total` column) |

### Accepted ID formats

- **ZCTA / ZIP** — 5-digit string or integer. Sub-five-digit values are
  zero-padded (`601` → `"00601"`); non-numeric characters are stripped.
- **Census Tract** — 11-digit FIPS string `state(2) + county(3) + tract(6)`,
  e.g. `"01001020100"`. The Census Summary File prefixed form
  `"1400000US01001020100"` is also accepted.
- **Block Group** — 12-digit FIPS string `state(2) + county(3) + tract(6) + bg(1)`,
  e.g. `"010010201001"`. The prefixed form `"1500000US010010201001"`
  is also accepted.

IDs that don't match any row in the bundled table return `NULL` from
`geo_prior()`; in `predict_race()` the result falls back to the
name-only posterior and `$geography$found` is set to `FALSE`. In
`predict_names()` the affected row likewise falls back to the name-only
posterior (the 7-column output carries no per-row match indicator).

### Bundled datasets, sources, and vintages

The package ships six lazy-loaded geographic-prior tables, one for each
combination of geography level × population basis. All six have the same
columns: `geoid`, `total`, and the six race / Hispanic-origin shares
(`white`, `black`, `aian`, `aapi`, `nh_multi`, `hispanic`) summing to 1
per row.

| Object | Level | Basis | Source | Reference period |
|---|---|---|---|---|
| `geo_zcta_cvap` | ZCTA (5-digit) | CVAP — citizens 18+ | 2020-2024 ACS CVAP Special Tabulation, apportioned from tract to ZCTA via the Census 2020 ZCTA-to-Tract relationship file (the special tab does not publish ZCTA directly) | 2020-2024 ACS 5-year |
| `geo_tract_cvap` | Census Tract (11-digit) | CVAP | 2020-2024 ACS CVAP Special Tabulation, tract level | 2020-2024 ACS 5-year |
| `geo_bg_cvap` | Block Group (12-digit) | CVAP | 2020-2024 ACS CVAP Special Tabulation, block-group level | 2020-2024 ACS 5-year |
| `geo_zcta_vap` | ZCTA | VAP — everyone 18+ | 2020 Decennial DHC, Table P11 (ZCTA) | April 1, 2020 |
| `geo_tract_vap` | Census Tract | VAP | 2020 Decennial DHC, Table P11 (tract) | April 1, 2020 |
| `geo_bg_vap` | Block Group | VAP | 2020 Decennial DHC, Table P11 (block group) | April 1, 2020 |

The geography vintage is fixed by the bundled data — to use a different
ACS or Decennial release, pass your own table via
`predict_race(geography_probs = ...)` (or rebuild the `.rda` files; see
**Rebuilding the bundled data** below).

### Choosing CVAP vs VAP

- **CVAP** (Citizen Voting Age Population) — citizens age 18+ only.
  Appropriate for predictions about likely voters, e.g. matching against
  a voter file or registration list.
- **VAP** (Voting Age Population) — everyone age 18+ (citizens and
  non-citizens). Appropriate when the bearer's citizenship status is
  unknown and you want the broadest 18+ denominator.

CVAP is the default. The two bases can give materially different priors
in geographies with large non-citizen populations.

### Zero cells and `geo_smooth`

The BISG fold divides by `P(R)` and multiplies by `P(R | G)`, so a cell
of `P(R | G)` that is **exactly zero** drives that group's posterior to
zero no matter how decisive the name is. The bundled tables are full of
such cells at fine geographies:

| share of rows with an exact zero | white | black | aian | aapi | nh_multi | hispanic |
|---|---|---|---|---|---|---|
| `geo_bg_cvap` | 3.4% | 36.2% | 86.0% | 49.1% | 35.1% | 19.8% |
| `geo_tract_cvap` | 1.4% | 15.1% | 70.5% | 26.2% | 9.3% | 4.8% |
| `geo_zcta_cvap` | 0.1% | 13.8% | 32.8% | 19.3% | 1.9% | 2.9% |
| `geo_bg_vap` | 0.2% | 5.2% | 31.1% | 7.7% | 0.3% | 0.4% |

These are overwhelmingly **sampling** zeros rather than structural ones.
The CVAP Special Tabulation is an ACS estimate, so a group with nobody
in the sample for a block group is published as zero even where the true
count is not — 42% of the national CVAP lives in a block group whose
published Asian / NHPI count is zero. Taken at face value the zero is
fatal, and the displaced mass lands on whichever surviving group has the
smallest marginal prior (usually `nh_multi`, whose `P(R)` of 0.033 gives
it the largest `/P(R)` boost).

`geo_smooth` shrinks each looked-up composition toward the
population-weighted national marginal of the same table, with a
pseudo-count of `geo_smooth` people — a Dirichlet(`geo_smooth` ×
national) prior on the geography's composition:

$$p_{\mathrm{smooth}} = \frac{\mathrm{total} \times p_G + \alpha \, p_{\mathrm{national}}}{\mathrm{total} + \alpha}$$

At the default `alpha = 1`, one pseudo-person moves a populated cell by
well under a tenth of a percentage point, but turns an exact zero into a
small positive share — so geography still weighs heavily against the
group, just not infinitely. Pass `geo_smooth = 0` for the published
shares unchanged.

```r
# Three name tokens that are each 95-98% AAPI in the Census tables, in
# a block group whose published CVAP has no Asian / NHPI citizens.
df <- data.frame(first = "WEI", middle = "MINH", last = "NGUYEN",
                 block_group = "010010201001")

predict_names(df, geo_smooth = 0)
#> p_aapi = 0.000   p_nh_multi = 0.996   <- the zero cell wins outright
predict_names(df)
#> p_aapi = 0.997   p_nh_multi = 0.003   <- default geo_smooth = 1
```

Over a 10,000-row sample of names joined to block groups, smoothing
removes a hard zero from 95% of rows while changing the modal group for
only 0.24% of them; the median per-row change is 1e-4.

### Relation to `wru`'s fBISG

This is the same correction that `wru`'s `model = "fBISG"` applies to the
geography term, arrived at from the same conjugate structure. Imai,
Olivella & Rosenman (2022) model the published counts as
`N_g ~ Multinomial(N_g, ζ_g)` over a geography's unknown true composition
`ζ_g`, put a `Dirichlet(α)` prior on `ζ_g`, and replace BISG's
`N_rg / Σ N_r'g` with `(n⁻ⁱ_rg + N_rg + α_r) / Σ_r'(n⁻ⁱ_r'g + N_r'g + α_r')`.
`geo_smooth` is that expression without the `n⁻ⁱ_rg` term:

- **α points differently.** `wru` uses a uniform `α = 1` on every
  category. `geo_smooth` distributes its pseudo-count along the national
  marginal, so rare groups are not floored at the same level as common
  ones and the result does not depend on how finely the categories are
  split.
- **No pooling across records.** `n⁻ⁱ_rg` counts the *other records in
  your own input* currently assigned to race `r` in geography `g`, which
  is what lets fBISG conclude that the Census undercounted a group in a
  place. `geo_smooth` cannot do that, and deliberately does not try: it
  would require holding every observation for a geography at once, which
  is exactly what this package's per-record and streaming-friendly
  interfaces are built to avoid. `geo_smooth` only declines to believe an
  exact zero.
- **Closed form.** fBISG needs a Gibbs sampler (`wru`'s `control` takes
  `iter`, `burnin`, `me.correct`), so it is stochastic and costs
  `O(iter × n)`. `geo_smooth` is a deterministic one-line adjustment to
  the prior with no measurable runtime.

If you need the pooled estimate and can afford the sampler, use `wru`'s
fBISG. `geo_smooth` is the cheap fix for the failure mode that motivates
it.

Name-table zeros are a separate matter and are **not** smoothed: those
come from published Census counts rather than survey estimates, and a
zero there still zeroes the group. Around 38% of first-name rows and 62%
of surname rows carry at least one zero cell, so a token whose count for
some group is zero rules that group out on its own.

### Examples

```r
# Standalone geographic prior P(R | G).
geo_prior(zcta  = "30307")                  # CVAP, default
geo_prior(tract = "01001020100", type = "vap")

# BISG: name + geography, single call.
predict_race(first = "Maria", last = "Garcia",
             zcta  = "30307")               # P(R | name, ZCTA)
predict_race(last  = "Smith",
             block_group = "010010201001",
             geography_type = "vap")

# Geography only (no name): collapses to P(R | G).
predict_race(zcta = "00601")

# Bring your own prior (e.g. from a different ACS vintage).
my_prior <- c(white = 0.40, black = 0.20, aian = 0.01,
              aapi  = 0.10, nh_multi = 0.04, hispanic = 0.25)
predict_race(first = "Jose", last = "Lopez",
             geography_probs = my_prior)

# Vectorized: name the geography column `zcta` (or `tract` /
# `block_group`) and predict_names() picks it up automatically.
df$zcta <- c("30307", "10001", "94110")
predict_names(df, geography_type = "cvap")
#> 7-col data frame: p_white..p_hispanic (BISG when matched) + p_female.
```

## Rebuilding the bundled data

The `.rda` files in `data/` are generated from the proportion CSVs at the
repo root. Regenerate after the CSVs change:

```bash
cd openBISG/data-raw
Rscript build_data.R     # rebuilds first_names / last_names / etc.
Rscript build_geo.R      # rebuilds geo_zcta_* / geo_tract_* / geo_bg_*
```

## References

### Census source data

- Comenetz, J. (2026). *First Name Data From the 2020 Census.* 2020
  Census Briefs, C2020BR-13. U.S. Census Bureau.
- Comenetz, J. (2026). *Last Name Data From the 2020 Census.* 2020
  Census Briefs, C2020BR-14. U.S. Census Bureau.
- Comenetz, J. (2016). *Frequently Occurring Surnames in the 2010
  Census.* Technical Report, U.S. Census Bureau.
  <https://www2.census.gov/topics/genealogy/2010surnames/surnames.pdf>
- Word, D. L., Coleman, C. D., Nunziata, R., & Kominski, R. (2007).
  *Demographic Aspects of Surnames from Census 2000.* U.S. Census
  Bureau.
  <https://www2.census.gov/topics/genealogy/2000surnames/surnames.pdf>
- U.S. Census Bureau (2021). *2020 Census Redistricting Data
  (Public Law 94-171) Summary File* — Table P2 (Hispanic or Latino,
  and Not Hispanic or Latino by Race). Used for the AAPI / NHPI
  biracial population counts cited above.
  <https://api.census.gov/data/2020/dec/pl>

### Bayesian Improved Surname Geocoding (BISG) and name supplements

- Elliott, M. N., Fremont, A., Morrison, P. A., Pantoja, P., & Lurie,
  N. (2008). A new method for estimating race/ethnicity and associated
  disparities where administrative records lack self-reported
  race/ethnicity. *Health Services Research* 43, 1722–1736.
- Elliott, M. N., Morrison, P. A., Fremont, A., McCaffrey, D. F.,
  Pantoja, P., & Lurie, N. (2009). Using the Census Bureau's surname
  list to improve estimates of race/ethnicity and associated
  disparities. *Health Services and Outcomes Research Methodology* 9,
  69–83.
- Fiscella, K., & Fremont, A. M. (2006). Use of geocoding and surname
  analysis to estimate race and ethnicity. *Health Services Research*
  41, 1482–1500.
- Voicu, I. (2018). Using first name information to improve race and
  ethnicity classification. *Statistics and Public Policy* 5, 1–13.
- Imai, K., & Khanna, K. (2016). Improving ecological inference by
  predicting individual ethnicity from voter registration records.
  *Political Analysis* 24, 263–272.
- Imai, K., Olivella, S., & Rosenman, E. T. R. (2022). Addressing
  Census data problems in race imputation via fully Bayesian Improved
  Surname Geocoding and name supplements. *Science Advances* 8,
  eadc9824. ([arXiv:2205.06129](https://arxiv.org/abs/2205.06129)) —
  the methodology paper from which the (k − 1) prior-division formula
  is derived.
- Rosenman, E. T. R., Olivella, S., & Imai, K. (2023). Race and
  ethnicity data for first, middle, and surnames. *Scientific Data*
  10, 299.
  <https://www.nature.com/articles/s41597-023-02202-2> — voter-file
  dictionaries publishing both *P(race | name)* and *P(name | race)*
  for first, middle, and last names.

### Software

- Khanna, K., Imai, K., Rosenman, E. T. R., & Olivella, S. (2021).
  *wru: Who are You? Bayesian Prediction of Racial Category Using
  Surname and Geolocation.* R package.
  <https://github.com/kosukeimai/wru> — origin of the normalization
  cascade ported here.
