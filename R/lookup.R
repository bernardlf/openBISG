## Internal cache for per-table lookup environments. Each entry is an
## environment mapping uppercase name -> integer row index in the table.
.openBISG_caches <- new.env(parent = emptyenv())

table_df <- function(table) {
  ns <- asNamespace("openBISG")
  switch(
    table,
    first       = get("first_names",       envir = ns),
    last        = get("last_names",        envir = ns),
    first_sex   = get("first_names_sex",   envir = ns),
    first_extra = get("first_names_extra", envir = ns),
    last_extra  = get("last_names_extra",  envir = ns),
    stop("Unknown table: ", table, call. = FALSE)
  )
}

## Census tables that have a Rosenman, Olivella, and Imai (2023) NotInCensus2020
## complement. Used by `expand_extra_tables()` to extend a Census-only chain
## with the corresponding voter-file extras after every Census table missed.
.extra_pairs <- c(first = "first_extra", last = "last_extra")

expand_extra_tables <- function(tables) {
  pairs <- unname(.extra_pairs[intersect(tables, names(.extra_pairs))])
  c(tables, pairs)
}

lookup_env <- function(table) {
  key <- paste0(table, "_env")
  cached <- get0(key, envir = .openBISG_caches, inherits = FALSE, ifnotfound = NULL)
  if (!is.null(cached)) return(cached)
  df <- table_df(table)
  e <- list2env(
    stats::setNames(as.list(seq_len(nrow(df))), df$name),
    envir = new.env(parent = emptyenv(), hash = TRUE, size = nrow(df))
  )
  assign(key, e, envir = .openBISG_caches)
  e
}

make_hit <- function(df, idx, matched_as, rule) {
  groups <- attr(df, "groups")
  list(
    probs      = stats::setNames(as.numeric(df[idx, groups]), groups),
    matched_as = matched_as,
    rule       = rule,
    frequency  = df$frequency[idx]
  )
}

#' Tokenize a name input into individual name tokens
#'
#' Accepts either a length-one string (whitespace-tokenized) or an
#' arbitrary character vector (treated as already-tokenized). Empty or
#' whitespace-only entries are dropped. Hyphenated tokens like
#' `MARIA-JOSE` are kept intact — the cascade in [lookup_name()] handles
#' them via punctuation stripping or segment splitting.
#'
#' @param x Length-one character vector, or character vector of any length.
#' @return A character vector (possibly empty).
#' @export
tokenize_names <- function(x) {
  if (is.null(x)) return(character(0))
  if (!is.character(x)) x <- as.character(x)
  if (length(x) == 0L) return(character(0))
  x <- x[!is.na(x)]
  if (length(x) == 1L) {
    parts <- unlist(strsplit(x, "\\s+", perl = TRUE), use.names = FALSE)
  } else {
    parts <- x
  }
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

#' Look up a name token in one or more tables, with optional fallback
#'
#' Tries each table in order, returning the first hit. Each table is
#' looked up with the full normalization cascade in [lookup_name()].
#' The returned hit carries two extra elements: `source` naming the
#' Census-style table key (`"first"`, `"last"`, or `"first_sex"`) and
#' `dataset` naming the underlying source (`"census"` for the bundled
#' 2020 Census tables or `"rosenman"` for the Rosenman, Olivella, and Imai
#' (2023) NotInCensus2020 voter-file additions).
#'
#' @param name Length-one character vector.
#' @param tables Character vector of table keys to try in order
#'   (`"first"`, `"last"`, or `"first_sex"`). Default tries first then
#'   last (the conventional given-name fallback).
#' @param include_extra If `TRUE`, after every Census table in `tables`
#'   has missed, fall back to the corresponding Rosenman, Olivella, and Imai
#'   (2023) NotInCensus2020 table (`first_extra` / `last_extra`). The
#'   Rosenman tables only cover names that are absent from Census 2020,
#'   so a Census-table hit always wins over a Rosenman hit. Default
#'   `FALSE`.
#' @return `NULL` if every table missed, otherwise the [lookup_name()]
#'   result with `source` and `dataset` appended.
#' @export
lookup_with_fallback <- function(name,
                                 tables = c("first", "last"),
                                 include_extra = FALSE) {
  if (isTRUE(include_extra)) tables <- expand_extra_tables(tables)
  for (tab in tables) {
    hit <- lookup_name(name, table = tab)
    if (!is.null(hit)) {
      is_extra <- grepl("_extra$", tab)
      hit$source  <- sub("_extra$", "", tab)
      hit$dataset <- if (is_extra) "rosenman" else "census"
      return(hit)
    }
  }
  NULL
}

#' Look up a field that may be a single compound name or several tokens
#'
#' Tries the entire input as a single name first — passing through the
#' [lookup_name()] cascade, which strips internal spaces and so will
#' catch the Census-collapsed compound forms (e.g. `"MARY ANN"` →
#' `MARYANN`, `"MARIA JOSE"` → `MARIAJOSE`). If the whole-string
#' lookup misses, falls back to per-token lookup with the same fallback
#' chain.
#'
#' This matches `wru`'s "try whole, then split" behavior on a single
#' name field, but extended so that **all** tokens of a multi-token
#' field are looked up (and combined later) when the compound form is
#' absent — `wru` only looks at the first two halves.
#'
#' @param tokens Character vector of name tokens (already tokenized).
#' @param tables Tables to try in order — see [lookup_with_fallback()].
#' @param include_extra Forwarded to [lookup_with_fallback()] — when
#'   `TRUE`, Census misses fall through to the Rosenman, Olivella, and Imai
#'   (2023) NotInCensus2020 voter-file tables.
#' @return Named list of hits/NULLs. When the whole-string match
#'   succeeds, returns a single-element list keyed by the joined form;
#'   otherwise returns one element per token, keyed by token. The
#'   length of the result tells the caller how many evidence pieces to
#'   feed into the Naive-Bayes combination.
#' @export
lookup_compound_or_tokens <- function(tokens,
                                      tables = c("first", "last"),
                                      include_extra = FALSE) {
  if (length(tokens) == 0L) return(list())
  if (length(tokens) == 1L) {
    hit <- lookup_with_fallback(tokens, tables = tables,
                                include_extra = include_extra)
    return(stats::setNames(list(hit), tokens))
  }
  ## Multi-token: try the joined whole-string first, but only accept
  ## matches that did not come from the cascade's segment-split rule —
  ## a segment match means the compound row is absent and only one
  ## piece survived, so falling through to per-token lookup recovers
  ## evidence from every token instead of dropping all but one.
  joined <- paste(tokens, collapse = " ")
  hit <- lookup_with_fallback(joined, tables = tables,
                              include_extra = include_extra)
  if (!is.null(hit) && !grepl("segment", hit$rule, fixed = TRUE)) {
    return(stats::setNames(list(hit), joined))
  }
  hits <- lapply(tokens, function(t) lookup_with_fallback(
    t, tables = tables, include_extra = include_extra
  ))
  stats::setNames(hits, tokens)
}

#' Look up a name in one of the bundled Census tables
#'
#' Applies the same wru-style normalization cascade as the bundled
#' `index.html` lookup page, returning the first table hit. The cascade is:
#'
#' 1. **exact** — [normalize_name()] then exact match;
#' 2. **punctuation removed** — drop non-alphanumerics except spaces;
#' 3. **punctuation and spaces removed** — also drop spaces;
#' 4. **generational suffix removed** (last names only) — strip JR, SR, II,
#'    III, IV, JUNIOR, SENIOR, THIRD;
#' 5. **first / second segment of multi-part name** — split on hyphen, comma,
#'    or space and try the first then the second piece.
#'
#' @param name A length-one character vector. `NA`, empty, or whitespace-only
#'   inputs return `NULL`.
#' @param table One of `"first"`, `"last"`, `"first_sex"`,
#'   `"first_extra"`, or `"last_extra"`. The two `_extra` tables are
#'   the Rosenman, Olivella, and Imai (2023) NotInCensus2020 voter-file
#'   subsets; they contain only names that are absent from the
#'   corresponding Census table, so a single-table lookup against an
#'   `_extra` table only returns a hit for those rare names. For most
#'   callers it is more convenient to set `include_extra = TRUE` on
#'   [lookup_with_fallback()], [lookup_compound_or_tokens()],
#'   [predict_race()], or [predict_names()] than to call
#'   `lookup_name()` directly with an `_extra` table.
#' @return `NULL` if no rule matched, otherwise a list with components:
#'   \describe{
#'     \item{probs}{Named numeric vector of group proportions for the matched
#'       row.}
#'     \item{matched_as}{The form of the name that hit the table.}
#'     \item{rule}{Which cascade rule matched (`"exact"`, `"punctuation
#'       removed"`, `"punctuation and spaces removed"`, `"generational suffix
#'       removed"`, `"first segment of multi-part name"`, or `"second segment
#'       of multi-part name"`).}
#'     \item{frequency}{The Census-published `FREQUENCY (COUNT)` for the
#'       matched row.}
#'   }
#' @export
lookup_name <- function(name,
                        table = c("first", "last", "first_sex",
                                  "first_extra", "last_extra")) {
  table <- match.arg(table)
  if (is.null(name) || !is.character(name) || length(name) != 1L) return(NULL)
  if (is.na(name) || !nzchar(name)) return(NULL)

  initial <- normalize_name(name)
  if (!nzchar(initial)) return(NULL)

  env <- lookup_env(table)
  df  <- table_df(table)
  is_last <- table %in% c("last", "last_extra")

  ## Step 1: exact
  idx <- get0(initial, envir = env, inherits = FALSE, ifnotfound = NA_integer_)
  if (!is.na(idx)) return(make_hit(df, idx, initial, "exact"))

  ## Step 2: punctuation removed (keep spaces)
  no_punct <- gsub("[^A-Z0-9 ]", "", initial, perl = TRUE)
  if (no_punct != initial && nzchar(no_punct)) {
    idx <- get0(no_punct, envir = env, inherits = FALSE, ifnotfound = NA_integer_)
    if (!is.na(idx)) return(make_hit(df, idx, no_punct, "punctuation removed"))
  }

  ## Step 3: punctuation and spaces removed
  no_space <- gsub(" ", "", no_punct, fixed = TRUE)
  if (no_space != no_punct && nzchar(no_space)) {
    idx <- get0(no_space, envir = env, inherits = FALSE, ifnotfound = NA_integer_)
    if (!is.na(idx)) return(make_hit(df, idx, no_space, "punctuation and spaces removed"))
  }

  ## Step 4: generational suffix removed (last names only)
  if (is_last) {
    desuffixed <- strip_last_name_suffix(no_space)
    if (desuffixed != no_space && nzchar(desuffixed)) {
      idx <- get0(desuffixed, envir = env, inherits = FALSE, ifnotfound = NA_integer_)
      if (!is.na(idx)) return(make_hit(df, idx, desuffixed, "generational suffix removed"))
    }
  }

  ## Step 5: split on hyphen / comma / space, try first then second segment
  if (grepl("[-, ]", initial)) {
    parts <- Filter(nzchar, strsplit(initial, "[-, ]+", perl = TRUE)[[1]])
    if (length(parts) >= 1L && parts[1L] != initial) {
      idx <- get0(parts[1L], envir = env, inherits = FALSE, ifnotfound = NA_integer_)
      if (!is.na(idx)) return(make_hit(df, idx, parts[1L], "first segment of multi-part name"))
    }
    if (length(parts) >= 2L) {
      idx <- get0(parts[2L], envir = env, inherits = FALSE, ifnotfound = NA_integer_)
      if (!is.na(idx)) return(make_hit(df, idx, parts[2L], "second segment of multi-part name"))
    }
  }

  NULL
}
