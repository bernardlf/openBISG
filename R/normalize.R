#' Normalize a raw name to its initial Census-table form
#'
#' Trims, NFD-decomposes and drops combining marks (so `PEÑA` matches `PENA`),
#' then upper-cases. This is the first step of the matching cascade in
#' [lookup_name()] and is exposed for users who want to apply it themselves.
#'
#' @param x A length-one character vector. `NA`, `NULL`, or empty strings
#'   return `""`.
#' @return A length-one character vector, possibly empty.
#' @examples
#' normalize_name("oconnor")  # "OCONNOR"
#' normalize_name("  Smith ") # "SMITH"
#' normalize_name(NA)         # ""
#' @export
normalize_name <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  if (is.na(x)) return("")
  x <- stringi::stri_trim_both(x)
  if (!nzchar(x)) return("")
  x <- stringi::stri_trans_nfd(x)
  x <- stringi::stri_replace_all_regex(x, "\\p{Mn}", "")
  stringi::stri_trans_toupper(x)
}

## Generational suffixes stripped from last names. Order matters: longer
## variants must come first so JUNIOR isn't shortened to JR's stem first.
LAST_NAME_SUFFIXES_NOSPACE <- c("JUNIOR", "SENIOR", "THIRD", "III", "IV", "JR", "II")

strip_last_name_suffix <- function(name) {
  n <- nchar(name)
  for (suf in LAST_NAME_SUFFIXES_NOSPACE) {
    s <- nchar(suf)
    if (n > s && substr(name, n - s + 1L, n) == suf) {
      return(substr(name, 1L, n - s))
    }
  }
  ## "SR" is only stripped when the suffixed form is at least 7 characters,
  ## so short names like "ASR" or "USR" aren't accidentally truncated.
  if (n >= 7L && substr(name, n - 1L, n) == "SR") {
    return(substr(name, 1L, n - 2L))
  }
  name
}
