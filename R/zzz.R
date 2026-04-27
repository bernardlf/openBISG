## Suppress R CMD check NOTEs about lazy-loaded data referenced in package
## code by their bare names.
utils::globalVariables(c(
  "first_names", "last_names", "first_names_sex",
  "first_names_extra", "last_names_extra"
))
