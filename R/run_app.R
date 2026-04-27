#' Launch the bundled Shiny lookup app
#'
#' Starts the Shiny app shipped in `inst/shiny/app.R`, which mirrors the
#' `index.html` page in the parent repo: a first/last name input, the
#' per-name race / Hispanic-origin probabilities for each side, the combined
#' probability under conditional independence, and the `P(sex | first name)`
#' table.
#'
#' @param ... Passed to [shiny::runApp()]. Common choices: `port`, `host`,
#'   `launch.browser`.
#' @return Called for its side effect of running a Shiny app; the
#'   underlying `shiny::runApp()` returns invisibly.
#' @examples
#' \dontrun{
#' run_app()                                    # blocks until the app closes
#' run_app(port = 4321, launch.browser = FALSE) # custom port, no browser
#' }
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The 'shiny' package is required to run the app. Install it with ",
      "`install.packages(\"shiny\")`.",
      call. = FALSE
    )
  }
  app_dir <- system.file("shiny", package = "openBISG")
  if (!nzchar(app_dir)) {
    stop("Could not find the bundled Shiny app directory.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
