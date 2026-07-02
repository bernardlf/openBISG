## Minimal single-line text progress bar on stderr, shared by
## predict_names() and predict_demog(). `start()` draws the empty bar,
## `tick()` advances by one unit (drawing is throttled to ~1%
## increments), `done()` terminates the line.
make_progress_bar <- function(total, width = 30L) {
  t0 <- Sys.time()
  i  <- 0L
  step <- max(1L, total %/% 100L)
  draw <- function() {
    frac    <- if (total > 0L) i / total else 1
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    eta     <- if (frac > 0) elapsed * (1 - frac) / frac else 0
    filled  <- floor(width * frac)
    arrow   <- filled < width
    bar <- paste0(
      "|",
      strrep("=", filled),
      if (arrow) ">" else "",
      strrep(" ", max(0L, width - filled - as.integer(arrow))),
      "|"
    )
    cat(sprintf("\r%s %3d%% (%.0fs elapsed, ~%.0fs remaining)   ",
                bar, round(100 * frac), elapsed, eta),
        file = stderr())
  }
  list(
    start = draw,
    tick  = function() {
      i <<- i + 1L
      if (i == total || i %% step == 0L) draw()
    },
    done  = function() cat("\n", file = stderr())
  )
}

## Validate and cap the `n_cores` argument shared by predict_names() and
## predict_demog(). Fork-based parallelism is unavailable on Windows, so
## values above 1 fall back to serial there.
resolve_n_cores <- function(n_cores, n) {
  if (!is.numeric(n_cores) || length(n_cores) != 1L || is.na(n_cores) ||
      n_cores < 1L) {
    stop("`n_cores` must be a single positive integer.", call. = FALSE)
  }
  n_cores <- as.integer(n_cores)
  if (.Platform$OS.type == "windows") return(1L)
  n_cores <- min(n_cores, max(1L, n))
  if (n_cores > 1L) {
    n_cores <- min(n_cores, parallel::detectCores(logical = TRUE))
  }
  n_cores
}
