## openBISG Shiny app
## Launched via openBISG::run_app().

library(shiny)
library(openBISG)

`%||%` <- function(a, b) if (is.null(a)) b else a

race_keys   <- race_groups()
race_labels <- race_group_labels()
sex_keys    <- sex_groups()
sex_labels  <- sex_group_labels()

fmt_pct <- function(x) {
  if (!is.finite(x)) return("—")
  pct <- x * 100
  if (pct >= 10) return(sprintf("%.1f%%", pct))
  if (pct >= 1)  return(sprintf("%.2f%%", pct))
  sprintf("%.3f%%", pct)
}

prob_row <- function(label, p) {
  pct <- max(0, min(1, ifelse(is.finite(p), p, 0))) * 100
  tags$tr(
    tags$td(label),
    tags$td(class = "bar-cell",
            tags$div(class = "bar",
                     tags$div(style = sprintf("width:%.2f%%", pct)))),
    tags$td(class = "num", fmt_pct(p))
  )
}

prob_table <- function(title, probs, label_map, column_header, note = NULL) {
  ord <- order(probs, decreasing = TRUE)
  rows <- lapply(ord, function(i) prob_row(label_map[[names(probs)[i]]], probs[i]))
  tagList(
    div(class = "card",
        h2(title),
        tags$table(
          tags$thead(tags$tr(tags$th(column_header), tags$th(""), tags$th(class = "num", "Probability"))),
          tags$tbody(rows)
        ),
        if (!is.null(note)) p(class = "meta", HTML(note))
    )
  )
}

token_note <- function(token, hit) {
  if (is.null(hit)) {
    return(sprintf("<span class='error'><strong>%s</strong> — not found in either name table.</span>",
                   token))
  }
  table_label <- sprintf(
    "%s-names%s table",
    ifelse(hit$source == "first", "first", "last"),
    if (identical(hit$dataset, "rosenman")) " (Rosenman et al. 2023)" else ""
  )
  parts <- c(
    sprintf("matched in <strong>%s</strong>", table_label),
    if (!identical(hit$rule, "exact"))
      sprintf("as <code>%s</code> (%s)", hit$matched_as, hit$rule)
  )
  sprintf("<strong>%s</strong> — %s.", token, paste(parts, collapse = ", "))
}

token_section <- function(label, token_hits) {
  if (length(token_hits) == 0L) return(NULL)
  notes <- vapply(seq_along(token_hits),
                  function(i) token_note(names(token_hits)[i], token_hits[[i]]),
                  character(1))
  div(class = "card",
      h2(label),
      HTML(paste0("<ul class='token-list'>",
                  paste0("<li>", notes, "</li>", collapse = ""),
                  "</ul>"))
  )
}

app_css <- "
:root {
  --fg: #1a1a1a; --muted: #666; --bg: #fafafa; --card: #fff;
  --border: #ddd; --accent: #2b6cb0; --bar: #2b6cb0; --bar-bg: #e6eef7;
}
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  color: var(--fg); background: var(--bg);
  max-width: 880px; margin: 0 auto; padding: 24px 16px 64px; line-height: 1.45;
}
h1 { font-size: 1.4rem; margin: 0 0 4px; }
p.sub { color: var(--muted); margin: 0 0 24px; }
.card { background: var(--card); border: 1px solid var(--border);
        border-radius: 8px; padding: 16px; margin-bottom: 16px; }
.row { display: flex; gap: 12px; flex-wrap: wrap; align-items: end; }
label { display: block; font-size: 0.85rem; color: var(--muted); margin-bottom: 4px; }
input[type='text'] { font-size: 1rem; padding: 8px 10px; border: 1px solid var(--border);
                     border-radius: 6px; width: 280px; box-sizing: border-box; }
table { width: 100%; border-collapse: collapse; font-size: 0.92rem; }
th, td { text-align: left; padding: 6px 8px; border-bottom: 1px solid #eee; vertical-align: middle; }
th { color: var(--muted); font-weight: 500; font-size: 0.82rem; }
td.num { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
.bar-cell { width: 220px; }
.bar { height: 10px; background: var(--bar-bg); border-radius: 5px; overflow: hidden; }
.bar > div { height: 100%; background: var(--bar); }
.error { color: #b00020; }
.meta { font-size: 0.85rem; color: var(--muted); margin-top: 8px; }
h2 { font-size: 1rem; margin: 0 0 8px; }
.shiny-input-container { width: auto; margin-bottom: 0; }
.action-button { font-size: 0.95rem; padding: 8px 16px; border: 1px solid var(--accent);
                 background: var(--accent); color: #fff; border-radius: 6px; cursor: pointer; }
.token-list { margin: 0; padding-left: 20px; font-size: 0.92rem; }
.token-list li { margin-bottom: 4px; }
"

ui <- fluidPage(
  tags$head(
    tags$title("Name → Race/Hispanic-origin Probabilities (2020 Census)"),
    tags$style(HTML(app_css))
  ),
  h1("Name → Race/Hispanic-origin Probabilities"),
  p(class = "sub",
    "Based on the 2020 U.S. Census frequently occurring first and last names. ",
    "First and middle name fields try the whole input as a compound name first ",
    "(catching Census-collapsed forms like MARYANN, MARIAJOSE), then fall back ",
    "to combining each token. Maiden name, when supplied, replaces the last ",
    "name in the combined estimate."),

  div(class = "card",
      div(class = "row",
          div(textInput("first",  "First name(s)",
                        placeholder = "e.g. MARIA or MARIA JOSE")),
          div(textInput("middle", "Middle name(s)",
                        placeholder = "e.g. ANN")),
          div(textInput("last",   "Last name(s)",
                        placeholder = "e.g. GARCIA LOPEZ")),
          div(textInput("maiden", "Maiden name(s)",
                        placeholder = "(replaces last name if given)")),
          actionButton("go", "Compute", class = "action-button")
      ),
      div(class = "row", style = "margin-top: 10px;",
          div(textInput("zcta",  "ZIP / ZCTA",
                        placeholder = "e.g. 00601")),
          div(textInput("tract", "Census Tract FIPS",
                        placeholder = "11 digits, e.g. 01001020100")),
          div(textInput("block_group", "Census Block Group FIPS",
                        placeholder = "12 digits, e.g. 010010201001")),
          div(radioButtons("geography_type", "Population basis",
                           choices = c("CVAP (citizens 18+)" = "cvap",
                                       "VAP (everyone 18+)"  = "vap"),
                           selected = "cvap", inline = TRUE))
      ),
      div(class = "row", style = "margin-top: 10px;",
          checkboxInput(
            "include_extra",
            HTML(paste(
              "Fall back to the <strong>Rosenman, Olivella, and Imai (2023)</strong>",
              "voter-file additions (NotInCensus2020) when a name does not",
              "match the 2020 Census tables"
            )),
            value = FALSE
          )
      )
  ),

  uiOutput("results"),

  p(class = "meta", HTML(paste(
    "Each given-name field is first looked up as a single compound name;",
    "if absent, the field is split on whitespace and each token is looked",
    "up separately. Each given-name token falls back to the last-name",
    "table on miss; each surname token falls back to the first-name table.",
    "Across <em>k</em> matched tokens the combination is",
    "<em>P(g | n<sub>1</sub>, …, n<sub>k</sub>) ∝ Π P(g | n<sub>i</sub>) /",
    "P(g)<sup>k − 1</sup></em>. Sex uses the same compound-first cascade",
    "against the first-name sex table, but is computed from the",
    "<strong>first-name field only</strong> — middle names are",
    "deliberately excluded because cross-sex middle names are common",
    "and tend to mislead."
  )))
)

server <- function(input, output, session) {
  result <- eventReactive(input$go, {
    list(first           = input$first,
         middle          = input$middle,
         last            = input$last,
         maiden          = input$maiden,
         include_extra   = isTRUE(input$include_extra),
         zcta            = input$zcta,
         tract           = input$tract,
         block_group     = input$block_group,
         geography_type  = input$geography_type %||% "cvap")
  }, ignoreNULL = FALSE)

  output$results <- renderUI({
    r <- result()
    raw <- function(x) if (is.null(x)) "" else trimws(x)
    f_raw <- raw(r$first); m_raw <- raw(r$middle)
    l_raw <- raw(r$last);  d_raw <- raw(r$maiden)
    z_raw <- raw(r$zcta);  tr_raw <- raw(r$tract);  bg_raw <- raw(r$block_group)
    if (!nzchar(f_raw) && !nzchar(m_raw) && !nzchar(l_raw) && !nzchar(d_raw) &&
        !nzchar(z_raw) && !nzchar(tr_raw) && !nzchar(bg_raw)) {
      return(div(class = "card meta",
                 "Enter at least one name token in any field, or a geography ID."))
    }
    geo_supplied <- nzchar(z_raw) + nzchar(tr_raw) + nzchar(bg_raw)
    if (geo_supplied > 1L) {
      return(div(class = "card error",
                 "Provide at most one of ZIP / ZCTA, Census Tract, or Block Group."))
    }
    name_supplied <- nzchar(f_raw) || nzchar(m_raw) || nzchar(l_raw) || nzchar(d_raw)
    if (!name_supplied && geo_supplied == 0L) {
      return(div(class = "card meta",
                 "Enter at least one name token in any field, or a geography ID."))
    }

    pred <- tryCatch(
      predict_race(
        first          = if (nzchar(f_raw))  f_raw  else NULL,
        middle         = if (nzchar(m_raw))  m_raw  else NULL,
        last           = if (nzchar(l_raw))  l_raw  else NULL,
        maiden         = if (nzchar(d_raw))  d_raw  else NULL,
        include_extra  = isTRUE(r$include_extra),
        zcta           = if (nzchar(z_raw))  z_raw  else NULL,
        tract          = if (nzchar(tr_raw)) tr_raw else NULL,
        block_group    = if (nzchar(bg_raw)) bg_raw else NULL,
        geography_type = r$geography_type
      ),
      error = function(e) NULL
    )
    if (is.null(pred)) {
      return(div(class = "card error",
                 "Enter at least one name token in any field."))
    }

    sections <- list()

    ## Per-field token lookup summaries.
    field_labels <- list(
      first  = "First name token lookups",
      middle = "Middle name token lookups",
      last   = "Last name token lookups",
      maiden = "Maiden name token lookups"
    )
    for (field in c("first", "middle", "last", "maiden")) {
      hits <- pred$tokens[[field]]
      if (length(hits) == 0L) next
      label <- field_labels[[field]]
      ## Annotate ignored last-name lookups.
      if (field == "last" && !is.null(pred$surname_used) &&
          pred$surname_used == "maiden") {
        label <- paste0(label, " (not used — maiden name takes precedence)")
      }
      sections <- c(sections, list(token_section(label, hits)))
    }

    ## Per-token race tables (only for tokens that matched).
    used_surname <- pred$surname_used   # "maiden", "last", or NULL
    field_render_label <- list(first = "given name", middle = "middle name",
                               last = "last name",  maiden = "maiden name")
    for (field in c("first", "middle", "last", "maiden")) {
      ## Skip the "ignored" surname source so we don't render its rows in
      ## the per-token race tables (token-summary card above already
      ## flagged it).
      if (field == "last"   && identical(used_surname, "maiden")) next
      hits <- pred$tokens[[field]]
      side_label <- field_render_label[[field]]
      for (tok_name in names(hits)) {
        hit <- hits[[tok_name]]
        if (is.null(hit)) next
        title <- sprintf("P(group | %s = %s)", side_label, tok_name)
        ## Note source-mismatch / non-exact rule, plus compound notice.
        note_parts <- character(0)
        expected_source <- if (field %in% c("first", "middle")) "first" else "last"
        if (hit$source != expected_source) {
          note_parts <- c(note_parts,
            sprintf("Looked up in <strong>%s-names</strong> after %s table missed.",
                    hit$source, expected_source))
        } else if (!identical(hit$rule, "exact")) {
          note_parts <- c(note_parts,
            sprintf("Matched as <strong>%s</strong> (%s).", hit$matched_as, hit$rule))
        }
        if (identical(hit$dataset, "rosenman")) {
          note_parts <- c(note_parts,
            paste("Source: <strong>Rosenman, Olivella, and Imai (2023)</strong>",
                  "voter-file additions (NotInCensus2020). The OTHER bucket",
                  "from Rosenman is split between AIAN and",
                  "non-Hispanic two-or-more proportionally to the Census prior."))
        }
        if (grepl(" ", tok_name, fixed = TRUE)) {
          note_parts <- c(note_parts,
            "Compound row matched directly — no per-token combination.")
        }
        note <- if (length(note_parts)) paste(note_parts, collapse = " ") else NULL
        sections <- c(sections, list(prob_table(
          title, hit$probs, race_labels, "Origin group", note
        )))
      }
    }

    ## Combined race tables when at least one token matched.
    if (!is.null(pred$combined)) {
      k <- pred$combined$n
      sections <- c(sections, list(prob_table(
        sprintf("P(group | %d matched name token%s)",
                k, if (k == 1L) "" else "s"),
        pred$combined$probs, race_labels, "Origin group",
        sprintf(paste(
          "Combined under conditional independence given the group:",
          "<em>P(g | n<sub>1</sub>, …, n<sub>%d</sub>) ∝ Π P(g | n<sub>i</sub>) /",
          "P(g)<sup>%d</sup></em>."
        ), k, max(0L, k - 1L))
      )))
    }

    ## Geography prior + name+geography combination.
    if (!is.null(pred$geography)) {
      geo <- pred$geography
      type_label <- if (identical(geo$type, "vap")) "VAP (everyone 18+)"
                    else "CVAP (citizens 18+)"
      level_label <- switch(geo$level %||% "national",
                            zcta        = "ZIP / ZCTA",
                            tract       = "Census Tract",
                            block_group = "Census Block Group",
                            user        = "user-supplied",
                            national    = "national prior")
      if (isTRUE(geo$found) && !is.null(geo$probs)) {
        tot <- if (!is.na(geo$total)) sprintf(" (n = %s)",
                                              format(geo$total, big.mark = ","))
               else ""
        note <- sprintf(
          "%s prior at %s for <strong>%s</strong>%s.",
          type_label, level_label,
          if (is.null(geo$key)) "supplied geography" else geo$key,
          tot
        )
        sections <- c(sections, list(prob_table(
          sprintf("P(group | %s)", level_label),
          geo$probs, race_labels, "Origin group", note
        )))
        if (!is.null(geo$combined) && !is.null(pred$combined)) {
          sections <- c(sections, list(prob_table(
            "P(group | name, geography) — BISG / BIFSG combination",
            geo$combined, race_labels, "Origin group",
            paste(
              "Combined under conditional independence given the group:",
              "<em>P(g | name, G) ∝ P(g | name) × P(g | G) / P(g)</em>.",
              "Tighten or loosen the name-only estimate using the local",
              "demographic mix."
            )
          )))
        }
      } else if (!isTRUE(geo$found)) {
        sections <- c(sections, list(div(
          class = "card error",
          HTML(sprintf(
            "Geography <strong>%s</strong> not found in bundled %s table at %s level.",
            geo$key %||% "(empty)", type_label, level_label
          ))
        )))
      }
    }

    ## Sex prediction — from the FIRST name field only. Middle names
    ## are intentionally excluded (cross-sex middle names are common).
    if (!is.null(pred$sex) && pred$sex$n >= 1L) {
      ks <- pred$sex$n
      sections <- c(sections, list(prob_table(
        sprintf("P(sex | %d matched first-name token%s)",
                ks, if (ks == 1L) "" else "s"),
        pred$sex$probs, sex_labels, "Sex",
        sprintf(paste(
          "Computed from the first-name field only (middle names are",
          "excluded). Across <em>%d</em> matched token%s the formula is",
          "<em>Π P(sex | n<sub>i</sub>) / P(sex)<sup>%d</sup></em>."
        ), ks, if (ks == 1L) "" else "s", max(0L, ks - 1L))
      )))
    }

    do.call(tagList, sections)
  })
}

shinyApp(ui, server)
