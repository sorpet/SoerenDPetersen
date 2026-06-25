library(yaml)
library(purrr)
library(readr)
library(dplyr)
library(glue)
library(here)
library(stringr)
library(lubridate)
library(htmltools)

source(here("R", "utils_cv.R"))

# === Text cleaning and formatting ===
clean_text <- function(x) {
  x %>%
    str_replace_all("[\r\n]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("\\s+([,\\.])", "\\1") %>%
    str_remove("([,\\.])$") %>%
    str_trim()
}

highlight_presenters <- function(authors, presenters) {
  if (is.na(presenters) || presenters == "") return(authors)
  str_split(presenters, ";")[[1]] %>%
    str_trim() %>%
    reduce(~ str_replace_all(.x, fixed(.y), glue("<u>{.y}</u>")), .init = authors)
}

highlight_own_name <- function(authors) {
  str_replace_all(
    authors,
    regex("Petersen,\\s*S\\.?\\s*D\\.?", ignore_case = FALSE),
    function(match) glue("<strong>{match}</strong>")
  )
}

format_date_range <- function(start_date, end_date) {
  if (!is.na(end_date)) {
    same_month <- format(start_date, "%Y %b") == format(end_date, "%Y %b")
    if (same_month) {
      glue("{format(start_date, '%Y %b')} {format(start_date, '%d')} – {format(end_date, '%d')}")
    } else {
      glue("{format(start_date, '%Y %b %d')} – {format(end_date, '%Y %b %d')}")
    }
  } else {
    format(start_date, "%Y %b %d")
  }
}

maybe_prefix_comma <- function(value) {
  if (!is.na(value) && value != "") paste0(", ", value) else ""
}

compact_html_entry <- function(type, authors_formatted, title, conference, location) {
  HTML(
    paste0(
      type, ": ",
      authors_formatted,
      maybe_prefix_comma(title),
      ". <em>", conference, "</em>",
      maybe_prefix_comma(location),
      "."
    )
  )
}

generate_entry_html <- function(display_date, type, authors_formatted, title, conference, location) {
  as.character(
    tags$div(class = "education-entry",
             tags$div(class = "education-date", display_date),
             tags$div(class = "education-role",
                      tags$p(compact_html_entry(type, authors_formatted, title, conference, location))
             )
    )
  )
}

standardize_communication_columns <- function(df) {
  df <- normalize_column_names(df)
  if ("date_start" %in% names(df) && !"start_date" %in% names(df)) {
    df <- rename(df, start_date = date_start)
  }
  if ("date_end" %in% names(df) && !"end_date" %in% names(df)) {
    df <- rename(df, end_date = date_end)
  }
  df
}

process_communication_entry <- function(entry) {
  html_path <- here::here(entry$html)

  df <- read_configured_table(entry, local_keys = c("csv", "xlsx")) %>%
    standardize_communication_columns() %>%
    mutate(
      across(c(title, authors, type, conference, location, presenters), clean_text),
      start_date = ymd(as.character(start_date), quiet = TRUE),
      end_date = ymd(as.character(end_date), quiet = TRUE)
    ) %>%
    arrange(desc(start_date)) %>%
    rowwise() %>%
    mutate(
      display_date = format_date_range(start_date, end_date),
      authors_formatted = highlight_own_name(highlight_presenters(authors, presenters)),
      html = generate_entry_html(display_date, type, authors_formatted, title, conference, location)
    )

  writeLines(paste(df$html, collapse = "\n\n"), html_path)
  cat("✅ Written:", html_path, "\n")
}

# === Run pipeline ===
config <- read_yaml(here("data", "paths.yml"))
walk(config$communications, process_communication_entry)
