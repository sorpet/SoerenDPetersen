library(yaml)
library(readr)
library(stringr)
library(glue)
library(here)
library(purrr)
library(readxl)
library(htmltools)

source(here("R", "utils_cv.R"))

# === Utility functions ===

create_bullet_list <- function(bullets_string) {
  if (is.na(bullets_string) || bullets_string == "") return(NULL)
  items <- str_split(bullets_string, ";\\s*")[[1]]
  tags$ul(lapply(items, function(item) tags$li(HTML(item))))
}

format_title_anchor <- function(title) {
  if (is.na(title) || title == "") return(HTML(""))

  title_parts <- str_match(title, "^([^,]+)(.*)$")
  title_anchor <- str_trim(title_parts[, 2])
  title_tail <- title_parts[, 3]

  HTML(paste0(
    "<strong>", htmlEscape(title_anchor), "</strong>",
    htmlEscape(title_tail)
  ))
}

format_education_entry <- function(row) {
  date_text <- row$date %>%
    str_replace_all(" ", "&nbsp;") %>%
    str_replace("–", "&ndash;")

  as.character(
    tags$div(
      class = "education-entry",
      tags$div(class = "education-date", HTML(date_text)),
      tags$div(
        class = "education-role",
        tags$p(format_title_anchor(row$title)),
        create_bullet_list(row$bullets)
      )
    )
  )
}

process_education <- function(df, output_path) {
  df <- normalize_column_names(df)
  html_entries <- map_chr(seq_len(nrow(df)), ~format_education_entry(df[.x, ]))
  writeLines(html_entries, output_path)
  cat("✅", nrow(df), "entries written to:", output_path, "\n")
}

config <- yaml::read_yaml(here("data", "paths.yml"))

# Access education paths
education <- config$education

process_education(
  read_configured_table(education, local_keys = c("csv", "xlsx")),
  here(education$html)
)
