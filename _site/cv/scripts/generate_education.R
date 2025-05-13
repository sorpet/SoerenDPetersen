library(yaml)
library(readr)
library(stringr)
library(glue)
library(here)
library(purrr)
library(readxl)
library(htmltools)

#source(here("R/utils_cv.R"))

# === Utility functions ===

create_bullet_list <- function(bullets_string) {
  if (is.na(bullets_string) || bullets_string == "") return(NULL)
  items <- str_split(bullets_string, ";\\s*")[[1]]
  tags$ul(lapply(items, function(item) tags$li(HTML(item))))
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
        tags$p(row$title),
        create_bullet_list(row$bullets)
      )
    )
  )
}

process_education <- function(xlsx_path, output_path) {
  df <- read_xlsx(xlsx_path)
  html_entries <- map_chr(seq_len(nrow(df)), ~format_education_entry(df[.x, ]))
  writeLines(html_entries, output_path)
  cat("✅", nrow(df), "entries written to:", output_path, "\n")
}

config <- yaml::read_yaml(here("data", "paths.yml"))

# Access education paths
education <- config$education

# === Run ===
xlsx_path <- if (!is.null(education$xlsx_gdrive)) {
  download_gdrive_xlsx(education$xlsx_gdrive)
} else {
  here(education$xlsx)
}

process_education(
  xlsx_path,
  here(education$html)
)
