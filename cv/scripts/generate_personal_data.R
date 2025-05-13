library(readr)
library(dplyr)
library(glue)
library(stringr)
library(lubridate)
library(here)
library(yaml)

#source(here("R/utils_cv.R"))

# === Load paths from config ===
config <- read_yaml(here("data", "paths.yml"))
personal_paths <- config$personal_data

# === Core functions ===
transform_value <- function(key, value) {
  if (key == "Birthday") {
    birthdate <- suppressWarnings(ymd(value))
    age <- floor(interval(birthdate, Sys.Date()) / years(1))
    formatted <- format(birthdate, "%B %d, %Y")
    return(glue("{formatted} ({age} years old)"))
  }
  if (key == "Email") {
    return(glue('<a href="mailto:{value}">{value}</a>'))
  }
  if (key == "Address") {
    return(str_replace(value, "Building 223,", "Building 223,<br>"))
  }
  return(value)
}

read_personal_data <- function(path) {
  if (grepl("^https?://", path)) {
    path <- download_gdrive_csv(path)
  }
  readr::read_delim(path, delim = ";", show_col_types = FALSE)
}

render_to_html <- function(df) {
  df %>%
    rowwise() %>%
    mutate(value = transform_value(key, value)) %>%
    mutate(html = glue("<dt>{key}</dt><dd>{value}</dd>")) %>%
    pull(html) %>%
    paste(collapse = "\n")
}

generate_personal_data_html <- function(input_path, output_path) {
  df <- read_personal_data(input_path)
  html <- render_to_html(df)
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, output_path)
  cat("✅ Wrote static personal data HTML to:", output_path, "\n")
}

# === Run using paths from config ===
generate_personal_data_html(
  personal_paths$csv_gdrive,
  here(personal_paths$html)
)
