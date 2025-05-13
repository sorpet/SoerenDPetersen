#!/usr/bin/env Rscript
library(yaml)
library(readxl)
library(dplyr)
library(glue)
library(purrr)
library(htmltools)
library(stringr)
library(scales)
library(here)

source(here("R/utils_cv.R"))

# Load configuration
config <- yaml::read_yaml(here("data", "paths.yml"))
funding <- config$funding

# Download Excel file
xlsx_path <- if (!is.null(funding$xlsx_gdrive)) {
  download_gdrive_xlsx(funding$xlsx_gdrive)
} else if (!is.null(funding$xlsx)) {
  here(funding$xlsx)
} else {
  stop("⚠️ No funding xlsx path provided in config")
}

# Process funding data and write HTML
process_funding <- function(xlsx_path, output_path) {
  df <- readxl::read_excel(xlsx_path) %>%
    mutate(
      start_date = as.Date(date_start, format = "%d-%m-%Y"),
      end_date   = as.Date(date_end,   format = "%d-%m-%Y")
    ) %>%
    arrange(desc(start_date))

  html_entries <- purrr::map_chr(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    date_text <- format_date_range(row$start_date, row$end_date)
    type_label <- stringr::str_replace_all(row$type, "_", " ") %>% stringr::str_to_sentence()
    # Format description: conference entries read differently
    if (row$type == "conference") {
      desc <- glue::glue(
        "{type_label} {row$conference_full_name} ({row$conference_short_name}) at {row$location}. ",
        "Total received: <strong>{scales::comma(row$total_received, prefix = 'DKK ')}</strong>."
      )
    } else {
      desc <- glue::glue(
        "{type_label} at {row$university_full_name} ({row$university_short_name}), {row$location}. ",
        "Total received: <strong>{scales::comma(row$total_received, prefix = 'DKK ')}</strong>."
      )
    }
    as.character(
      tags$div(
        class = "education-entry",
        tags$div(class = "education-date", date_text),
        tags$div(class = "education-role", tags$p(HTML(desc)))
      )
    )
  })

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(html_entries, output_path)
  cat("✅", nrow(df), "funding entries written to:", output_path, "\n")
}

# Run processing
if (is.null(funding$md)) {
  stop("⚠️ No funding markdown output path provided in config")
}
process_funding(xlsx_path, here(funding$md))


# update_total_received in funding_occasions.xlsx ---------------------------------------------------
# library(tidyverse)
# library(readxl)
# path_funding <- "/Users/sorpet/Library/CloudStorage/CloudMounter-SDP_gdrive/0105_funding_occassation"
# path_events <- path_funding |> file.path("funding_occassions.xlsx")
# path_occassions <- path_funding |> file.path("funding_occassions")
#
# # Step 1: Read and combine all Excel files
# df_all_occassions <-
#   tibble(file = list.files(path_occassions, pattern = "^[0-9]+\\.xlsx$", full.names = TRUE)) |>
#   mutate(id_funding_occassion = tools::file_path_sans_ext(basename(file)),
#          data = map(file, read_xlsx)) |>
#   unnest(data)
#
# # Step 2: Summarise beloeb_modtaget per occasion
# received_summary <- df_all_occassions |>
#   group_by(id_funding_occassion) |>
#   summarise(total_received = sum(beloeb_modtaget, na.rm = TRUE), .groups = "drop")
#
#
# # Step 3: Join with the main metadata table
# funding_occassions <- readxl::read_xlsx(path_events)
# funding_occassions <- funding_occassions |>
#   left_join(received_summary, by = "id_funding_occassion")
#
# funding_occassions |>
#   writexl::write_xlsx(path_events)
