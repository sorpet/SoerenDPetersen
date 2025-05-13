# === Load packages ===
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(here)
library(htmltools)
library(readxl)
library(yaml)
library(httr)
library(glue)

# === Constants ===
GROUP_REGEX <- "\\(.*\\)"
DOI_REGEX <- "\\[doi:([^\\]]+)\\]"

# === Load config and input ===
load_config <- function() {
  read_yaml(here("data", "paths.yml"))
}

config <- load_config()
gdrive_url <- config$work_experience$xlsx_gdrive
xlsx_path <- download_gdrive_xlsx(gdrive_url)

# === Load and prepare publication references ===
source(here("scripts/generate_publications.R"))  # Assumes bib_df is available

bib_lookup <- bib_df %>%
  filter(!is.na(doi)) %>%
  select(doi, index)

linkify_dois_to_publications <- function(text) {
  str_replace_all(text, DOI_REGEX, function(m) {
    doi <- str_match(m, DOI_REGEX)[, 2]
    pub_index <- bib_lookup %>% filter(doi == .env$doi) %>% pull(index)
    if (length(pub_index) > 0) {
      as.character(a(href = glue("#pub-{pub_index}"), paste0("publication no. ", pub_index)))
    } else {
      m
    }
  })
}

make_link <- function(text, link) {
  if (!is.na(link) && nzchar(link)) {
    as.character(a(href = link, target = "_blank", text))
  } else {
    text
  }
}

# === Prepare work experience data ===
load_work_experience <- function(path) {
  read_xlsx(path) %>%
    mutate(
      group_raw = str_extract(institution, GROUP_REGEX) %>% str_remove_all("^\\(|\\)$"),
      institution_base = str_trim(str_remove(institution, GROUP_REGEX))
    )
}

set_group_links <- function(df) {
  df$group_link_html <- NA_character_

  # Handle TeselaGen case
  teselagen_idx <- which(str_detect(df$institution, "Lawrence Berkeley National Lab"))
  df$group_link_html[teselagen_idx] <- paste0(
    make_link("Group – Quantitative metabolic modeling", "https://qmm.lbl.gov/"),
    " in collaboration with ",
    make_link("TeselaGen Biotechnology San Francisco Data science group", "http://teselagen.com/")
  )

  # Handle others
  other_rows <- is.na(df$group_link_html)
  df$group_link_html[other_rows] <- map2_chr(
    df$group_raw[other_rows],
    df$institution_link[other_rows],
    make_link
  )

  df
}

finalize_work_df <- function(df) {
  df %>%
    mutate(
      institution_full = if_else(
        is.na(group_raw) | group_raw == "",
        institution_base,
        paste0(institution_base, " (", group_link_html, ")")
      ),
      work = linkify_dois_to_publications(work),
      result = linkify_dois_to_publications(result),
      project_html = map2_chr(project, project_link, make_link)
    )
}

# === Generate HTML ===
generate_work_experience_html <- function(df) {
  pmap(df, function(date, role, institution_full, project_html, work, result, ...) {
    tags$div(class = "education-entry", list(
      tags$div(class = "education-date", date),
      tags$div(class = "education-role", list(
        tags$p(tags$strong(HTML(paste0(role, " at ", institution_full)))),
        if (!is.na(project_html) && nzchar(project_html)) tags$p(HTML(paste0("Project: ", project_html))),
        tags$p(HTML(paste0("Work: ", work))),
        if (!is.na(result) && nzchar(result)) tags$p(HTML(paste0("Main result: ", result)))
      ))
    ))
  })
}

# === Run pipeline ===
work_experience_df <- load_work_experience(xlsx_path) %>%
  set_group_links() %>%
  finalize_work_df()

html_entries <- generate_work_experience_html(work_experience_df)

# === Save output ===
save_html(tagList(html_entries), here("content_html/work_experience.html"))
message("✅ work_experience.html generated using htmltools.")
