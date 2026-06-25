#!/usr/bin/env Rscript
library(dplyr)
library(glue)
library(htmltools)
library(purrr)
library(stringr)
library(yaml)

here <- function(...) file.path(getwd(), ...)

source(here("R", "utils_cv.R"))
source(here("R", "publications_cv.R"))

GROUP_REGEX <- "\\(.*\\)"

config <- yaml::read_yaml(here("data", "paths.yml"))
work_source_df <- read_configured_table(config$work_experience, local_keys = c("csv", "xlsx"))

make_link <- function(text, link) {
  if (!is.na(link) && nzchar(link)) {
    as.character(a(href = link, target = "_blank", text))
  } else {
    text
  }
}

load_work_experience <- function(df) {
  df %>%
    normalize_column_names() %>%
    mutate(
      group_raw = str_extract(institution, GROUP_REGEX) %>% str_remove_all("^\\(|\\)$"),
      institution_base = str_trim(str_remove(institution, GROUP_REGEX))
    )
}

set_group_links <- function(df) {
  df$group_link_html <- NA_character_

  teselagen_idx <- which(str_detect(df$institution, "Lawrence Berkeley National Lab"))
  df$group_link_html[teselagen_idx] <- paste0(
    make_link("Group – Quantitative metabolic modeling", "https://qmm.lbl.gov/"),
    " in collaboration with ",
    make_link("TeselaGen Biotechnology San Francisco Data science group", "http://teselagen.com/")
  )

  other_rows <- is.na(df$group_link_html)
  df$group_link_html[other_rows] <- map2_chr(
    df$group_raw[other_rows],
    df$institution_link[other_rows],
    make_link
  )

  df
}

finalize_work_df <- function(df) {
  paths <- load_publication_paths()
  bib_df <- load_publications_df(paths$bib)
  bib_lookup <- make_publication_lookup(bib_df)

  df %>%
    mutate(
      institution_full = if_else(
        is.na(group_raw) | group_raw == "",
        institution_base,
        paste0(institution_base, " (", group_link_html, ")")
      ),
      work = linkify_dois_to_publications(work, bib_lookup),
      result = linkify_dois_to_publications(result, bib_lookup),
      project_html = map2_chr(project, project_link, make_link)
    )
}

format_role_anchor <- function(role_html) {
  if (is.na(role_html) || role_html == "") return(HTML(""))

  role_parts <- str_match(role_html, "^(.*?)\\s+at\\s+(.*)$")

  if (is.na(role_parts[, 2])) {
    return(HTML(paste0("<strong>", htmlEscape(role_html), "</strong>")))
  }

  HTML(paste0(
    "<strong>", htmlEscape(str_trim(role_parts[, 2])), "</strong>",
    " at ",
    role_parts[, 3]
  ))
}

render_work_entry <- function(date, role_html, detail_html) {
  detail_tags <- detail_html[!is.na(detail_html) & detail_html != ""]

  tags$div(
    class = "education-entry",
    tags$div(class = "education-date", HTML(date)),
    tags$div(
      class = "education-role",
      tags$p(format_role_anchor(role_html)),
      lapply(detail_tags, function(detail) tags$p(HTML(detail)))
    )
  )
}

generate_generic_work_html <- function(df) {
  detail_cols <- names(df)[str_detect(names(df), "^detail_[0-9]+_html$")]

  map(seq_len(nrow(df)), function(row_index) {
    render_work_entry(
      df$date[[row_index]],
      df$role_html[[row_index]],
      unlist(df[row_index, detail_cols], use.names = FALSE)
    )
  })
}

generate_structured_work_html <- function(df) {
  pmap(df, function(date, role, institution_full, project_html, work, result, ...) {
    tags$div(class = "education-entry", list(
      tags$div(class = "education-date", date),
      tags$div(class = "education-role", list(
        tags$p(format_role_anchor(paste0(role, " at ", institution_full))),
        if (!is.na(project_html) && nzchar(project_html)) tags$p(HTML(paste0("Project: ", project_html))),
        tags$p(HTML(paste0("Work: ", work))),
        if (!is.na(result) && nzchar(result)) tags$p(HTML(paste0("Main result: ", result)))
      ))
    ))
  })
}

work_experience_df <- normalize_column_names(work_source_df)

html_entries <- if (all(c("date", "role_html") %in% names(work_experience_df))) {
  generate_generic_work_html(work_experience_df)
} else {
  work_experience_df %>%
    load_work_experience() %>%
    set_group_links() %>%
    finalize_work_df() %>%
    generate_structured_work_html()
}

output_path <- here(config$work_experience$html)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
writeLines(paste0(vapply(html_entries, as.character, character(1)), collapse = "\n"), output_path)

message("✅ work_experience.html generated as a fragment.")
