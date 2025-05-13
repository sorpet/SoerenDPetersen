library(readr)
library(dplyr)
library(glue)
library(here)
library(yaml)
library(htmltools)
library(purrr)
library(stringr)
library(readxl)
library(httr)

#source(here("R/utils_cv.R"))
prepare_courses_df <- function(xlsx_path) {
  df <- readxl::read_excel(xlsx_path)

  if (all(c("date_start", "date_end") %in% names(df))) {
    df <- df %>%
      mutate(
        date = glue::glue("{format(as.Date(date_start), '%b %Y')} – {format(as.Date(date_end), '%b %Y')}")
      )
  }

  df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, "")))
}

prepare_students_df <- function(xlsx_path) {
  df <- readxl::read_excel(xlsx_path)

  df <- df %>%
    mutate(
      date = glue::glue(
        "{format(as.Date(paste0(date_start, '-01')), '%b %Y')} – {format(as.Date(paste0(date_end, '-01')), '%b %Y')}"
      )
    )

}

process_teaching_entry <- function(entry) {
  xlsx_path <- download_gdrive_xlsx(entry$xlsx_gdrive)
  html_path <- here::here(entry$html)

  if (entry$name == "students") {
    df <- prepare_students_df(xlsx_path)
    temp_csv <- tempfile(fileext = ".csv")
    readr::write_delim(df, temp_csv, delim = ";")
    process_students_csv(temp_csv, html_path)
  } else if (entry$name == "courses") {
    df <- prepare_courses_df(xlsx_path)
    temp_csv <- tempfile(fileext = ".csv")
    readr::write_delim(df, temp_csv, delim = ";")
    process_courses_csv(temp_csv, html_path)
  } else {
    warning(glue::glue("⚠️ Unknown entry name: {entry$name}"))
  }
}

# Utility
format_linked <- function(label, url) {
  if (is.na(url) || url == "") return(label)
  tags$a(href = url, target = "_blank", label)
}

# Course entry formatter
row_to_course_html <- function(row) {
  course_full <- glue("{row$course_level} course: ") %>%
    paste0(as.character(format_linked(row$course_name, row$course_link)))

  title <- glue("{row$role} in {course_full} at ") %>%
    paste0(as.character(format_linked(row$institution, row$institution_link)))

  contribution_tag <- if (!is.na(row$contribution) && row$contribution != "") {
    tags$p(glue("Contribution: {row$contribution}"))
  } else {
    NULL
  }

  tags$div(
    class = "education-entry",
    tags$div(class = "education-date", row$date),
    tags$div(
      class = "education-role",
      tags$p(tags$strong(HTML(title))),
      contribution_tag
    )
  )
}

# Student entry formatter
row_to_student_html <- function(row) {
  # Main supervision line
  role_line <- glue("{row$role} for {row$student_name} at {format_linked(row$institution, row$institution_link)}")

  # Inline grade and ECTS formatting
  grade_ects_inline <- c(
    if (!is.na(row$grade)) glue("Grade: {row$grade}"),
    if (!is.na(row$ects))  glue("ECTS: {row$ects}")
  ) %>%
    paste(collapse = "; ")

  # Combine with project
  project_with_grading <- if (grade_ects_inline != "") {
    glue("Project: {row$project} ({grade_ects_inline})")
  } else {
    glue("Project: {row$project}")
  }

  # Final HTML block
  full_html <- paste(
    as.character(tags$strong(HTML(role_line))),
    project_with_grading,
    sep = "<br>\n"
  )

  tags$div(
    class = "education-entry",
    tags$div(class = "education-date", row$date),
    tags$div(
      class = "education-role",
      tags$p(HTML(full_html))
    )
  )
}



# Processors
process_courses_csv <- function(input_csv, output_html, verbose = TRUE) {
  df <- read_delim(input_csv, delim = ";", show_col_types = FALSE, trim_ws = TRUE)
  required_cols <- c("date", "role", "course_level", "course_name", "course_link",
                     "institution", "institution_link", "contribution")
  if (!all(required_cols %in% names(df))) stop("❌ Missing columns for teaching_courses.")
  html_tags <- df %>%
    mutate(date_end = as.Date(date_end)) %>%
    arrange(desc(date_end)) %>%
    rowwise() %>%
    group_split() %>%
    map(row_to_course_html)
  writeLines(paste0(sapply(html_tags, as.character), collapse = "\n"), output_html)
  if (verbose) message("✅ teaching_courses.html written to: ", output_html)
}

process_students_csv <- function(input_csv, output_html, verbose = TRUE) {
  df <- read_delim(input_csv, delim = ";", show_col_types = FALSE, trim_ws = TRUE)
  required_cols <- c("date", "role", "student_name", "institution", "institution_link", "project", "grade", "ects")
  if (!all(required_cols %in% names(df))) stop("❌ Missing columns for teaching_students.")
  html_tags <- df %>%
    mutate(date_end_sort = as.Date(paste0(date_end, "-01"))) %>%
    arrange(desc(date_end_sort)) %>%
    rowwise() %>%
    group_split() %>%
    map(row_to_student_html)

  writeLines(paste0(sapply(html_tags, as.character), collapse = "\n"), output_html)
  if (verbose) message("✅ teaching_students.html written to: ", output_html)
}

# Load config and dispatch correct handler
config <- read_yaml(here("data", "paths.yml"))

walk(config$teaching, process_teaching_entry)
