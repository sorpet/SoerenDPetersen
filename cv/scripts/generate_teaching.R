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

is_now_like <- function(x) {
  tolower(trimws(as.character(x))) %in% c("now", "present", "current")
}

as_month_date <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == "" | is_now_like(x)] <- NA_character_
  month_only <- str_detect(x, "^\\d{4}-\\d{2}$")
  month_only[is.na(month_only)] <- FALSE
  x[month_only] <- paste0(x[month_only], "-01")
  as.Date(x)
}

format_month_year <- function(x) {
  format(as_month_date(x), "%b %Y")
}

format_month_year_range <- function(start_date, end_date) {
  start_fmt <- format_month_year(start_date)

  if (is_now_like(end_date)) {
    return(glue("{start_fmt} – now"))
  }

  end_fmt <- format_month_year(end_date)

  glue("{start_fmt} – {end_fmt}")
}

source(here("R", "utils_cv.R"))
prepare_courses_df <- function(df) {
  df %>%
    normalize_column_names() %>%
    mutate(across(everything(), as.character)) %>%
    mutate(
      # keep as character so "now" can coexist with dates
      across(c(date_start, date_end), as.character),
      date = map2_chr(date_start, date_end, format_month_year_range)
    ) %>%
    select(-matches("_fmt$")) %>%
    mutate(across(everything(), ~tidyr::replace_na(.x, "")))
}

prepare_students_df <- function(df) {
  df %>%
    normalize_column_names() %>%
    mutate(across(everything(), as.character)) %>%
    mutate(
      across(c(date_start, date_end), as.character),
      date_start_full = if_else(str_detect(date_start, "^\\d{4}-\\d{2}$"), paste0(date_start, "-01"), date_start),
      date_end_full = if_else(is.na(date_end) | date_end == "" | is_now_like(date_end) | str_detect(date_end, "^\\d{4}-\\d{2}-\\d{2}$"), date_end, paste0(date_end, "-01")),
      date = map2_chr(date_start_full, date_end_full, format_month_year_range)
    ) %>%
    select(-date_start_full, -date_end_full)
}

process_teaching_entry <- function(entry) {
  source_df <- read_configured_table(entry, local_keys = c("csv", "xlsx"))
  html_path <- here::here(entry$html)

  if (entry$name == "students") {
    df <- prepare_students_df(source_df)
    temp_csv <- tempfile(fileext = ".csv")
    readr::write_delim(df, temp_csv, delim = ";")
    process_students_csv(temp_csv, html_path)
  } else if (entry$name == "courses") {
    df <- prepare_courses_df(source_df)
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

collapse_html_lines <- function(lines) {
  lines <- lines[!is.na(lines) & nzchar(lines)]
  if (!length(lines)) return("")
  paste(htmlEscape(unique(lines)), collapse = "<br>")
}

build_course_contribution_html <- function(offering_dates, contributions) {
  contribution_df <- tibble::tibble(
    offering_date = as.character(offering_dates),
    contribution = trimws(as.character(contributions))
  ) %>%
    filter(!is.na(contribution), contribution != "") %>%
    distinct()

  if (nrow(contribution_df) == 0) {
    return("")
  }

  if (n_distinct(contribution_df$contribution) == 1) {
    return(htmlEscape(contribution_df$contribution[[1]]))
  }

  lines <- glue("{contribution_df$offering_date}: {contribution_df$contribution}")
  paste(htmlEscape(lines), collapse = "<br>")
}

collapse_course_offerings <- function(df) {
  group_cols <- c(
    "role", "course_level", "course_name", "course_link",
    "institution", "institution_link"
  )

  df %>%
    mutate(
      date_start_sort = as_month_date(date_start),
      date_end_sort = case_when(
        is_now_like(date_end) ~ Sys.Date(),
        TRUE ~ as_month_date(date_end)
      )
    ) %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(~ {
      offerings <- .x %>%
        arrange(desc(date_end_sort), desc(date_start_sort))

      tibble::tibble(
        date_html = collapse_html_lines(offerings$date),
        contribution_html = build_course_contribution_html(
          offerings$date,
          offerings$contribution
        ),
        date_end_sort = max(offerings$date_end_sort),
        date_start_sort = min(offerings$date_start_sort)
      )
    }) %>%
    ungroup() %>%
    arrange(desc(date_end_sort), desc(date_start_sort))
}

format_course_role_label <- function(role, course_level) {
  glue("{role} in the {course_level} course:")
}

format_supervision_role_label <- function(role) {
  role %>%
    str_replace("^Co-supervisor for PhD project$", "Co-supervisor for PhD student") %>%
    str_replace("^Co-supervisor for special course$", "Co-supervisor of the special course") %>%
    str_replace("^Co-supervisor for BSc thesis$", "Co-supervisor of the BSc thesis") %>%
    str_replace("^Co-supervisor for MSc thesis$", "Co-supervisor of the MSc thesis")
}

student_role_connector <- function(role_label) {
  if (str_detect(role_label, "student$")) {
    " "
  } else if (str_detect(role_label, "^Co-supervisor of the ")) {
    " of "
  } else {
    " for "
  }
}

# Course entry formatter
row_to_course_html <- function(row) {
  role_label <- format_course_role_label(row$role, row$course_level)
  course_link <- as.character(format_linked(row$course_name, row$course_link))
  institution_link <- as.character(format_linked(row$institution, row$institution_link))
  title <- paste0(
    as.character(tags$strong(role_label)),
    "\n      ",
    course_link,
    " at ",
    institution_link
  )

  contribution_tag <- if (!is.na(row$contribution_html) && row$contribution_html != "") {
    tags$p(HTML(paste0("Contribution: ", row$contribution_html)))
  } else {
    NULL
  }

  tags$div(
    class = "education-entry",
    tags$div(class = "education-date", HTML(row$date_html)),
    tags$div(
      class = "education-role",
      tags$p(HTML(title)),
      contribution_tag
    )
  )
}

# Student entry formatter
row_to_student_html <- function(row) {
  role_label <- format_supervision_role_label(row$role)
  role_line <- paste0(
    as.character(tags$strong(role_label)),
    student_role_connector(role_label),
    htmlEscape(row$student_name),
    " at ",
    as.character(format_linked(row$institution, row$institution_link))
  )

  grade_ects_inline <- c(
    if (!is.na(row$grade) && row$grade != "") glue("Grade: {row$grade}"),
    if (!is.na(row$ects)  && row$ects  != "") glue("ECTS: {row$ects}")
  ) %>%
    paste(collapse = "; ") %>%
    trimws()

  project_with_grading <- if (grade_ects_inline != "") {
    glue("Project: {row$project} ({grade_ects_inline})")
  } else {
    glue("Project: {row$project}")
  }

  full_html <- paste(
    role_line,
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

  required_cols <- c("date", "date_start", "role", "course_level", "course_name", "course_link",
                     "institution", "institution_link", "contribution", "date_end")
  if (!all(required_cols %in% names(df))) stop("❌ Missing columns for teaching_courses.")

  html_tags <- df %>%
    collapse_course_offerings() %>%
    rowwise() %>%
    group_split() %>%
    map(row_to_course_html)

  writeLines(paste0(sapply(html_tags, as.character), collapse = "\n"), output_html)
  if (verbose) message("✅ teaching_courses.html written to: ", output_html)
}

process_students_csv <- function(input_csv, output_html, verbose = TRUE) {
  df <- read_delim(input_csv, delim = ";", show_col_types = FALSE, trim_ws = TRUE)

  required_cols <- c("date", "role", "student_name", "institution", "institution_link",
                     "project", "grade", "ects", "date_end")
  if (!all(required_cols %in% names(df))) stop("❌ Missing columns for teaching_students.")

  html_tags <- df %>%
    mutate(
      date_end_sort = case_when(
        is_now_like(date_end) ~ Sys.Date(),
        TRUE ~ as_month_date(date_end)
      )
    ) %>%
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
