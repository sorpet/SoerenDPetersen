#!/usr/bin/env Rscript
library(yaml)
library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(scales)

here <- function(...) file.path(getwd(), ...)

source(here("R", "utils_cv.R"))

first_present <- function(row, columns, default = "") {
  for (column in columns) {
    if (column %in% names(row) && !is.na(row[[column]]) && row[[column]] != "") {
      return(row[[column]])
    }
  }
  default
}

format_applied_text <- function(x) {
  if (is.null(x) || is.na(x)) return("")

  x_fmt <- ifelse(
    round(x, 1) == round(x, 0),
    format(round(x, 0), nsmall = 0, trim = TRUE),
    format(round(x, 1), nsmall = 1, trim = TRUE)
  )
  x_fmt <- gsub("\\.", ",", x_fmt)

  glue(" Applied for in total: <strong>DKK {x_fmt} mio</strong> (including OH).")
}

format_amount_text <- function(row) {
  total_received <- first_present(row, "total_received", default = NA)
  if (!is.na(total_received)) {
    return(glue(" Total received: <strong>{scales::comma(total_received, prefix = 'DKK ')}</strong>."))
  }

  investment <- first_present(row, c("innovation_fund_investment_dkk_mio", "investment_dkk_mio"), default = NA)
  total_budget <- first_present(row, c("total_project_budget_dkk_mio", "total_budget_dkk_mio"), default = NA)
  if (!is.na(investment) && !is.na(total_budget)) {
    return(glue(
      " Innovation Fund investment: <strong>DKK {investment} million</strong>; ",
      "total project budget: <strong>DKK {total_budget} million</strong>."
    ))
  }

  applied_raw <- first_present(
    row,
    c("applied_for_in_total_dkk_mio", "applied_total", "applied_for_total"),
    default = NA
  )
  applied_num <- suppressWarnings(
    as.numeric(stringr::str_replace(as.character(applied_raw), ",", "."))
  )

  if (!is.na(applied_num)) {
    return(format_applied_text(applied_num))
  }

  ""
}

render_funding_row <- function(row) {
  date_text <- if ("date" %in% names(row) && !is.na(row$date) && row$date != "") {
    row$date
  } else {
    format_date_range(row$start_date, row$end_date)
  }

  desc <- first_present(row, "description_html", default = NA)
  if (is.na(desc)) {
    type_label <- stringr::str_replace_all(row$type, "_", " ") %>%
      stringr::str_to_sentence()
    amount_txt <- format_amount_text(row)

    role_txt <- first_present(row, "role")
    role_prefix <- if (nzchar(role_txt)) paste0(role_txt, " in ") else ""
    foundation <- first_present(row, c("foundation_short_name", "foundation_full_name"))
    project_title <- first_present(row, c("project_title", "conference_full_name"))
    project_link <- first_present(row, "project_link", default = NA)

    linked_project <- if (!is.na(project_link) && nzchar(project_link)) {
      glue("<a href=\"{project_link}\" target=\"_blank\">{project_title}</a>")
    } else {
      project_title
    }

    desc <- if (row$type %in% c("funded_project", "research_proposal", "grant_application", "application")) {
      title_txt <- if (nzchar(project_title)) paste0(": ", linked_project) else ""
      glue("{role_prefix}{foundation} project application{title_txt}.{amount_txt}")
    } else if (row$type == "conference") {
      glue("{type_label} {row$conference_full_name} ({row$conference_short_name}) at {row$location}.{amount_txt}")
    } else {
      glue("{type_label} at {row$university_full_name} ({row$university_short_name}), {row$location}.{amount_txt}")
    }
  }

  glue(
    "<div class=\"education-entry\">",
    "  <div class=\"education-date\">{date_text}</div>",
    "  <div class=\"education-role\"><p>{desc}</p></div>",
    "</div>"
  )
}

parse_funding_date <- function(x) {
  if (inherits(x, "Date")) return(x)

  parsed <- suppressWarnings(as.Date(x, format = "%d-%m-%Y"))
  fallback <- suppressWarnings(as.Date(x))
  dplyr::if_else(is.na(parsed), fallback, parsed)
}

process_funding <- function(source_df, output_paths) {
  df <- source_df %>%
    normalize_column_names()

  if ("Applied for in total" %in% names(df) && !"applied_total" %in% names(df)) {
    df <- rename(df, applied_total = `Applied for in total`)
  }

  if ("date_start" %in% names(df)) {
    df <- df %>%
      mutate(
        start_date = parse_funding_date(date_start),
        end_date = parse_funding_date(date_end)
      ) %>%
      arrange(desc(start_date))
  }

  html_entries <- purrr::map_chr(seq_len(nrow(df)), function(i) {
    render_funding_row(df[i, ])
  })

  for (output_path in output_paths) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(html_entries, output_path)
  }

  cat("✅", nrow(df), "funding entries written to:", paste(output_paths, collapse = ", "), "\n")
}

config <- yaml::read_yaml(here("data", "paths.yml"))
funding <- config$funding
output_paths <- unique(unlist(c(funding$html, funding$md)))

if (!length(output_paths)) {
  stop("No funding HTML output path provided in config")
}

process_funding(
  read_configured_table(funding, local_keys = c("csv", "xlsx")),
  here(output_paths)
)
