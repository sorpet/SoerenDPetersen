library(stringr)
library(glue)
library(lubridate)

if (!exists("here", mode = "function")) {
  here <- function(...) file.path(getwd(), ...)
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

resolve_local_path <- function(path) {
  if (is.null(path) || is.na(path) || path == "") return(NULL)
  if (grepl("^/", path)) return(path)
  here(path)
}

local_path_exists <- function(path) {
  resolved <- resolve_local_path(path)
  !is.null(resolved) && file.exists(resolved)
}

read_table_file <- function(path, ...) {
  resolved <- resolve_local_path(path)
  ext <- tolower(tools::file_ext(resolved))
  dots <- list(...)

  if (ext %in% c("xlsx", "xls")) {
    return(do.call(readxl::read_excel, c(list(path = resolved), dots)))
  }
  if (ext == "csv") {
    args <- c(
      list(
        file = resolved,
        colClasses = "character",
        strip.white = TRUE,
        check.names = FALSE,
        fileEncoding = "UTF-8-BOM",
        na.strings = c("", "NA"),
        stringsAsFactors = FALSE
      ),
      dots
    )
    args$col_types <- NULL
    args$show_col_types <- NULL
    args$trim_ws <- NULL
    return(tibble::as_tibble(do.call(utils::read.csv, args)))
  }

  stop("Unsupported table format: ", resolved)
}

read_configured_table <- function(config_entry, local_keys = c("xlsx", "csv"), gdrive_key = "xlsx_gdrive", ...) {
  for (key in local_keys) {
    path <- config_entry[[key]]
    if (!is.null(path) && local_path_exists(path)) {
      return(read_table_file(path, ...))
    }
  }

  gdrive_url <- config_entry[[gdrive_key]]
  if (!is.null(gdrive_url)) {
    temp_path <- if (identical(gdrive_key, "csv_gdrive")) {
      download_gdrive_csv(gdrive_url)
    } else {
      download_gdrive_xlsx(gdrive_url)
    }
    return(read_table_file(temp_path, ...))
  }

  stop("No readable local path or Google Drive fallback configured.")
}

resolve_configured_file <- function(config_entry, local_keys = c("bib"), gdrive_key = "bib_gdrive") {
  for (key in local_keys) {
    path <- config_entry[[key]]
    if (!is.null(path) && local_path_exists(path)) {
      return(resolve_local_path(path))
    }
  }

  gdrive_url <- config_entry[[gdrive_key]]
  if (!is.null(gdrive_url)) {
    if (identical(gdrive_key, "csv_gdrive")) {
      return(download_gdrive_csv(gdrive_url))
    }
    return(download_gdrive_file(gdrive_url, paste0(".", local_keys[[1]])))
  }

  stop("No readable local path or Google Drive fallback configured.")
}

normalize_column_names <- function(df) {
  names(df) <- names(df) %>%
    stringr::str_replace("^\ufeff", "") %>%
    stringr::str_trim()
  df
}

clean_text <- function(x) {
  x %>%
    str_replace_all("[\r\n]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

extract_gdrive_id <- function(url) {
  file_id <- str_extract(url, "(?<=/d/)[^/]+")
  if (is.na(file_id) || file_id == "") {
    stop("Could not extract Google Drive file ID from URL: ", url)
  }
  file_id
}

get_gdrive_download_links <- function(url, format = NULL) {
  file_id <- extract_gdrive_id(url)
  links <- character()

  if (str_detect(url, "docs.google.com/spreadsheets")) {
    export_format <- format %||% "xlsx"
    links <- c(links, glue("https://docs.google.com/spreadsheets/d/{file_id}/export?format={export_format}"))
  }

  c(links, glue("https://drive.google.com/uc?export=download&id={file_id}"))
}

is_valid_download <- function(path, fileext) {
  if (fileext == ".xlsx") {
    signature <- readBin(path, what = "raw", n = 2)
    return(identical(signature, charToRaw("PK")))
  }
  TRUE
}

download_gdrive_file <- function(gdrive_url, fileext, format = NULL) {
  errors <- character()

  for (download_url in get_gdrive_download_links(gdrive_url, format = format)) {
    temp_path <- tempfile(fileext = fileext)
    response <- httr::GET(download_url, httr::write_disk(temp_path, overwrite = TRUE))

    if (httr::http_error(response)) {
      errors <- c(errors, glue("HTTP {httr::status_code(response)} from {download_url}"))
      next
    }
    if (!file.exists(temp_path) || file.size(temp_path) == 0) {
      errors <- c(errors, glue("empty file from {download_url}"))
      next
    }
    if (!is_valid_download(temp_path, fileext)) {
      errors <- c(errors, glue("unexpected file content from {download_url}"))
      next
    }

    return(temp_path)
  }

  stop(
    "Google Drive download failed for: ", gdrive_url,
    "\nTried:\n- ", paste(errors, collapse = "\n- ")
  )
}

download_gdrive_xlsx <- function(gdrive_url) {
  download_gdrive_file(gdrive_url, ".xlsx", format = "xlsx")
}

download_gdrive_csv <- function(gdrive_url) {
  download_gdrive_file(gdrive_url, ".csv", format = "csv")
}

highlight_presenters <- function(authors, presenters) {
  if (is.na(presenters) || presenters == "") return(authors)
  str_split(presenters, ";")[[1]] %>%
    str_trim() %>%
    purrr::reduce(~ str_replace_all(.x, fixed(.y), glue("<u>{.y}</u>")), .init = authors)
}

format_date_range <- function(start_date, end_date) {
  start_fmt <- format(start_date, "%Y %b")
  if (!is.na(end_date)) {
    end_fmt <- format(end_date, "%Y %b")
    if (start_fmt == end_fmt) {
      return(start_fmt)
    } else {
      return(glue::glue("{start_fmt} – {end_fmt}"))
    }
  } else {
    return(start_fmt)
  }
}
