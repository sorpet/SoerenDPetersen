library(stringr)
library(glue)
library(lubridate)

clean_text <- function(x) {
  x %>%
    str_replace_all("[\r\n]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

get_gdrive_download_link <- function(url) {
  file_id <- str_extract(url, "(?<=/d/)[^/]+")
  glue("https://drive.google.com/uc?export=download&id={file_id}")
}

download_gdrive_xlsx <- function(gdrive_url) {
  temp_xlsx <- tempfile(fileext = ".xlsx")
  download_url <- get_gdrive_download_link(gdrive_url)
  httr::GET(download_url, httr::write_disk(temp_xlsx, overwrite = TRUE))
  temp_xlsx
}

download_gdrive_csv <- function(gdrive_url) {
  temp_csv <- tempfile(fileext = ".csv")
  download_url <- get_gdrive_download_link(gdrive_url)
  httr::GET(download_url, httr::write_disk(temp_csv, overwrite = TRUE))
  temp_csv
}

highlight_presenters <- function(authors, presenters) {
  if (is.na(presenters) || presenters == "") return(authors)
  str_split(presenters, ";")[[1]] %>%
    str_trim() %>%
    reduce(~ str_replace_all(.x, fixed(.y), glue("<u>{.y}</u>")), .init = authors)
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
