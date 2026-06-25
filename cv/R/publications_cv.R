library(bibtex)
library(RefManageR)
library(dplyr)
library(glue)
library(here)
library(purrr)
library(stringr)
library(tibble)
library(yaml)

source(here("R", "utils_cv.R"))

load_publication_paths <- function(config_path = here("data", "paths.yml")) {
  config <- read_yaml(config_path)

  list(
    bib = resolve_configured_file(config$publications, local_keys = c("bib")),
    html = here(config$publications$html),
    preserve_existing_html = isTRUE(config$publications$preserve_existing_html)
  )
}

replace_unicode <- function(x) {
  x %>%
    str_replace_all("\\{\\\\'a\\}", "á") %>%
    str_replace_all("\\{\\\\'e\\}", "é") %>%
    str_replace_all("\\{\\\\'i\\}", "í") %>%
    str_replace_all("\\{\\\\'o\\}", "ó") %>%
    str_replace_all("\\{\\\\'u\\}", "ú") %>%
    str_replace_all("\\{\\ae\\}", "æ") %>%
    str_replace_all("\\{\\o\\}", "ø") %>%
    str_replace_all("\\{\\aa\\}", "å")
}

replace_math_symbols <- function(x) {
  x %>%
    str_replace_all(regex("\\.\\."), ".") %>%
    str_replace_all("\\{\\$<\\$\\}", "<") %>%
    str_replace_all("\\{\\$>\\$\\}", ">") %>%
    str_replace_all("\\$lt\\$", "<") %>%
    str_replace_all("\\$gt\\$", ">")
}

clean_latex <- function(x) {
  x %>%
    replace_math_symbols() %>%
    replace_unicode() %>%
    str_replace_all("\\\\=\\s*u", "ū") %>%
    str_replace_all("\\\\=\\s*a", "ā") %>%
    str_replace_all("\\\\=\\s*e", "ē") %>%
    str_replace_all("\\\\v\\s*c", "č") %>%
    str_replace_all("\\\\v\\s*s", "š") %>%
    str_replace_all("\\\\v\\s*z", "ž") %>%
    str_replace_all("\\{\\\\'\\{([aeiouAEIOU])\\}\\}", "\\1́") %>%
    str_replace_all("\\{\\\\\"\\{([aeiouAEIOU])\\}\\}", "\\1̈") %>%
    str_replace_all("\\{\\\\`\\{([aeiouAEIOU])\\}\\}", "\\1̀") %>%
    str_replace_all("\\{\\\\~\\{([aonAON])\\}\\}", "\\1̃") %>%
    str_replace_all("\\{\\\\c\\{c\\}\\}", "ç") %>%
    str_replace_all("\\{\\\\ae\\}", "æ") %>%
    str_replace_all("\\{\\\\o\\}", "ø") %>%
    str_replace_all("\\{\\\\aa\\}", "å") %>%
    str_replace_all("\\{\\\\ss\\}", "ß") %>%
    str_replace_all("\\{\\\\'([aeiouAEIOU])\\}", "\\1́") %>%
    str_replace_all("\\$\\\\prime\\$", "′") %>%
    str_replace_all("\\{\\$\\\\prime\\$\\}", "′") %>%
    str_replace_all("\\{\\{([^}]+)\\}\\}", "\\1") %>%
    str_replace_all("\\{([^{}]+)\\}", "\\1") %>%
    str_replace_all("\\\\%", "%") %>%
    str_replace_all("\\\\&", "&") %>%
    str_replace_all("\\\\_", "_") %>%
    str_replace_all("--", "–") %>%
    str_replace_all("\\.\\.", ".") %>%
    str_replace_all("\\{\\\\v\\{([cCsSzZ])\\}\\}", "\u030C\\1") %>%
    str_replace_all("\\{\\\\=\\{([aeiouAEIOU])\\}\\}", "\u0304\\1") %>%
    str_squish() %>%
    str_remove("\\.$")
}

clean_url <- function(url) {
  url %>%
    str_replace_all("\\\\_", "_") %>%
    gsub("(?<=/)(_|%5[Ff])(?=[0-9a-zA-Z]*-)", "", ., perl = TRUE)
}

make_slug <- function(...) {
  paste(..., sep = " ") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_remove_all("(^-|-$)")
}

format_authors <- function(authors, has_footnote = FALSE) {
  sapply(authors, function(p) {
    family <- p$family[[1]]
    given_parts <- unlist(p$given)
    initials <- paste0(substr(given_parts, 1, 1), collapse = "")
    full <- glue("{family} {initials}")
    full <- if (family == "Petersen" && initials == "SD") glue("<strong>{full}</strong>") else full
    if (has_footnote && family == "Petersen" && initials == "SD") glue("{full}<sup>*</sup>") else full
  }) |> paste(collapse = ", ")
}

load_publications_df <- function(bib_path) {
  bib_entries <- ReadBib(bib_path, check = FALSE, .Encoding = "UTF-8")

  tibble(
    bibtype = sapply(bib_entries, function(x) x$bibtype),
    title = sapply(bib_entries, function(x) x$title),
    author = lapply(bib_entries, function(x) x$author),
    journal = sapply(bib_entries, function(x) x$journal %||% NA_character_),
    booktitle = sapply(bib_entries, function(x) x$booktitle %||% NA_character_),
    editor = lapply(bib_entries, function(x) x$editor %||% list()),
    publisher = sapply(bib_entries, function(x) x$publisher %||% NA_character_),
    volume = sapply(bib_entries, function(x) x$volume %||% NA_character_),
    number = sapply(bib_entries, function(x) x$number %||% NA_character_),
    pages = sapply(bib_entries, function(x) x$pages %||% NA_character_),
    year = sapply(bib_entries, function(x) x$year),
    month = sapply(bib_entries, function(x) x$month %||% NA_character_),
    url = sapply(bib_entries, function(x) x$url %||% NA_character_),
    doi = sapply(bib_entries, function(x) x$doi %||% NA_character_),
    note = sapply(bib_entries, function(x) x$note %||% NA_character_),
    abstract = sapply(bib_entries, function(x) x$abstract %||% NA_character_),
    file = sapply(bib_entries, function(x) x$file %||% NA_character_),
    keywords = sapply(bib_entries, function(x) x$keywords %||% NA_character_)
  ) %>%
    mutate(
      footnote_raw = clean_latex(str_match(note, "footnote=(.*)$")[, 2]),
      has_footnote = !is.na(footnote_raw),
      author_string = map2_chr(author, has_footnote, ~ clean_latex(format_authors(.x, .y))),
      editor_string = map_chr(editor, ~ if (length(.x) > 0) glue("I: {clean_latex(format_authors(.x))} (red.),") else ""),
      title = clean_latex(title),
      journal = clean_latex(journal),
      note = clean_latex(note),
      abstract = clean_latex(abstract),
      booktitle = clean_latex(booktitle),
      publisher = clean_latex(publisher),
      volume = clean_latex(volume),
      number = clean_latex(number),
      pages = clean_latex(pages),
      year = as.character(year),
      url = clean_url(url),
      doi = as.character(doi),
      numeric_year = as.numeric(str_extract(year, "\\d{4}"))
    ) %>%
    arrange(desc(numeric_year)) %>%
    mutate(
      index = rev(row_number()),
      slug = make_slug(bibtype, title)
    )
}

make_publication_lookup <- function(bib_df) {
  bib_df %>%
    filter(!is.na(doi), doi != "") %>%
    select(doi, index)
}

linkify_dois_to_publications <- function(text, bib_lookup) {
  doi_regex <- "\\[doi:([^\\]]+)\\]"

  map_chr(as.character(text), function(value) {
    if (is.na(value) || value == "") return(value)

    str_replace_all(value, doi_regex, function(match) {
      doi_value <- str_match(match, doi_regex)[, 2]
      pub_index <- bib_lookup %>%
        filter(.data$doi == .env$doi_value) %>%
        pull(index)

      if (length(pub_index) > 0) {
        as.character(htmltools::a(href = glue("#pub-{pub_index[[1]]}"), paste0("publication no. ", pub_index[[1]])))
      } else {
        match
      }
    })
  })
}
