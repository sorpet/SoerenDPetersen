library(bibtex)
library(RefManageR)
library(dplyr)
library(stringr)
library(purrr)
library(glue)
library(readr)
library(here)
library(yaml)
library(htmltools)

# https://www.landbrugsinfo.dk/-/media/landbrugsinfo/public/7/e/a/pm/_20/_3865/_oversigten2020/_249-253.pdf
# https://www.landbrugsinfo.dk/-/media/landbrugsinfo/public/7/e/a/pm/_20/_3865/_oversigten2020/_249-253.pdf
#

"https://www.landbrugsinfo.dk/-/media/landbrugsinfo/public/7/e/a/pm_20_3865_oversigten2020_249-253.pdf" |> clean_url()

#source(here("R/utils_cv.R"))

#--------- Configuration ---------#
load_publication_paths <- function(config_path = here("data", "paths.yml")) {
  config <- read_yaml(config_path)

  get_direct_gdrive_link <- function(link) {
    # Extract file ID from Google Drive share/view link
    if (str_detect(link, "drive.google.com")) {
      file_id <- str_match(link, "[-\\w]{25,}")[1]
      if (!is.na(file_id)) {
        return(glue::glue("https://drive.google.com/uc?export=download&id={file_id}"))
      }
    }
    return(link)
  }

  if (!is.null(config$publications$bib_gdrive)) {
    direct_url <- get_direct_gdrive_link(config$publications$bib_gdrive)
    temp_bib <- tempfile(fileext = ".bib")
    tryCatch({
      download.file(direct_url, destfile = temp_bib, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      stop("❌ Failed to download .bib file from Google Drive: ", e$message)
    })
    bib_path <- temp_bib
  } else {
    bib_path <- here(config$publications$bib)
  }

  list(
    bib = bib_path,
    html = here(config$publications$html)
  )
}

save_html_output <- function(html_block, output_path) {
  write_lines(html_block, output_path)
  message("✅ publications.html generated.")
}

#--------- LaTeX Cleaning ---------#
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
    # str_replace_all(fixed("5$prime$"), "5′") %>%
    # str_replace_all(fixed("$prime$"), "′") %>%
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
    str_replace_all("\\{\\\\'\\{([aeiouAEIOU])\\}\\}", "\\1́") %>%  # e.g. {\'{a}} → á
    str_replace_all("\\{\\\\\"\\{([aeiouAEIOU])\\}\\}", "\\1̈") %>% # e.g. {\"{o}} → ö
    str_replace_all("\\{\\\\`\\{([aeiouAEIOU])\\}\\}", "\\1̀") %>%  # e.g. {\`{a}} → à
    str_replace_all("\\{\\\\~\\{([aonAON])\\}\\}", "\\1̃") %>%      # e.g. {\~{n}} → ñ
    str_replace_all("\\{\\\\c\\{c\\}\\}", "ç") %>%
    str_replace_all("\\{\\\\ae\\}", "æ") %>%
    str_replace_all("\\{\\\\o\\}", "ø") %>%
    str_replace_all("\\{\\\\aa\\}", "å") %>%
    str_replace_all("\\{\\\\ss\\}", "ß") %>%
    str_replace_all("\\{\\\\'([aeiouAEIOU])\\}", "\\1́") %>%        # {'a} → á
    str_replace_all("\\$\\\\prime\\$", "′") %>%
    str_replace_all("\\{\\$\\\\prime\\$\\}", "′") %>%
    str_replace_all("\\{\\{([^}]+)\\}\\}", "\\1") %>%              # Remove double braces {{foo}} → foo
    str_replace_all("\\{([^{}]+)\\}", "\\1") %>%                   # Remove remaining single-level braces
    str_replace_all("\\\\%", "%") %>%
    str_replace_all("\\\\&", "&") %>%
    str_replace_all("\\\\_", "_") %>%
    # str_replace_all("\\\\", "") %>%
    str_replace_all("--", "–") %>%
    str_replace_all("\\.\\.", ".") %>%
    str_replace_all("\\{\\\\v\\{([cCsSzZ])\\}\\}", "\u030C\\1") %>%  # caron: č, š, ž (uses combining caron)
    str_replace_all("\\{\\\\=\\{([aeiouAEIOU])\\}\\}", "\u0304\\1") %>%  # macron: ū, ā, etc. (combining macron)
    str_squish() %>%
    str_remove("\\.$")
}

clean_url <- function(url) {
  url %>%
    str_replace_all("\\\\_", "_") %>%  # unescape LaTeX-style underscores
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

generate_html_list <- function(df) {
  footnotes <- list()
  html_list <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    # Extract and clean citation note: remove footnote markup and optional prefix
    note_text <- row$note %>%
      str_remove("footnote=.*$") %>%
      str_replace("^Additional citation text:\\s*", "") %>%
      str_replace("\\.$", "")
    note_final <- if (!is.na(note_text) && str_trim(note_text) != "") glue("{str_trim(note_text)}.") else NA_character_

    if (!is.na(row$footnote_raw)) {
      footnotes[[as.character(row$index)]] <- glue("<div class='footnote'><sup>*</sup> {row$footnote_raw}</div>")
    }

    info <- case_when(
      row$bibtype == "InCollection" ~ glue("{row$editor_string} {row$booktitle}. {row$publisher}, s. {row$pages}.") %>% str_squish(),
      !is.na(row$journal) & !is.na(note_final) ~ glue("{row$journal}. {note_final}"),
      !is.na(row$journal) ~ glue("{row$journal}.") %>% str_trim(),
      !is.na(note_final) ~ note_final,
      TRUE ~ ""
    )

    link <- if (!is.na(row$doi) && row$doi != "") {
      tags$a(href = glue("https://doi.org/{row$doi}"), target = "_blank", info)
    } else if (!is.na(row$url) && row$url != "") {
      tags$a(href = row$url, target = "_blank", info)
    } else {
      info
    }

    html_list[[i]] <- tags$li(
      id = glue("pub-{row$index}"),
      `data-id` = row$slug,
      HTML(glue("{row$author_string} ({row$year}). <em>{row$title}</em>. {as.character(link)}"))
    )
  }

  list(html = as.character(map_chr(html_list, as.character)), footnotes = footnotes)
}

#--------- Main ---------#
paths <- load_publication_paths()
stopifnot(file.exists(paths$bib))

bib_entries <- ReadBib(paths$bib, check = FALSE, .Encoding = "UTF-8")

bib_df <- tibble::tibble(
  bibtype = sapply(bib_entries, function(x) x$bibtype),
  title = sapply(bib_entries, function(x) x$title),
  author = lapply(bib_entries, function(x) x$author),
  journal = sapply(bib_entries, function(x) x$journal %||% NA_character_),
  booktitle = sapply(bib_entries, function(x) x$booktitle %||% NA_character_),
  editor = lapply(bib_entries, function(x) x$editor %||% list()),
  publisher = sapply(bib_entries, function(x) x$publisher %||% NA_character_),
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

peer_reviewed_df <- bib_df %>% filter(!str_detect(tolower(note), "not peer reviewed") | is.na(note))
non_peer_reviewed_df <- bib_df %>%
  filter(str_detect(tolower(note), "not peer reviewed")) %>%
  mutate(note = str_remove(note, "[;,.]?\\s*[Nn]ot peer reviewed"))

peer_result <- generate_html_list(peer_reviewed_df)
nonpeer_result <- generate_html_list(non_peer_reviewed_df)

html_block <- as.character(
  tagList(
    tags$section(class = "publications",
                 tags$div(class = "h2-body",
                          tags$h3(class = "subsection-heading", "Peer-reviewed"),
                          tags$ol(reversed = NA, HTML(paste(peer_result$html, collapse = "\n"))),
                          tags$h3(class = "subsection-heading", "Not peer-reviewed"),
                          tags$ol(reversed = NA, HTML(paste(nonpeer_result$html, collapse = "\n"))),
                          HTML(paste(unlist(c(peer_result$footnotes, nonpeer_result$footnotes)), collapse = "\n"))
                 )
    )
  )
)

save_html_output(html_block, paths$html)