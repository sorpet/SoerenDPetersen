#!/usr/bin/env Rscript
# Generate a markdown summary of publication metrics using BibTeX and Google Scholar
library(scholar)
library(RefManageR)
library(yaml)
library(glue)
library(here)
library(httr)
source(here("R/utils_cv.R"))

# Load configuration
config <- yaml::read_yaml(here("data", "paths.yml"))
ps_cfg <- config$publication_summary
if (is.null(ps_cfg$md) || is.null(ps_cfg$scholar_id) || is.null(ps_cfg$orcid)) {
  stop("publication_summary configuration missing in data/paths.yml")
}


## Read BibTeX entries (download if needed) and count publications
pub_cfg <- config$publications
if (!is.null(pub_cfg$bib_gdrive)) {
  bib_url <- get_gdrive_download_link(pub_cfg$bib_gdrive)
  temp_bib <- tempfile(fileext = ".bib")
  httr::GET(bib_url, httr::write_disk(temp_bib, overwrite = TRUE))
  bib_path <- temp_bib
} else {
  bib_path <- here(pub_cfg$bib)
}
message("Reading BibTeX from ", bib_path)
bib_entries <- ReadBib(bib_path, check = FALSE, .Encoding = "UTF-8")
total_pubs <- length(bib_entries)
## Count first-author entries (surname Petersen, initials SD)
fa_flags <- sapply(bib_entries, function(e) {
  authors <- e$author
  if (length(authors) < 1) return(FALSE)
  p <- authors[[1]]
  family <- p$family[[1]]
  initials <- paste0(substr(unlist(p$given), 1, 1), collapse = "")
  identical(family, "Petersen") && identical(initials, "SD")
})
first_author_count <- sum(fa_flags)

# Fetch Google Scholar profile and metrics
message("Fetching Google Scholar profile for ID: ", ps_cfg$scholar_id)
profile <- get_profile(ps_cfg$scholar_id)
## H-index and total citations
hindex <- profile$h_index
hist <- get_citation_history(ps_cfg$scholar_id)
total_citations <- sum(hist$cites, na.rm = TRUE)

# Prepare markdown lines
formatted_date <- format(Sys.Date(), "%B %Y")
lines <- c(
  "# Publication summary",
  "",
  glue("- Total publications: {total_pubs} ({first_author_count} as first author)"),
  glue("- H-index = {hindex} (Google Scholar/{formatted_date})."),
  glue("- Total citations: {total_citations} (Google Scholar/{formatted_date})"),
  glue("- ORCID ID: {ps_cfg$orcid}")
)

# Write output
output_path <- here(ps_cfg$md)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, output_path)
message("✅ Publication summary written to ", output_path)
