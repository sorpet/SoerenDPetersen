#!/usr/bin/env Rscript
library(glue)
library(yaml)

here <- function(...) file.path(getwd(), ...)

source(here("R", "utils_cv.R"))

config <- yaml::read_yaml(here("data", "paths.yml"))
summary_config <- config$publication_summary
metrics <- summary_config$metrics

metrics <- modifyList(
  list(
    peer_reviewed_journal_publications = 10,
    first_author = 4,
    last_author = 1,
    notable_index = 5,
    notable_citations = 347,
    notable_note = "I have shared first-authorship with Dr. Zhang",
    h_index = 7,
    total_citations = 564,
    as_of_label = "Jun 2026"
  ),
  if (is.null(metrics)) list() else metrics
)

orcid <- if (is.null(summary_config$orcid)) "0000-0003-4104-5144" else summary_config$orcid
md_path <- if (is.null(summary_config$md)) "content_md/publication_summary.md" else summary_config$md
html_path <- if (is.null(summary_config$html)) "content_html/publication_summary.html" else summary_config$html

summary_md <- glue(
  "# Publication summary\n\n",
  "- Peer-reviewed journal publications: {metrics$peer_reviewed_journal_publications} ({metrics$first_author} as first author; {metrics$last_author} as last author)\n",
  "- No. {metrics$notable_index} in the list below has {metrics$notable_citations} citations ({metrics$notable_note})\n",
  "- H-index = {metrics$h_index} (Google Scholar profile/{metrics$as_of_label}).\n",
  "- Total citations: {metrics$total_citations} (Google Scholar profile/{metrics$as_of_label})\n",
  "- ORCID ID: {orcid}\n"
)

summary_html <- glue(
  "<ul>\n",
  "<li>Peer-reviewed journal publications: {metrics$peer_reviewed_journal_publications} ({metrics$first_author} as first author; {metrics$last_author} as last author)</li>\n",
  "<li>No. {metrics$notable_index} in the list below has {metrics$notable_citations} citations ({metrics$notable_note})</li>\n",
  "<li>H-index = {metrics$h_index} (Google Scholar profile/{metrics$as_of_label}).</li>\n",
  "<li>Total citations: {metrics$total_citations} (Google Scholar profile/{metrics$as_of_label})</li>\n",
  "<li>ORCID ID: {orcid}</li>\n",
  "</ul>\n"
)

dir.create(dirname(here(md_path)), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(here(html_path)), showWarnings = FALSE, recursive = TRUE)
writeLines(summary_md, here(md_path))
writeLines(summary_html, here(html_path))

message("✅ publication_summary.html generated from fixed baseline metrics.")
