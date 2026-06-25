#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(yaml))

args <- commandArgs(trailingOnly = TRUE)
script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", script_args[startsWith(script_args, file_arg)][1])

if (is.na(script_path) || script_path == "") {
  script_path <- "cv/scripts/sync_drive_snapshots.R"
}

cv_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(cv_root)

usage <- function() {
  cat(
    "Usage: Rscript --vanilla cv/scripts/sync_drive_snapshots.R --check|--apply\n",
    "\n",
    "  --check  Compare raw Drive sources to repo snapshots; exit nonzero on drift.\n",
    "  --apply  Refresh raw synced snapshots from Drive sources.\n",
    "\n",
    "Projection snapshots are reported but not overwritten by this script.\n",
    sep = ""
  )
}

if (length(args) != 1 || !args %in% c("--check", "--apply")) {
  usage()
  quit(status = 2)
}

mode <- sub("^--", "", args[[1]])

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || is.na(a) || identical(a, "")) b else a
}

resolve_path <- function(path) {
  if (is.null(path) || is.na(path) || path == "") return(NULL)
  if (grepl("^/", path)) return(path)
  file.path(cv_root, path)
}

read_text_file <- function(path) {
  size <- file.info(path)$size
  if (is.na(size)) stop("Cannot stat file: ", path)
  if (size == 0) return("")
  rawToChar(readBin(path, what = "raw", n = size))
}

normalize_text <- function(text) {
  text <- enc2utf8(text)
  text <- gsub("\r\n?", "\n", text, perl = TRUE)
  text <- gsub("[ \t]+(?=\n)", "", text, perl = TRUE)
  text <- sub("[ \t]+$", "", text, perl = TRUE)
  paste0(sub("\n*$", "", text, perl = TRUE), "\n")
}

extract_gdrive_id <- function(url) {
  file_id <- regmatches(url, regexpr("(?<=/d/)[^/]+", url, perl = TRUE))
  if (length(file_id) && nzchar(file_id)) return(file_id)

  file_id <- regmatches(url, regexpr("(?<=[?&]id=)[^&]+", url, perl = TRUE))
  if (length(file_id) && nzchar(file_id)) return(file_id)

  stop("Could not extract Google Drive file ID from URL: ", url)
}

download_source <- function(url, type) {
  file_id <- extract_gdrive_id(url)
  ext <- switch(type, csv = ".csv", bib = ".bib", ".txt")
  temp_path <- tempfile(fileext = ext)

  download_urls <- if (grepl("docs.google.com/spreadsheets", url)) {
    c(
      sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=%s", file_id, type),
      sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)
    )
  } else {
    sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)
  }

  errors <- character()
  for (download_url in download_urls) {
    result <- try(
      utils::download.file(download_url, temp_path, mode = "wb", quiet = TRUE),
      silent = TRUE
    )
    if (inherits(result, "try-error")) {
      errors <- c(errors, conditionMessage(attr(result, "condition")))
      next
    }
    if (file.exists(temp_path) && file.info(temp_path)$size > 0) {
      return(temp_path)
    }
    errors <- c(errors, paste("empty response from", download_url))
  }

  stop("Could not download Drive source: ", url, "\n", paste(errors, collapse = "\n"))
}

collect_sources <- function(node, prefix = character()) {
  sources <- list()

  if (is.list(node) && !is.null(node$canonical_drive)) {
    canonical <- node$canonical_drive
    sources <- c(sources, list(list(
      name = paste(prefix, collapse = "."),
      type = canonical$type,
      url = canonical$url,
      local_path = canonical$local_path,
      snapshot = canonical$snapshot
    )))
  }

  if (!is.list(node)) return(sources)

  node_names <- names(node)
  for (idx in seq_along(node)) {
    child_name <- if (!is.null(node_names) && nzchar(node_names[[idx]])) {
      node_names[[idx]]
    } else {
      as.character(idx)
    }
    if (child_name == "canonical_drive") next

    child <- node[[idx]]
    if (!is.list(child)) next

    label <- child$name %||% child_name
    sources <- c(sources, collect_sources(child, c(prefix, label)))
  }

  sources
}

config <- yaml::read_yaml(file.path("data", "paths.yml"))
sources <- unlist(
  lapply(names(config), function(name) collect_sources(config[[name]], name)),
  recursive = FALSE
)

if (!length(sources)) {
  stop("No canonical_drive sources found in data/paths.yml")
}

raw_types <- c("csv", "bib")
drifted <- character()
failed <- character()

for (source in sources) {
  type <- source$type %||% ""
  name <- source$name
  snapshot_path <- resolve_path(source$snapshot)
  local_path <- resolve_path(source$local_path)

  if (!type %in% raw_types) {
    cat(sprintf("SKIP %-35s %-15s projection source; not raw-overwritten\n", name, type))
    next
  }

  source_path <- tryCatch({
    if (!is.null(local_path) && file.exists(local_path)) {
      local_path
    } else {
      download_source(source$url, type)
    }
  }, error = function(err) {
    failed <<- c(failed, sprintf("%s: %s", name, conditionMessage(err)))
    NA_character_
  })

  if (is.na(source_path)) next

  source_text <- normalize_text(read_text_file(source_path))
  snapshot_exists <- !is.null(snapshot_path) && file.exists(snapshot_path)
  snapshot_text <- if (snapshot_exists) normalize_text(read_text_file(snapshot_path)) else ""
  aligned <- snapshot_exists && identical(source_text, snapshot_text)

  if (aligned) {
    cat(sprintf("OK   %-35s %s\n", name, source$snapshot))
    next
  }

  drifted <- c(drifted, name)
  if (mode == "apply") {
    dir.create(dirname(snapshot_path), recursive = TRUE, showWarnings = FALSE)
    con <- file(snapshot_path, open = "wb")
    writeBin(charToRaw(source_text), con)
    close(con)
    cat(sprintf("APPLY %-35s %s\n", name, source$snapshot))
  } else {
    cat(sprintf("DRIFT %-35s %s\n", name, source$snapshot))
  }
}

if (length(failed)) {
  cat("\nInaccessible sources:\n")
  cat(paste0("- ", failed, collapse = "\n"), "\n")
}

if (mode == "check" && (length(drifted) || length(failed))) {
  quit(status = 1)
}

if (mode == "apply" && length(failed)) {
  quit(status = 1)
}
