#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[startsWith(args, file_arg)][1])

if (is.na(script_path) || script_path == "") {
  script_path <- "cv/scripts/render_cv_fragments.R"
}

cv_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(cv_root)

source(file.path("scripts", "generate_cv_sections.R"))
