#!/usr/bin/env Rscript
# copy_cv_to_site.R — sync your local cv_html into your quarto site

# exit in CI
if (identical(tolower(Sys.getenv("CI")), "true")) {
  stop("🚫  copy_cv_to_site.R is intended for local use only.")
}

# ---- CONFIGURE THESE TWO ----
source_dir <- "/Users/sorpet/code/1_active/1_personal/cv_html"
target_dir <- "cv"
# ------------------------------

# sanity checks
if (!dir.exists(source_dir)) {
  stop("Source directory does not exist: ", source_dir)
}

required <- c("main.html", "style.css", "scripts/script.js", "scripts/buildMd.js")
miss <- required[!file.exists(file.path(source_dir, required))]
if (length(miss)) {
  stop("Missing required source files: ", paste(miss, collapse = ", "))
}

# remove old and re-create
if (dir.exists(target_dir)) {
  unlink(target_dir, recursive = TRUE, force = TRUE)
  message("🗑️  Removed existing ‘", target_dir, "’")
}
dir.create(target_dir, recursive = TRUE)
message("📁  Created ‘", target_dir, "’")

# copy everything recursively
copy_ok <- file.copy(
  from = list.files(source_dir, full.names = TRUE),
  to   = target_dir,
  recursive = TRUE
)
if (!all(copy_ok)) {
  bad <- list.files(source_dir, full.names=TRUE)[!copy_ok]
  stop("Failed to copy: ", paste(bad, collapse = ", "))
}
message("✅  Full copy from ‘", source_dir, "’ to ‘", target_dir, "’")
## Remove markdown files so Quarto won't re-process them and override fragments
content_dir <- file.path(target_dir, "content")
md_files <- list.files(content_dir, pattern = "\\.md$", full.names = TRUE)
if (length(md_files) > 0) {
  unlink(md_files, recursive = FALSE, force = TRUE)
  message("🗑️  Removed markdown source files from content/: ", paste(basename(md_files), collapse = ", "))
}

# post‐processing: strip phone from personal_data.html
pd <- file.path(target_dir, "content", "personal_data.html")
if (file.exists(pd)) {
  lines <- readLines(pd)
  lines <- lines[!grepl("\\+45\\s*5176\\s*1832", lines)]
  writeLines(lines, pd)
  message("✂️  Phone number removed from personal_data.html")
} else {
  warning("personal_data.html not found at ", pd)
}

message("🎉  CV files copied successfully.")

#
# # DOn't run in CI
# if (Sys.getenv("CI") == "true") {
#   stop("copy_cv_to_site.R is intended for local use only.")
# }
#
# # Paths
# source_dir <- "/Users/sorpet/code/1_active/1_personal/cv_html"
# target_dir <- "cv"
#
# # Clean old folder
# if (dir.exists(target_dir)) {
#   unlink(target_dir, recursive = TRUE)
#   message("Old cv/ folder removed.")
# }
#
# # Create necessary folders
# dir.create(target_dir, showWarnings = FALSE)
# dir.create(file.path(target_dir, "images"), showWarnings = FALSE)
# dir.create(file.path(target_dir, "content"), showWarnings = FALSE)
# dir.create(file.path(target_dir, "scripts"), showWarnings = FALSE)
#
# # Copy main files
# file.copy(file.path(source_dir, "main.html"), file.path(target_dir, "main.html"), overwrite = TRUE)
# file.copy(file.path(source_dir, "style.css"), file.path(target_dir, "style.css"), overwrite = TRUE)
# file.copy(file.path(source_dir, "scripts", "script.js"), file.path(target_dir, "scripts", "script.js"), overwrite = TRUE)
#
# # Copy images
# file.copy(
#   from = list.files(file.path(source_dir, "images"), full.names = TRUE),
#   to = file.path(target_dir, "images"),
#   overwrite = TRUE
# )
#
# # Copy content files
# content_files <- list.files(file.path(source_dir, "content"), full.names = TRUE)
# for (file in content_files) {
#   target_path <- file.path(target_dir, "content", basename(file))
#
#   if (basename(file) == "personal_data.html") {
#     # Read, filter out line with phone number, and write
#     lines <- readLines(file)
#     lines <- lines[!grepl("\\+45 5176 1832", lines)]
#     writeLines(lines, target_path)
#     message("Removed phone number from personal_data.html.")
#   } else {
#     file.copy(file, target_path, overwrite = TRUE)
#   }
# }

# message("CV files copied successfully.")
