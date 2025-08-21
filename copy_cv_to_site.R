# exit in CI
if (identical(tolower(Sys.getenv("CI")), "true")) {
  stop("🚫  copy_cv_to_site.R is intended for local use only.")
}

# ---- CONFIGURE THESE TWO ----
source_dir <- "/Users/sorpet/code/cv_html"
target_dir <- "cv"
# ------------------------------

# sanity checks
if (!dir.exists(source_dir)) {
  stop("Source directory does not exist: ", source_dir)
}

required <- c("main.html", "style.css",
              "scripts/script.js", "scripts/buildMd.js",
              "content_html/personal_data.html")
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
dir.create(file.path(target_dir, "scripts"))
dir.create(file.path(target_dir, "images"))
dir.create(file.path(target_dir, "content_html"))
dir.create(file.path(target_dir, "data"))
message("📁  Created ‘", target_dir, "’ and subdirectories")

# copy core files
file.copy(file.path(source_dir, "main.html"), file.path(target_dir, "main.html"))
file.copy(file.path(source_dir, "style.css"), file.path(target_dir, "style.css"))

# copy scripts
script_files <- c("script.js", "buildMd.js")
file.copy(file.path(source_dir, "scripts", script_files),
          file.path(target_dir, "scripts", script_files))

# copy images
img_files <- list.files(file.path(source_dir, "images"), full.names = TRUE)
file.copy(img_files, file.path(target_dir, "images"), overwrite = TRUE)

# copy data (e.g., paths.yml)
data_files <- list.files(file.path(source_dir, "data"), full.names = TRUE)
file.copy(data_files, file.path(target_dir, "data"), overwrite = TRUE)

# copy content HTML files (excluding .md files)
content_html_files <- list.files(file.path(source_dir, "content_html"), full.names = TRUE)
file.copy(content_html_files, file.path(target_dir, "content_html"), overwrite = TRUE)

# remove markdown files from target/content_html (just in case)
md_files <- list.files(file.path(target_dir, "content_html"), pattern = "\\.md$", full.names = TRUE)
if (length(md_files) > 0) {
  unlink(md_files)
  message("🗑️  Removed markdown files from content_html/: ", paste(basename(md_files), collapse = ", "))
}

# strip phone number from personal_data.html
pd <- file.path(target_dir, "content_html", "personal_data.html")
if (file.exists(pd)) {
  lines <- readLines(pd)
  lines <- lines[!grepl("\\+45\\s*5176\\s*1832", lines)]
  writeLines(lines, pd)
  message("✂️  Phone number removed from personal_data.html")
} else {
  warning("personal_data.html not found at ", pd)
}

message("🎉  CV files copied successfully.")
