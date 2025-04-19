# Paths
source_dir <- "/Users/sorpet/code/1_active/1_personal/cv_html"
target_dir <- "cv"

# Clean old folder
if (dir.exists(target_dir)) {
  unlink(target_dir, recursive = TRUE)
  message("Old cv/ folder removed.")
}

# Create necessary folders
dir.create(target_dir, showWarnings = FALSE)
dir.create(file.path(target_dir, "images"), showWarnings = FALSE)
dir.create(file.path(target_dir, "content"), showWarnings = FALSE)
dir.create(file.path(target_dir, "scripts"), showWarnings = FALSE)

# Copy main files
file.copy(file.path(source_dir, "main.html"), file.path(target_dir, "main.html"), overwrite = TRUE)
file.copy(file.path(source_dir, "style.css"), file.path(target_dir, "style.css"), overwrite = TRUE)
file.copy(file.path(source_dir, "scripts", "script.js"), file.path(target_dir, "scripts", "script.js"), overwrite = TRUE)

# Copy images
file.copy(
  from = list.files(file.path(source_dir, "images"), full.names = TRUE),
  to = file.path(target_dir, "images"),
  overwrite = TRUE
)

# Copy content files
content_files <- list.files(file.path(source_dir, "content"), full.names = TRUE)
for (file in content_files) {
  target_path <- file.path(target_dir, "content", basename(file))

  if (basename(file) == "personal_data.html") {
    # Read, filter out line with phone number, and write
    lines <- readLines(file)
    lines <- lines[!grepl("\\+45 5176 1832", lines)]
    writeLines(lines, target_path)
    message("Removed phone number from personal_data.html.")
  } else {
    file.copy(file, target_path, overwrite = TRUE)
  }
}

message("CV files copied successfully.")
