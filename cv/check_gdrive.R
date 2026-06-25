library(googledrive)
library(dplyr)
library(purrr)
library(tibble)

get_drive_paths <- function(verbose = TRUE) {
  if (verbose) cat("🔐 Authenticating with Google Drive...\n")
  drive_auth()

  if (verbose) cat("📁 Fetching all files and folders...\n")
  all_files <- drive_find()

  if (verbose) cat("🧱 Building folder map...\n")
  folders <- all_files %>%
    filter(map_chr(drive_resource, ~ .x$mimeType) == "application/vnd.google-apps.folder") %>%
    transmute(
      id = id,
      name = name,
      parent_id = map_chr(drive_resource, ~ if (!is.null(.x$parents)) .x$parents[[1]] else NA_character_)
    )

  folder_map <- folders %>%
    deframe() %>%
    map(~ list(name = .x$name, parent_id = .x$parent_id))

  # Recursive function to get folder path
  build_folder_path <- function(folder_id, folder_map, path_so_far = character()) {
    if (is.na(folder_id) || !(folder_id %in% names(folder_map))) {
      return(path_so_far)
    }
    folder <- folder_map[[folder_id]]
    parent_path <- build_folder_path(folder$parent_id, folder_map, path_so_far)
    return(c(parent_path, folder$name))
  }

  if (verbose) cat("🧭 Constructing full paths...\n")
  folder_paths <- map(names(folder_map), ~ build_folder_path(.x, folder_map)) %>%
    set_names(names(folder_map))

  # Helper to build full path for each file
  get_full_path <- function(file_row) {
    parent_id <- file_row$drive_resource[[1]]$parents
    if (is.null(parent_id)) return(file_row$name)
    parent_id <- parent_id[[1]]
    parent_path <- folder_paths[[parent_id]]
    if (is.null(parent_path)) parent_path <- character()
    file.path(file.path(parent_path, fsep = "/"), file_row$name)
  }

  all_files %>%
    transmute(
      id = id,
      name = name,
      mime_type = map_chr(drive_resource, ~ .x$mimeType),
      full_path = map_chr(row_number(), ~ get_full_path(all_files[.x, ]))
    )
}

drive_file_paths <- get_drive_paths()

# View some file paths
head(drive_file_paths$full_path)