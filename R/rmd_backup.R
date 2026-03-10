#' Backup all .Rmd files matching a pattern, if there is an exact copy already existing, with same hash, replace the most recent backup with this one timestamp is added as a suffix to the file name.
#'
#' @param search File name to search for default is * or any .Rmd file in the project folder, for a specific file only pass in file name with no file type e.g. Group_Analysis_Plan
#'
#' @export
#'
rmd_backup <- function(search = "*") {
  backup_dir <- file.path("Prompt", "rmd_backups")
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

  search_path <- paste0(search, ".Rmd")
  current_files <- Sys.glob(search_path)

  if (length(current_files) == 0) {
    return(invisible(NULL))
  }

  for (x in current_files) {
    if (!file.exists(x)) {
      next
    }

    stem <- tools::file_path_sans_ext(basename(x))
    current_hash <- rlang::hash_file(x)

    stem_escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", stem)

    backup_files <- list.files(
      path = backup_dir,
      pattern = paste0("^", stem_escaped, "_[0-9]{14}(_[[:xdigit:]]{8})?\\.Rmd$"),
      full.names = TRUE
    )

    has_match <- FALSE

    if (length(backup_files) > 0) {
      backup_hashes <- vapply(
        backup_files,
        FUN = rlang::hash_file,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
      has_match <- current_hash %in% backup_hashes
    }

    if (!has_match) {
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      short_hash <- substr(current_hash, 1, 8)

      backup_path <- file.path(
        backup_dir,
        paste0(stem, "_", timestamp, "_", short_hash, ".Rmd")
      )

      ok <- file.copy(
        from = x,
        to = backup_path,
        overwrite = FALSE,
        recursive = FALSE,
        copy.mode = TRUE
      )

      if (!ok) {
        warning("Backup failed for file: ", x, call. = FALSE)
      }
    }
  }

  invisible(NULL)
}
