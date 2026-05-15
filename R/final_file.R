#' This is a helper function for the final exam
#'
#'This function will tell if there is an issue with the data file
#' @export
final_file <- function() {
  if (!file.exists("pbgc_clean.csv")) {
    stop("File 'pbgc_clean.csv' was not found in the working directory.", call. = FALSE)
  }

  stored_hash <- Sys.getenv("R_FILE_HASH", unset = NA_character_)

  if (is.na(stored_hash) || stored_hash == "") {
    stop("R_FILE_HASH is not set.", call. = FALSE)
  }

  current_hash <- digest::digest(file = "pbgc_clean.csv", algo = "sha256")
  truth <- identical(current_hash, stored_hash)

  c(
    paste("File Hash Match:", truth),
    paste("Stored Hash:", stored_hash)
  )
}
