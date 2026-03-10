#' This is a helper function for the final exam
#'
#'This function will load the data for this section's final exam
#' @export
final_load <- function() {
  tm <- Sys.time()
  usr <- Sys.getenv("RSTUDIO_USER_IDENTITY_DISPLAY")

  if (!file.exists("pbgc_clean.csv")) {
    df <- utils::read.csv(
      "https://raw.githubusercontent.com/ECO230/rstudio-course/master/pbgc_clean.csv"
    )

    fud <- tm -
      lubridate::years(2) -
      lubridate::period(num = 1, units = "month") -
      lubridate::days(7) -
      lubridate::minutes(42)

    Sys.setenv(R_FINAL_FILE_TIME = as.character(tm))
    Sys.setenv(R_FINAL_USER = usr)

    df$ID <- seq_len(nrow(df))
    hash <- digest::digest(usr, algo = "sha256")
    df$PIN <- paste0(hash, df$ID)
    df$Data_Accessed <- fud
    df$ID <- NULL

    utils::write.csv(df, "pbgc_clean.csv", row.names = FALSE)

    dhash <- digest::digest(file = "pbgc_clean.csv", algo = "sha256")
    Sys.setenv(R_FILE_HASH = dhash)

  } else {
    if (Sys.getenv("R_FINAL_FILE_TIME") == "") {
      Sys.setenv(R_FINAL_FILE_TIME = as.character(tm))
    }

    if (Sys.getenv("R_FINAL_USER") == "") {
      Sys.setenv(R_FINAL_USER = usr)
    }

    if (Sys.getenv("R_FILE_HASH") == "") {
      dhash <- digest::digest(file = "pbgc_clean.csv", algo = "sha256")
      Sys.setenv(R_FILE_HASH = dhash)
    }
  }

  invisible(NULL)
}
