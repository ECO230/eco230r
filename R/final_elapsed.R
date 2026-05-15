#' This is a helper function for the final exam
#'
#'This function will tell how much time has passed since loading the file
#' @export
final_elapsed <- function() {
  st <- lubridate::ymd_hms(Sys.getenv("R_FINAL_FILE_TIME"))
  en <- lubridate::ymd_hms(as.character(Sys.time()))

  tint <- lubridate::`%--%`(st, en)
  elps <- as.character(lubridate::as.duration(tint))

  now <- as.character(Sys.time())
  est <- Sys.getenv("R_FINAL_FILE_TIME")

  frt <- max(readr::read_csv("pbgc_clean.csv", show_col_types = FALSE)$Data_Accessed)
  fud <- frt + lubridate::years(2) + lubridate::period(num = 1, units = "month") +
    lubridate::days(7) + lubridate::minutes(42)

  fl <- paste("File Accessed:", fud)
  ft <- paste("Environment Started:", est)
  fn <- paste("Current Time:", now)
  fe <- paste("Elapsed Time:", elps)

  c(fl, ft, fn, fe)
}

