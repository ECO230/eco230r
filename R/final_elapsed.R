#' This is a helper function for the final exam
#'
#'This function will tell how much time has passed since loading the file
#' @export
final_elapsed <- function(){
  library(lubridate)
  st <- ymd_hms(Sys.getenv('R_FINAL_FILE_TIME'))
  en <- ymd_hms(Sys.time())
  tint <- st %--% en
  elps <- as.character(as.duration(tint))

  now <-as.character(Sys.time())
  est <- as.character(Sys.getenv('R_FINAL_FILE_TIME'))
  frt <- max(read_csv('pbgc_clean.csv')$Data_Accessed)
  fud <- frt + years(2) + months(1) + days(7) + minutes(42)

  fl <- paste('File Accessed:',fud)
  ft <- paste('Environment Started:',est)
  fn <- paste('Current Time:',now)
  fe <- paste('Elapsed Time:',elps)
  return(c(fl,ft,fn,fe))
}

