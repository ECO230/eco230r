#' This is a helper function for the final exam
#'
#'This function will tell how much time has passed since loading the file
#' @export
final_elapsed <- function(){
library(lubridate)
  st <- ymd_hms(Sys.getenv('R_FINAL_FILE_TIME'))
  en <- ymd_hms(Sys.time())
  tint <- st %--% en
  elps <- as.duration(tint)

  return(elps)
}
