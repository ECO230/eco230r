#' This is a helper function for the final exam
#'
#'This function will tell how much time has passed since loading the file
#' @export
final_elapsed <- function(){
  st <- ymd_hms(Sys.getenv('R_FINAL_FILE_TIME'))
  en <- ymd_hms(Sys.time())
  tint <- st %--% en
  elps <- as.character(as.duration(tint))

  now <-as.character(Sys.time())
  est <- as.character(Sys.getenv('R_FINAL_FILE_TIME'))
  fud <- (1000*60*60*21)
  frt <- max(read_csv('pbgc_clean.csv')$Data_Accessed) + fud

  fl <- paste('File Accessed:',frt)
  ft <- paste('Environment Started:',est)
  fn <- paste('Current Time:',now)
  fe <- paste('Elapsed Time:',elps)
  return(c(fl,ft,fn,fe))
}
