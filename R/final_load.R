#' This is a helper function for the final exam
#'
#'This function will load the data for this section's final exam
#' @export
final_load <- function(){
library(digest)
    tm <- Sys.time()
  Usr <- Sys.getenv('RSTUDIO_USER_IDENTITY_DISPLAY')

  if (!file.exists('pbgc_clean.csv')) {
    df <-read.csv('https://raw.githubusercontent.com/ECO230/rstudio-course/master/pbgc_clean.csv')
    fud <- (1000*60*60*12)
    Sys.setenv(R_FINAL_FILE_TIME = as.character(tm))
    Sys.setenv(R_FINAL_USER = Usr)
    df$ID <- seq.int(nrow(df))
    hash <- digest(Usr,algo='sha256')
    df$PIN <- paste(hash,df$ID, sep = '')
    df$Data_Accessed = tm - fud
    df <- subset(df, select = -c(ID))
    write.csv(df,'pbgc_clean.csv',row.names = FALSE)
    dhash <- digest(read_csv('pbgc_clean.csv'), algo="sha256")
    Sys.setenv(R_FILE_HASH = dhash)

  } else {
    if (Sys.getenv('R_FINAL_FILE_TIME') == '') {
      Sys.setenv(R_FINAL_FILE_TIME = as.character(tm))
    }
    if (Sys.getenv('R_FINAL_USER') == '') {
      Sys.setenv(R_FINAL_USER = Usr)
    }
    if (Sys.getenv('R_FILE_HASH') == '') {
      dhash <- digest(read_csv('pbgc_clean.csv'), algo="sha256")
      Sys.setenv(R_FILE_HASH = dhash)
    }
  }
}
