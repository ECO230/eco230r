#' This is a helper function for the final exam
#'
#'This function will tell if there is an issue with the data file
#' @export
final_file <- function(){
library(digest)
    truth <- read_csv('pbgc_clean.csv')[[1,1]] == paste(digest(Sys.getenv('RSTUDIO_USER_IDENTITY_DISPLAY'),algo='sha256'),1,sep='')[1]
  test <- paste('File Hash Match:',as.character(truth))
  return(test)
}
