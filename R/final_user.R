#' This is a helper function for the final exam
#'
#'This function will tell if there are login issues
#' @export
final_user <- function() {
  truth <- Sys.getenv('RSTUDIO_USER_IDENTITY_DISPLAY') == Sys.getenv('R_FINAL_USER')
  test <- paste('User',Sys.getenv('RSTUDIO_USER_IDENTITY_DISPLAY'),'Login Match:',as.character(truth))
  return(test)
}
