final_user <- function() {
  truth <- Sys.getenv('RSTUDIO_USER_IDENTITY_DISPLAY') == Sys.getenv('R_FINAL_USER')
  test <- paste('User',Sys.getenv('RSTUDIO_USER_IDENTITY_DISPLAY'),'Login Match:',as.character(truth))
  return(test)
}
