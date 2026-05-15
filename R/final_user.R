#' This is a helper function for the final exam
#'
#'This function will tell if there are login issues
#' @export
final_user <- function() {
  current_user <- Sys.getenv("RSTUDIO_USER_IDENTITY_DISPLAY")
  stored_user  <- Sys.getenv("R_FINAL_USER")

  truth <- identical(current_user, stored_user)

  paste("User", current_user, "Login Match:", truth)
}
