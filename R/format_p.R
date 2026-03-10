#' Format p-values
#'
#' Helper function used to format p-values for reporting. Values smaller
#' than 0.001 are reported as `"p < .001"`, otherwise the value is rounded
#' to the specified number of digits.
#'
#' @param p Numeric p-value.
#' @param digits Integer specifying the number of digits to display.
#'
#' @return A character string containing the formatted p-value.
#'
#' @keywords internal
format_p <- function(p, digits = 3) {
  if (p < 0.001) {
    "p < .001"
  } else {
    paste0("p = ", format(round(p, digits), nsmall = digits))
  }
}
