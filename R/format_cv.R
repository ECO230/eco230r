#' Format Cramer's V
#'
#' Helper function used to format Cramer's V effect size values for
#' reporting. Values smaller than 0.001 are reported as `"V < .001"`,
#' otherwise the value is rounded to the specified number of digits.
#'
#' @param v Numeric value of Cramer's V.
#' @param digits Integer specifying the number of digits to display.
#'
#' @return A character string containing the formatted effect size.
#'
#' @keywords internal
format_cv <- function(v, digits = 3) {
  if (v < 0.001) {
    "V < .001"
  } else {
    paste0("V = ", format(round(v, digits), nsmall = digits))
  }
}
