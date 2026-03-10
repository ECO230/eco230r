#' Format Bayes factors
#'
#' Helper function used to format Bayes factors for reporting.
#' Very large or very small Bayes factors are displayed in scientific
#' notation, while moderate values are rounded to a fixed number of digits.
#'
#' @param bf Numeric Bayes factor value (typically BF10 or BF01).
#' @param digits Integer specifying the number of digits to display.
#'
#' @return A character string containing the formatted Bayes factor.
#'
#' @keywords internal
format_bf <- function(bf, digits = 3) {
  if (bf > 1e5 || bf < 1e-3) {
    formatC(bf, format = "e", digits = digits)
  } else {
    format(round(bf, digits), nsmall = digits)
  }
}
