long_to_wide <- function(df, row_name, col_name, value_name) {
  wide <- stats::reshape(
    df,
    idvar = row_name,
    timevar = col_name,
    direction = "wide"
  )

  value_prefix <- paste0(value_name, ".")
  names(wide) <- sub(
    paste0("^", value_prefix),
    "",
    names(wide)
  )

  rownames(wide) <- NULL
  wide
}
