#' Bayes factor for chi-square goodness-of-fit
#'
#' Helper function used by `csf()` to compute the Bayes factor for a
#' multinomial goodness-of-fit test.
#'
#' The null model assumes fixed category probabilities (`p0`).
#' The alternative model assumes a Dirichlet prior with concentration
#' parameter `alpha` on the category probabilities.
#'
#' @param counts Numeric vector of observed counts for each category.
#' @param p0 Optional numeric vector of expected probabilities under the
#'   null hypothesis. If `NULL`, a uniform distribution over categories
#'   is assumed.
#' @param alpha Concentration parameter for the Dirichlet prior used under
#'   the alternative hypothesis. Larger values correspond to stronger prior
#'   concentration around equal probabilities. Default is `1`.
#'
#' @return A list containing:
#' \describe{
#'   \item{BF10}{Bayes factor in favor of the alternative hypothesis.}
#'   \item{BF01}{Bayes factor in favor of the null hypothesis.}
#'   \item{log_BF10}{Log Bayes factor in favor of the alternative hypothesis.}
#' }
#'
#' @keywords internal
bf_gof_multinom <- function(counts, p0 = NULL, alpha = 1) {
  counts <- as.numeric(counts)
  k <- length(counts)
  n <- sum(counts)

  if (is.null(p0)) {
    p0 <- rep(1 / k, k)
  }

  if (length(p0) != k) {
    stop("p0 must have same length as counts.")
  }

  if (abs(sum(p0) - 1) > 1e-8) {
    p0 <- p0 / sum(p0)
  }

  # H0: fixed multinomial probabilities
  log_ml_h0 <- sum(counts * log(p0))

  # H1: Dirichlet(alpha,...,alpha) prior on category probabilities
  log_ml_h1 <- lgamma(k * alpha) -
    lgamma(n + k * alpha) +
    sum(lgamma(counts + alpha) - lgamma(alpha))

  log_bf10 <- log_ml_h1 - log_ml_h0
  bf10 <- exp(log_bf10)
  bf01 <- exp(-log_bf10)

  list(
    BF10 = unname(bf10),
    BF01 = unname(bf01),
    log_BF10 = unname(log_bf10)
  )
}
