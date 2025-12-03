#' Function mycltu
#'
#' @param n Integer. Sample size per iteration.
#' @param iter Integer. Number of iterations (replicates)
#' @param a  Numeric. Uniform interval endpoint
#' @param b  Numeric. Uniform interval endpoint
#'
#' @returns (Invisibly) a numeric vector of length \code{iter} containing the

#' @export
mycltu <- function(n, iter, a = 0, b = 10){
  y <- runif(n * iter, a, b)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  w <- apply(data, 2, mean)

  param <- hist(w, plot = FALSE)
  ymax <- 1.1 * max(param$density)

  hist(w, freq = FALSE, ylim = c(0, ymax),
       main = paste("Histogram of sample mean (Uniform)", "\n", "n = ", n, sep = ""),
       xlab = "Sample mean")

  lines(stats::density(w), lwd = 3)
  stats::curve(stats::dnorm(x, mean = (a + b)/2, sd = (b - a)/sqrt(12 * n)),
               add = TRUE, col = "red", lty = 2, lwd = 3)
  stats::curve(stats::dunif(x, a, b), add = TRUE, lwd = 3)
  invisible(w)
}

