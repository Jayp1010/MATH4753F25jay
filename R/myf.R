#'
#' @param iter Number of simulation iterations. Default = 1000.
#' @param n Number of trials per iteration. Default = 10.
#' @param p Success probability. Default = 0.7.
#' @param seed Optional seed for reproducibility.
#' @param plot Logical; if TRUE, makes a barplot of simulated proportions.
#'
#' @return A vector of simulated proportions for 0..n successes.
#' @export
myf <- function(iter = 1000, n = 10, p = 0.7, seed = NULL, plot = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  succ <- integer(iter)
  for (i in seq_len(iter)) {
    succ[i] <- sum(sample(c(1, 0), size = n, replace = TRUE, prob = c(p, 1 - p)))
  }
  tab <- table(factor(succ, levels = 0:n))
  props <- as.numeric(tab) / iter
  names(props) <- 0:n

  if (plot) {
    barplot(props,
            col = rainbow(n + 1),
            xlab = "Number of successes",
            ylab = "Proportion",
            main = paste0("Binomial Simulation (n=", n, ", p=", p, ", iter=", iter, ")"))
  }
 invisible(props)
}
