#' Optimal number of tickets to sell
#'
#' Finds the ticket count n so that the probability of more than N show-ups
#' equals `gamma`, using (1) the exact binomial CDF and (2) a normal approximation.
#' It also draws two plots (objective vs n) and returns a named list.
#'
#' @param N Integer. Number of seats on the plane (> 0).
#' @param gamma Numeric in (0,1). Target probability of overbooking.
#' @param p Numeric in (0,1). Probability a booked passenger shows up.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{nd}{optimal n by the exact (discrete) method}
#'   \item{nc}{optimal n by the normal-approximation method}
#'   \item{N, p, gamma}{the inputs}
#'   \item{prob_over_at_nd}{exact P(X > N) at nd}
#'   \item{prob_over_at_nc}{exact P(X > N) at nc}
#' }
#' @examples
#' \dontrun{
#'   ntickets(N = 400, gamma = 0.02, p = 0.95)
#' }
#' @export
#' @importFrom stats pbinom pnorm
#' @importFrom graphics par plot abline
ntickets <- function(N, gamma, p) {
  stopifnot(N > 0, gamma > 0, gamma < 1, p > 0, p < 1)

  n_vals <- seq(N, N + max(40, ceiling(5 * sqrt(N))), by = 1)

  obj_discrete <- 1 - gamma - stats::pbinom(q = N, size = n_vals, prob = p)

  mu    <- n_vals * p
  sigma <- sqrt(n_vals * p * (1 - p))
  obj_norm <- 1 - gamma - stats::pnorm(q = N + 0.5, mean = mu, sd = sigma)

  nd <- n_vals[which.min(abs(obj_discrete))]
  nc <- n_vals[which.min(abs(obj_norm))]

  prob_over_nd <- 1 - stats::pbinom(q = N, size = nd, prob = p)
  prob_over_nc <- 1 - stats::pbinom(q = N, size = nc, prob = p)

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  graphics::par(mfrow = c(2, 1), mar = c(4.2, 4.2, 3.2, 1))

  graphics::plot(
    n_vals, obj_discrete, type = "b", pch = 19,
    xlab = "n (tickets sold)", ylab = "Objective = 1 - gamma - P(X <= N)",
    main = sprintf("Objective vs n (Discrete Binomial)\nChosen nd = %d,  gamma = %.4f,  N = %d,  p = %.4f",
                   nd, gamma, N, p)
  )
  graphics::abline(h = 0, col = "red", lwd = 2)
  graphics::abline(v = nd, col = "red", lwd = 2)

  graphics::plot(
    n_vals, obj_norm, type = "l",
    xlab = "n (tickets sold)", ylab = "Objective ≈ 1 - gamma - Φ((N+0.5 - np)/sqrt{np(1-p)})",
    main = sprintf("Objective vs n (Normal Approx.)\nChosen nc = %d,  gamma = %.4f,  N = %d,  p = %.4f",
                   nc, gamma, N, p)
  )
  graphics::abline(h = 0, col = "blue", lwd = 2)
  graphics::abline(v = nc, col = "blue", lwd = 2)

  list(
    nd = nd, nc = nc, N = N, p = p, gamma = gamma,
    prob_over_at_nd = prob_over_nd,
    prob_over_at_nc = prob_over_nc
  )
}
