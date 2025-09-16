#' My First Function
#'
#' @param x A numeric value.
#'
#' @returns A list of components 'x' and 'y' , where 'y' is square of 'x'
#' @export
#'
#' @examples
#' myfirstfunction(1:10)
myfirstfunction <- function(x) {
  y <- x^2
  plot(y ~ x)
  list(x = x, y = y)

}
