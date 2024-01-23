#' Evaluating Legendre's Polynomials of Degree 1, 2, 3, or 4
#' @rdname LegNorm
#' @aliases LegNorm
#' 
#' @description
#' This function evaluates the values of Legendre polynomials of degrees 1 to 4 on [0,1] at a value(s) x.
#' 
#' @param x A scaler or vector of values at which the Legendre's ploynomials are to be evaluated.
#' 
#' @returns 
#' The returned list has the following elements:
#' 
#' \item{p1}{p1 is value(s) of the Legendre's polynomial of degree 1 at x}
#' 
#' \item{p2}{p2 is value(s) of the Legendre's polynomial of degree 2 at x}
#' 
#' \item{p3}{p3 is value(s) of the Legendre's polynomial of degree 3 at x}
#' 
#' \item{p4}{p4 is value(s) of the Legendre's polynomial of degree 4 at x}
#' 
#' @references
#' David, F. N. (1939). ``On Neyman's "smooth" test for goodness of fit: I. Distribution of the criterion Psi2 when the hypothesis tested is true,'' \emph{Biometrika,} 31, 191-199, \doi{10.1093/biomet/31.1-2.191}.
#' 
#' @author 
#' Mortaza Jamshidian, Siavash Jalal, and Camden Jansen
#' @note
#' Legendre's polynomias on [0,1] are calculated.
#' 
#' @examples
#' p <- LegNorm(c(5.2,11,15))
#' p$p3
#' @export
LegNorm <- function(x)
{
  # This function evaluates Legendre polynomials on [0,1] (note not [-1,1] at
  # a value x, and the polynomial are such that they have norm 1. It only
  # calculates p1,p2,p3, and p4 [Note p0=1, so it is not reurned]
  # x can be a vector or a matrix. 
  # in return, each element of P1, p2, p3, p4 the values of the poly at each
  # component of x
  
  if (!is.matrix(x))
  {
    x <- as.matrix(x)
  }#end if
  x <- 2 * x - 1 # Transforming the legenre's to 0,1, and we will divide by the norm in each case
  p0 <- matrix(1,nrow(x), ncol(x))
  p1 <- x
  p2 <- (3 * x * p1 - p0) / 2
  p3 <- (5 * x * p2 - 2 * p1) / 3
  p4 <- (7 * x * p3 - 3 * p2) / 4
  p1 <- sqrt(3) * p1
  p2 <- sqrt(5) * p2
  p3 <- sqrt(7) * p3
  p4 <- 3 * p4
  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
}
