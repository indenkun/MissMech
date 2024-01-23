#' K-Sample Anderson Darling Test
#' @rdname AndresonDarling
#' @aliases AndresonDarling
#' 
#' @description
#' This is a non-parametric K-sample test that tests equality of distribution of 
#' a variable between k populations based on samples from each of the populations.
#' 
#' @param data A single vector consisting of concatenation of the k samples data being used for the test.
#' @param number.cases A vector consisting of the number of cases in samples 1, 2, ..., k, respectively.
#' 
#' @details
#' The data is a vector including all the k samples to be used for the test.
#' The j-th element of number.cases is the number of cases in sample j (included in data), for j= 1,...,k.
#' 
#' @returns
#' \item{pn }{The test's p-value.}
#' \item{adk.all }{The Anderson Darling test statistic corresponding to each group.}
#' \item{adl }{The sum of elements of adk.all.}
#' \item{var.sdk }{The variance of the finite sample distribution of the Anderson Darling test statistic under the null.}
#' 
#' @references 
#' Scholz, F.W. and Stephens, M.A. (1987). ''K-Sample Anderson-Darling Tests,''\emph{Journal of the American Statistical Association}, 82, 918-924, \doi{10.2307/2288805}.
#' 
#' @author 
#' Mortaza Jamshidian, Siavash Jalal, and Camden Jansen
#' @note
#' The test does not adjust for tie observations.
#' 
#' @examples
#' #---- Example 1
#' set.seed(50)
#' n1 <- 30
#' n2 <- 45
#' n3 <- 60
#' v1 <- rnorm(n1)
#' v2 <- runif(n2)
#' v3 <- rnorm(n3, 2, 3)
#' AndersonDarling(data = c(v1, v2, v3), number.cases=c(n1, n2, n3))
#' 
#' #---- Example 2
#' set.seed(50)
#' n1 <- 30
#' n2 <- 45
#' n3 <- 60
#' v1 <- rt(n1,4)
#' v2 <- rt(n2,4)
#' v3 <- rt(n3,4)
#' AndersonDarling(data=c(v1, v2, v3), number.cases=c(n1, n2, n3))
#'
#' @export
AndersonDarling <- function(data, number.cases) { 
  # Not adjusted for ties
  # x is data vector
  # ni is number of cases in each group
  x <- data
  ni <- number.cases
  if(length(ni)<2)
  {
    stop("Not enough groups for AndersonDarling test.")
  }
  
  k <- length(ni)
  ni.z <- c(0, cumsum(ni))
  n <- length(x)
  x.sort <- sort(x)
  x.sort <- x.sort[1:(n-1)]
  ind <- which(duplicated(x.sort) == 0)
  counts <- c(ind, length(x.sort) + 1) - c(0, ind)
  hj <- counts[2 : (length(ind) + 1)]
  hn <- cumsum(hj)
  zj <- x.sort[ind]
  adk <- 0
  adk.all <- matrix(0, k, 1) # to keep contribution of kth group
  for(i in 1:k)
  {
    ind <- (ni.z[i] + 1) : ni.z[i + 1]
    templist <- expand.grid(zj, x[ind])
    b <- templist[, 1] == templist[, 2]
    fij <- apply(matrix(b, length(zj)), 1, sum)
    mij <- cumsum(fij)
    num <- (n * mij - ni[i] * hn) ^ 2
    dem <- hn*(n - hn)
    adk.all[i] <- (1 / ni[i] * sum(hj * (num / dem)))
    adk <- adk + adk.all[i]
  }
  adk <- (1 / n) * adk
  adk.all <- adk.all / n
  # Exact sample variance of the k-sample Anderson-Darling
  # Finding Variance of the statistics
  j <- sum(1 / ni)
  i <- seq(1:(n - 1))
  h <- sum(1 / i)
  g <- 0
  for (i in 1:(n - 2)) {
    g <- g + (1 / (n - i)) * sum(1 / seq((i + 1), (n - 1)))
  }
  a <- (4 * g - 6) * (k - 1) + (10 - 6 * g) * j
  b <- (2 * g - 4) * k ^ 2 + 8 * h * k + 
    (2 * g - 14 * h - 4) * j - 8 * h + 4 * g - 6
  c <- (6 * h + 2 * g - 2) * k ^ 2 + (4 * h - 4 * g + 6) * k +
    (2 * h - 6) * j + 4 * h
  d <- (2 * h + 6) * k ^ 2 - 4 * h * k
  var.adk <- ((a * n ^ 3) + (b * n ^ 2) + (c * n) + d) /
    ((n - 1) * (n - 2) * (n - 3))
  if(var.adk<0) var.adk=0
  adk.s <- (adk - (k - 1)) / sqrt(var.adk) 
  # k-sample Anderson-Darling P-value calculation by an extrapolate-interpolate
  # procedure
  a0 <- c(0.25, 0.10, 0.05, 0.025, 0.01)
  b0 <- c(0.675, 1.281, 1.645, 1.96, 2.326)
  b1 <- c(-0.245, 0.25, 0.678 ,1.149, 1.822)
  b2 <- c(-0.105, -0.305, -0.362, -0.391, -0.396)
  c0 <- log((1 - a0) / a0)
  qnt <- b0 + b1 / sqrt(k - 1) + b2 / (k - 1)
  if (adk.s <= qnt[3]) {
    ind <- seq(1:4)
  } else {
    ind <- seq(2:5)
  }
  yy <- stats::spline(qnt[ind], c0[ind], xout = adk.s)$y
  p <- 1 / (1 + exp(yy))
  list(pn = p, adk.all = adk.all, adk = adk, var.sdk = var.adk)
}# end function

