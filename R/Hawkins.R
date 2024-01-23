#' Test Statistic for the Hawkins Homoscedasticity Test
#' @rdname Hawkins
#' @aliases Hawkins
#' 
#' @description
#' Produces the F_ij's and A_ij's that are used in the Hawkins' test of homogeneoity of covariances. See Hawkins (1981) and Jamshidian and Jalal (2010) for more details.
#' 
#' @param data A matrix consisting of at least two columns and two rows.
#' @param spatcnt The cumulative sum of the number of cases corresponding to each group.
#' 
#' @returns 
#'  \item{fij }{A vector of F_ij statistics, see Hawkins (1981) reference.}
#'  \item{a }{A list containg A_ij statistics, see Hawkins (1981) reference.}
#'  \item{ni }{A vector consisting of the number of cases in each group.}
#'  
#' @references 
#' Hawkins, D. M. (1981). ``A new test for multivariate normality and homoscedasticity,'' \emph{Technometrics,} 23, 105-110, \doi{10.2307/1267983}.
#'  
#' Jamshidian, M. and Jalal, S. (2010). ``Tests of homoscedasticity, normality, and missing at random for incomplete multivariate data,'' \emph{Psychometrika,} 75, 649-674, \doi{10.1007/s11336-010-9175-3}.
#'  
#' @author 
#' Mortaza Jamshidian, Siavash Jalal, and Camden Jansen
#' @note
#' There must be no rows in data that contain no observations.
#' 
#' @examples
#' set.seed <- 50
#' n <- 200
#' p <- 4
#' pctmiss <- 0.2
#' y <- matrix(rnorm(n * p),nrow = n)
#' spatcnt <- c(20, 50, 70, 200)
#' h <- Hawkins(data=y, spatcnt)
#' @export
Hawkins <- function(data, spatcnt)
{
  # This function performs the Hawkin's method for testing equality of
  # covariances among groups. It is assumed that the data in y is ordered so
  # that the cases from the same group are adjacent.
  #gind is a vector indicating the end of the cases for each group. For
  #example [8 23 30] means that there are three groups with cases 1-8 in one
  #group, 9-23 and 24-30 in another.
  
  # Also in the output, it gives the statistic computed for each group in a
  # cell array, called A. This statistic should be tested for uniformity.
  # also ni(i) gives the number of components of each [a[i]]
  y <- data
  n <- nrow(y)
  p <- ncol(y)
  g <- length(spatcnt)
  spool <- matrix(0, p, p)
  gind <- c(0, spatcnt)
  ygc <- matrix(0, n, p)
  ni <- matrix(0, g, 1)
  for(i in 1:g)
  {
    yg <- y[seq(gind[i] + 1, gind[i + 1]), ]
    ni[i] <- nrow(yg)
    spool <- spool + (ni[i] - 1) * stats::cov(yg)
    ygmean <- apply(yg, 2, mean)
    ygc[seq(gind[i]+1, gind[i + 1]), ] <-
      yg - matrix(ygmean, ni[i], p, byrow = TRUE)
  }
  spool <- spool / (n - g)
  spool <- solve(spool)
  f <- matrix(0, n, 1)
  nu <- n - g - 1
  a <- vector("list",g)
  for(i in 1:g)
  {
    vij <- ygc [seq(gind[i] + 1, gind[i + 1]), ]
    vij <- apply(vij %*% spool * vij, 1, sum)
    vij <-  vij*ni[i]
    f[seq(gind[i] + 1, gind[i + 1])] <- ((n - g - p) * vij)/
      (p * ((ni[i] -1 ) * (n - g) - vij))
    a[[i]] <- 1 - stats::pf(f[seq(gind[i] + 1, gind[i + 1])], p, (nu - p + 1))
  }
  list(fij = f, a = a, ni = ni)
} 
