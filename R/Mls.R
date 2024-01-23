#' ML Estimates of Mean and Covariance Based on Incomplete Data
#' @rdname Mls
#' @aliases Mls Sexpect
#' 
#' @description
#' Normal theory maximum likelihood estimates of mean and covariance matrix is obtained 
#' when data are incomplete, using EM algorithm (see Jamshidian and Bentler 1999). 
#' If the option Hessian is set to TRUE, then the observed information containing 
#' the standard errors of the parameter estimates is also computed. 
#' 
#' @param data A matrix consisting of at least two columns. Values must be numerical with missing data indicated by NA.
#' @param mu EM iteration initial value for mu (the mean vector). The default is a zero vector.
#' @param sig EM iteration initial value for sigma (the covariance matrix). The default is the identity matrix.
#' @param tol The tolerance value used in the convergence criteria described in Jamshidian and Bentler (1999) for stopping the EM algorithm.
#' @param Hessian Hessian of the log-likelihood function, see Jamshidian and Bentler (1999).
#' 
#' @returns
#'  \item{mu }{The maximum likelihood estimate of the mean vector.}
#'  \item{sig }{The maximum likelihood estimates of the covariance matrix.}
#'  \item{hessian}{The Hessian of the observed data log-likelihood function.}
#'  \item{stderror}{The negative of the inverse of the Hessian of the observed data log-likelihood function. The diagonal elements of stderror are the variance of the parameters based on the observed informaion matrix.}
#'  \item{iteration}{The number of iterations used in the EM iterative process.}
#'  
#' @references 
#'  Jamshidian, M. and Bentler, P. M. (1999). ``ML estimation of mean and covariance structures with missing data using complete data routines.'' \emph{Journal of Educational and Behavioral Statistics,} 24, 21-41, \doi{10.2307/1165260}.
#'
#' @author 
#' Mortaza Jamshidian, Siavash Jalal, and Camden Jansen
#' 
#' @examples
#' set.seed <- 50
#' n <- 200
#' p <- 4
#' pctmiss <- 0.2 # Generate 20\% missing data
#' y <- matrix(rnorm(n * p),nrow = n)
#' missing <- matrix(runif(n * p), nrow = n) < pctmiss
#' y[missing] <- NA
#' ml <- Mls(data=y, mu = NA, sig = NA, tol = 1e-6, Hessian=FALSE)
#' ml
#' 
#' @export
Mls  <- function(data, mu = NA, sig = NA, tol = 1e-6, Hessian = FALSE)
{
  # mu is estimate of the mean
  # sig is estimate of the covariance
  if (!is.matrix(data) && !inherits(data, "orderpattern")) {
    stop("Warning: data must have the classes of matrix or orderpattern.")
  }
  if (is.matrix(data)) {
    allempty <- which(apply(!is.na(data),1,sum) == 0)
    if (length(allempty) != 0) {
      data <- data[apply(!is.na(data), 1, sum) != 0, ]
      warning(length(allempty), " Cases with all variables missing have been removed
         from the data.")
    }
    data <- OrderMissing(data)
  }
  if (inherits(data, "orderpattern")) {
    allempty <- which(apply(!is.na(data$data),1,sum) == 0)
    if (length(allempty) != 0) {
      data <- data$data
      data <- data[apply(!is.na(data), 1, sum) != 0, ]
      warning(length(allempty), " Cases with all variables missing have been removed
         from the data.")
      data <- OrderMissing(data)
    }
  }
  if (length(data$data)==0)
  {
    stop("Data is empty")
  }
  if(ncol(data$data)<2)
  {
    stop("More than 1 variable is required.")
  }
  y <- data$data
  patused <- data$patused
  spatcnt <- data$spatcnt
  if (is.na(mu[1])){
    mu <- matrix(0, ncol(y), 1)
    sig <- diag(1, ncol(y))
  }
  itcnt <- 0
  em <- 0
  repeat {
    emtemp <- Sexpect(y, mu, sig, patused, spatcnt)
    ysbar <- emtemp$ysbar
    sstar <- emtemp$sstar
    em <- max(abs(sstar - mu %*% t(mu) - sig), abs(mu - ysbar))
    mu <- ysbar
    sig <- sstar - mu %*% t(mu)
    itcnt <- itcnt + 1
    if(!(em > tol || itcnt < 2)) break()
  }# end repeat
  rownames(mu) <- colnames(y)
  colnames(sig) <- colnames(y)
  if(Hessian)
  {
    templist <- Ddf(y,mu,sig)
    hessian <- templist$dd
    stderror <- templist$se
    return (list(mu = mu, sig = sig, hessian = hessian, stderror = stderror, iteration = itcnt))
  }
  return(list(mu = mu, sig = sig, iteration = itcnt))
}
#------------------------------------------------------------------

#' @rdname Mls
#' @aliases Mls Sexpect
#' @usage NULL
#' @export
Sexpect <- function(y, mu, sig, patused, spatcnt)
{
  
  n <-  nrow(y)
  pp <- ncol(y)
  sstar <- matrix(0, pp, pp)
  a <- nrow(mu)
  b <- ncol(mu)
  ysbar <- matrix(0, a, b)
  first <- 1
  for (i in 1:length(spatcnt)) {
    ni <- spatcnt[i] - first + 1 
    stemp <- matrix(0, pp, pp)
    indm <- which(is.na(patused[i, ]))
    indo <- which(!is.na(patused[i, ]))
    yo <- matrix(y[first:spatcnt[i], indo], ni, length(indo))
    first <- spatcnt[i] + 1
    muo <- mu[indo]
    mum <- mu[indm]
    sigoo <- sig[indo, indo]
    sigooi <- solve(sigoo)
    soo <- t(yo) %*% yo
    stemp[indo, indo] <- soo
    ystemp <- matrix(0, ni, pp)
    ystemp[, indo] <- yo
    if (length(indm)!= 0) {
      sigmo <- matrix(sig[indm, indo], length(indm), length(indo))
      sigmm <- sig[indm, indm]
      temp1 <- matrix(mum, ni, length(indm), byrow = TRUE)
      temp2 <- yo - matrix(muo, ni, length(indo), byrow = TRUE)
      ym <- temp1 + temp2 %*% sigooi %*% t(sigmo)
      som <- t(yo) %*% ym
      smm <- ni * (sigmm - sigmo %*% sigooi %*% t(sigmo))+ t(ym)%*%ym
      stemp[indo, indm] <- som
      stemp[indm, indo] <- t(som)
      stemp[indm, indm] <- smm
      ystemp[, indm] <- ym
    }# end if 
    sstar <- sstar + stemp;
    if (ni == 1){
      ysbar <- t(ystemp) + ysbar
    }else { 
      ysbar <- apply(ystemp, 2, sum) + ysbar
    }
  }# end for
  ysbar <- (1 / n) * ysbar
  sstar <- (1 / n) * sstar
  sstar <- (sstar + t(sstar))/2
  
  return(list(ysbar = ysbar, sstar = sstar))
}
