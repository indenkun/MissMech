#' Testing Homoscedasticity, Multivariate Normality, and Missing Completely at Random
#' @rdname TestMCARNormality
#' @aliases TestMCARNormality boxplot.testhomosc print.testhomosc summary.testhomosc
#' 
#' @description
#' The main purpose of this package is to test whether the missing data mechanism, for an incompletely observed data set, is one of 
#' missing completely at random (MCAR). As a by product, however, this package has the capabilities of imputing incomplete data, 
#' performing a test to determine whether data have a multivariate normal distribution, performing a test of
#' equality of covariances for groups, and obtaining normal-theory maximum likelihood estimates for mean and covariance when data are
#' incomplete. The test of MCAR follows the methodology proposed by Jamshidian and Jalal (2010). 
#' It is based on testing equality of covariances between groups having identical missing data patterns. The data are imputed, 
#' using two options of normality and distribution free, and the test of equality of covariances between 
#' groups with identical missing data patterns is performed also with options of assuming normality (Hawkins test) or non-parametrically.
#' Users can optionally use their own method of data imputation as well. Multiple imputation is an additional feature of
#' the program that can be used as a diagnostic tool to help identify cases or variables that contribute to rejection of 
#' MCAR, when the MCAR test is rejecetd (See Jamshidian and Jalal, 2010 for details). 
#' As explained in Jamshidian, Jalal, and Jansen (2014), this package can also be used 
#' for imputing missing data, test of multivariate normality, and test of equality of covariances between several 
#' groups when data are completly observed.
#' 
#' @param data A matrix or data frame consisting of at least two columns. Values must be numerical with missing data indicated by NA.
#' @param del.lesscases Missing data patterns consisting of del.lesscases number of cases or less will be removed from the data set.
#' @param imputation.number Number of imputations to be used, if data are to be multiply imputed.
#' @param method 
#' method is an option that allows the user to select one of the methods of Hawkins or nonparametric for the test. If the user is certain that data have multivariate normal distribution, the
#' method="Hawkins" should be selected. On the other hand if data are not normally distributed,
#' then method="Nonparametric" should be used. If the user is unsure, then the default value
#' of method="Auto" will be used, in which case both the Hawkins and the nonparametric tests
#' will be run, and the default output follows the recommendation by Jamshidian and Jalal
#' (2010) outlined in their flowchart given in Figure 7 of their paper.
#' @param imputation.method 
#' "Dist.Free": Missing data are imputed nonparametrically using the method of Sirvastava and Dolatabadi (2009); 
#' also see Jamshidian and Jalal (2010).
#' 
#' "normal": Missing data are imputed assuming that the data come from a multivariate normal distribution. The maximum 
#' likelihood estimate of the mean and covariance obtained from Mls is used for generating imputed values. 
#' The imputed values are based on the conditional distribution of the missing variables given the observed variables; 
#' see Jamshidian and Jalal (2010) for more details.
#' @param nrep
#' Number of replications used to simulate the Neyman distribution to determine the cut off value for the Neyman test 
#' in the program SimNey. Larger values increase the accuracy of the Neyman test.
#' @param n.min The minimum number of cases in a group that triggers the use of asymptotic Chi distribution in place of the emprical distribution in the Neyman test of uniformity.
#' @param seed
#' An initial random number generator seed. The default is 110 that can be reset to a user selected number. If the value is set to NA, a system selected seed is used.
#' @param alpha The significance level at which tests are performed.
#' @param imputed.data The user can optionally provide an imputed data set. In this case the program will not impute the data and will use 
#' the imputed data set for the tests performed. Note that the order of cases in the imputed data set should be the same as 
#' that of the incomplete data set.
#' 
#' @details
#' Theoretical, technical and prcatical details about this program and its uses can be found in Jamshidian and Jalal (2010) 
#' and Jamshidian, Jalal, and Jansen (2014).
#' @returns 
#'   \item{analyzed.data}{The data that were used in the analysis. If del.lesscases=0, this is the same as the orginal data inputted. 
#'     If del.lesscases > 0, then this is the data with cases removed.}
#'   \item{imputed.data }{The analyzed.data after imputation. If imputation.number > 1, the first imputed data set is returned.}
#'   \item{ordered.data }{The analyzed.data ordered according to missing data pattern, usin the function OrderMissing.}
#'   \item{caseorder }{A mapping of case number indices from ordered.data to the original data. More specifically, the j-th row 
#'     of the ordered.data is the caseorder[j]-th (the j-th element of caseorder) row of the original data.}
#'   \item{pnormality }{p-value for the nonparametric test: When imputation.number > 1, this is a vector with each element 
#'     corresponding to each of the imputed data sets.}
#'   \item{adistar }{A matrix consisting of the Anderson-Darling test statistic for each group (columns) and each imputation (rows).}
#'   \item{adstar }{Sum of adistar:  When imputation.number >1, this is a vector with each element corresponding to each 
#'     of the imputed data sets.}
#'   \item{pvalcomb }{p-value for the Hawkins test:  When imputation.number >1, this is a vector with each element 
#'     corresponding to each of the imputed data sets.}
#'   \item{pvalsn }{A matrix consisting of Hawkins test statistics for each group (columns) and each imputation (rows).}
#'   \item{g }{Number of patterns used in the analysis.}
#'   \item{combp }{Hawkins test statistic: When imputation.number > 1, this is a vector with each element corresponding 
#'     to each of the imputed data sets.}
#'   \item{alpha }{The significance level at which the hypothesis tests are performed.}
#'   \item{patcnt}{A vector consisting the number of cases corresponding to each pattern in patused.}
#'   \item{patused }{A matrix indicating the missing data patterns in the data set, using 1 and NA's.}
#'   \item{imputation.number }{A value greater than or equal to 1. If a value larger than 1 is used, data will be 
#'     imputed imputation.number times.}
#'   \item{mu }{The normal-theory maximum likelihood estimate of the variables means.}
#'   \item{sigma }{The normal-theory maximum likelihood estimate of the variables covariance matrix.}
#' @references 
#' Jamshidian, M. and Bentler, P. M. (1999). ``ML estimation of mean and covariance structures with missing data using complete data routines.'' \emph{Journal of Educational and Behavioral Statistics,} {24}, 21-41, \doi{10.2307/1165260}.
#' 
#' Jamshidian, M. and Jalal, S. (2010). ``Tests of homoscedasticity, normality, and missing at random for incomplete multivariate data,'' \emph{Psychometrika,} 75, 649-674, \doi{10.1007/s11336-010-9175-3}.
#' 
#' Jamshidian, M. Jalal, S., and Jansen, C. (2014). ``MissMech: An R Package for Testing Homoscedasticity, Multivariate Normality, and Missing Completely at Random (MCAR),'' \emph{Journal of Statistical Software,} 56(6), 1-31, \doi{10.18637/jss.v056.i06}.
#' 
#' @author 
#' Mortaza Jamshidian, Siavash Jalal, and Camden Jansen
#' 
#' @note
#' Note 1: In the above descriptions "original data" refers to the input data after deletion of the rows consisting of all NA's (if any) 
#' 
#' Note 2: The normal theory maximum likelihood estimate of mean and covariance is obtained using the EM algorithm, as described in Jamshidian
#' and Bentler (1999). The standard errors for these estimates, based on the observed information matrix, can be obtained via the 
#' function Ddf, included in this package. 
#' 
#' @examples
#' #-- Example 1: Data are MCAR and normally distributed
#' \donttest{
#' n <- 300
#' p <- 5
#' pctmiss <- 0.2
#' set.seed(1010)
#' y <- matrix(rnorm(n * p),nrow = n)
#' missing <- matrix(runif(n * p), nrow = n) < pctmiss
#' y[missing] <- NA
#' out <- TestMCARNormality(data=y)
#' print(out)
#' 
#' # --- Prints the p-value for both the Hawkins and the nonparametric test
#' summary(out)
#' 
#' # --- Uses more cases
#' out1 <- TestMCARNormality(data=y, del.lesscases = 1)
#' print(out1)
#' 
#' #---- performs multiple imputation
#' Out <- TestMCARNormality (data = y, imputation.number = 10)
#' summary(Out)
#' boxplot(Out)
#' }
#' #-- Example 2: Data are MCAR and non-normally distributed (t distributed with d.f. = 5)
#' \donttest{
#' n <- 300
#' p <- 5
#' pctmiss <- 0.2
#' set.seed(1010)
#' y <- matrix(rt(n * p, 5), nrow = n)
#' missing <- matrix(runif(n * p), nrow = n) < pctmiss
#' y[missing] <- NA
#' out <- TestMCARNormality(data=y)
#' print(out)
#' 
#' # Perform multiple imputation
#' Out_m <- TestMCARNormality (data = y, imputation.number = 20)
#' boxplot(Out_m)
#' }
#' #-- Example 3: Data are MAR (not MCAR), but are normally distributed
#' \donttest{
#' n <- 300
#' p <- 5
#' r <- 0.3
#' mu <- rep(0, p)
#' sigma <- r * (matrix(1, p, p) - diag(1, p))+ diag(1, p)
#' set.seed(110)
#' eig <- eigen(sigma)
#' sig.sqrt <- eig$vectors %*%  diag(sqrt(eig$values)) %*%  solve(eig$vectors)
#' sig.sqrt <- (sig.sqrt + sig.sqrt) / 2
#' y <- matrix(rnorm(n * p), nrow = n) %*%  sig.sqrt
#' tmp <- y
#' for (j in 2:p){
#'   y[tmp[, j - 1] > 0.8, j] <- NA 
#' }
#' out <- TestMCARNormality(data = y, alpha =0.1)
#' print(out)
#' }
#' #-- Example 4: Multiple imputation; data are MAR (not MCAR), but are normally distributed
#' \donttest{
#' n <- 300
#' p <- 5
#' pctmiss <- 0.2
#' set.seed(1010)
#' y <- matrix (rnorm(n * p), nrow = n)
#' missing <- matrix(runif(n * p), nrow = n) < pctmiss
#' y[missing] <- NA
#' Out <- OrderMissing(y)
#' y <- Out$data
#' spatcnt <- Out$spatcnt
#' g2 <- seq(spatcnt[1] + 1, spatcnt[2])
#' g4 <- seq(spatcnt[3] + 1, spatcnt[4])
#' y[c(g2, g4), ] <- 2 * y[c(g2, g4), ]
#' out <- TestMCARNormality(data = y, imputation.number = 20)
#' print(out)
#' boxplot(out)
#' 
#' # Removing Groups 2 and 4
#' y1= y[-seq(spatcnt[1]+1,spatcnt[2]),]
#' out <- TestMCARNormality(data=y1,imputation.number = 20)
#' print(out)
#' boxplot(out)
#' }
#' #-- Example 5: Test of homoscedasticity for complete data
#' \donttest{
#' n <- 50
#' p <- 5
#' r <- 0.4
#' sigma <- r * (matrix(1, p, p) - diag(1, p)) + diag(1, p)
#' set.seed(1010)
#' eig <- eigen(sigma)
#' sig.sqrt <- eig$vectors %*%  diag(sqrt(eig$values)) %*%  solve(eig$vectors)
#' sig.sqrt <- (sig.sqrt + sig.sqrt) / 2
#' y1 <- matrix(rnorm(n * p), nrow = n) %*%  sig.sqrt
#' n <- 75
#' p <- 5
#' y2 <- matrix(rnorm(n * p), nrow = n)
#' n <- 25
#' p <- 5
#' r <- 0
#' sigma <- r * (matrix(1, p, p) - diag(1, p)) + diag(2, p)
#' y3 <- matrix(rnorm(n * p), nrow = n) %*%  sqrt(sigma)
#' ycomplete <- rbind(y1 ,y2 ,y3)
#' y1 [ ,1] <- NA
#' y2[,c(1 ,3)] <- NA
#' y3 [ ,2] <- NA
#' ygroup <- rbind(y1, y2, y3)
#' out <- TestMCARNormality(data = ygroup, method = "Hawkins", imputed.data = ycomplete)
#' print(out)
#' }
#' # ---- Example 6, real data
#' \donttest{
#' data(agingdata)
#' TestMCARNormality(agingdata, del.lesscases = 1)
#' }
#' @export
TestMCARNormality <- function(data, del.lesscases = 6, imputation.number = 1, method = "Auto", 
                              imputation.method = "Dist.Free", nrep = 10000, n.min = 30, 
                              seed = 110, alpha = 0.05, imputed.data = NA)
{
  if (!is.na(seed))
    set.seed(seed) 
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  } 
  if(!is.na(imputed.data[1]) && imputation.number!=1)
  {
    warning("No multiple imputation allowed when imputed data is provided.")
  }
  if(!is.matrix(data))
  {
    stop("Data is not a matrix or data frame.")
  }
  if(length(data)==0)
  {
    stop("Data is empty.")
  }
  if(ncol(data)<2)
  {
    stop("More than 1 variable is required.")
  }
  allempty <- which(apply(!is.na(data),1,sum) == 0)
  if (length(allempty) != 0) {
    data <- data[apply(!is.na(data), 1, sum) != 0, ]
    warning(length(allempty), " Cases with all variables missing have been removed \n
          from the data.")
  }
  newdata <- OrderMissing(data, del.lesscases)
  if(length(newdata$data)==0)
  {
    stop("There are no data sets after deleting insufficient cases.")
  }
  
  if(newdata$g == 1)
  {
    stop("More than one missing data pattern should be present.")
  }
  if(sum(newdata$patcnt==1) > 0)
  {
    stop("At least 2 cases needed in each missing data patterns.")
  }
  
  y <- newdata$data
  patused <- newdata$patused
  patcnt <- newdata$patcnt
  spatcnt <- newdata$spatcnt
  caseorder <- newdata$caseorder
  removedcases <- newdata$removedcases
  n <- nrow(y)
  p <- ncol(y)
  g <- newdata$g
  spatcntz <- c(0, spatcnt)
  pvalsn <- matrix(0, imputation.number, g)
  adistar <- matrix(0, imputation.number, g)
  pnormality <- c()
  x <- vector("list", g)
  n4sim <- vector("list",g)
  #------------------------------imputation-----------------------
  mu <- matrix(0, p, 1)
  sig <- diag(1, p)
  
  emest <- Mls(newdata, mu, sig, 1e-6)
  mu <- emest$mu
  sig <- emest$sig
  if(is.na(imputed.data[1]))
  {
    yimp <- y
    if (imputation.method == "Dist.Free") {
      iscomp <- apply(patused, 1, sum, na.rm = TRUE) == p
      
      cind <- which(iscomp)
      ncomp <- patcnt[cind]
      if (length(ncomp) == 0) ncomp <- 0
      use.normal <- FALSE
      if (ncomp >= 10 && ncomp>=2*p){
        compy <- y[seq(spatcntz[cind] + 1, spatcntz[cind + 1]), ]
        ybar <- matrix(apply(compy, 2, mean))
        sbar <- stats::cov(compy)
        resid <- (ncomp / (ncomp - 1)) ^ .5 * 
          (compy - matrix(ybar, ncomp, p, byrow = TRUE))
      } else {
        warning("There is not sufficient number of complete cases.\n  Dist.Free imputation requires a least 10 complete cases\n  or 2*number of variables, whichever is bigger.\n  imputation.method = normal will be used instead.\n")
        use.normal <- TRUE
      }
    }
    for(k in 1:imputation.number)
    {
      #-----------------normal imputation--------------------------------
      if (imputation.method == "Normal" || use.normal){
        yimp <- Impute(data = y, mu, sig, imputation.method = "Normal")
        yimp <- yimp$yimpOrdered
      }
      #-----------------distribution free imputation---------------------------------
      if (imputation.method == "Dist.Free" && !use.normal){
        yimp <- Impute(data = y, ybar, sbar, imputation.method = "Dist.Free", resid)
        yimp <- yimp$yimpOrdered
      }
      if (k == 1) yimptemp <- yimp
      #--------------Hawkin's test on the completed data------------------
      templist <- Hawkins(yimp,spatcnt)
      fij <- templist$fij
      tail <- templist$a
      ni <- templist$ni
      if (method == "Auto" || method == "Hawkins") {
        #Neyman test of uniformity for each group
        for(i in 1:g)
        {
          if (ni[i] < n.min){
            if (k == 1) {
              n4sim[[i]] <- SimNey(ni[i], nrep)
            }
          }
          templist <- TestUNey(tail[[i]], nrep, sim = n4sim[[i]], n.min)
          pn <- templist$pn
          n4 <- templist$n4
          pn <- pn + (pn == 0) / nrep
          pvalsn[k,i] <- pn
        }
      }
      #--------------Anderson darling test for equality of distribution
      if (method == "Auto" || method == "Nonparametric") {
        if(length(ni)<2)
        {
          stop("Not enough groups for AndersonDarling test.")
        }
        templist <- AndersonDarling(fij, ni)
        p.ad <- templist$pn
        adistar[k, ] <- templist$adk.all
        pnormality <- c(pnormality, p.ad)
      }
    }
  } else {
    yimp <- imputed.data[caseorder, ]
    yimptemp <- yimp
    templist <- Hawkins(yimp,spatcnt)
    fij <- templist$fij
    tail <- templist$a
    ni <- templist$ni
    if (method == "Auto" || method == "Hawkins") {
      #Neyman test of uniformity for each group
      for(i in 1:g)
      {
        if (ni[i] < n.min){
          n4sim[[i]] <- SimNey(ni[i], nrep)
        }
        templist <- TestUNey(tail[[i]], nrep, sim = n4sim[[i]], n.min)
        pn <- templist$pn
        n4 <- templist$n4
        pn <- pn + (pn == 0) / nrep
        pvalsn[1, i] <- pn
      }
    }
    if (method == "Auto" || method == "Nonparametric") {
      #--------------Anderson darling test for equality of distribution
      templist <- AndersonDarling(fij, ni)
      p.ad <- templist$pn
      adistar[1, ] <- templist$adk.all
      pnormality <- c(pnormality, p.ad)
    }
  }
  adstar <- apply(adistar,1,sum)
  #combine p-values of test of uniformity
  combp <- -2 * apply(log(pvalsn), 1, sum)
  pvalcomb <- stats::pchisq(combp, 2*g, lower.tail = FALSE)
  if (method == "Hawkins") {
    pnormality <- NULL
    adstar <- NULL
    adistar <- NULL
  }
  if (method == "Nonparametric") {
    pvalcomb = NULL
    combp = NULL
    pvalsn = NULL
  }
  yimptemp <- yimptemp[order(caseorder), ]
  if (length(removedcases) == 0) {
    dataused <- data
  }else {dataused <- data[-1 * removedcases, ]}
  homoscedastic <- list(analyzed.data = dataused, imputed.data = yimptemp,
                        ordered.data =  y, caseorder = caseorder,
                        pnormality = pnormality, adstar = adstar, adistar = adistar,  
                        pvalcomb = pvalcomb, combp = combp, pvalsn = pvalsn, g = g, alpha = alpha,
                        patused = patused, patcnt = patcnt, imputation.number = imputation.number, mu = mu, sigma = sig)
  homoscedastic$call <- match.call()
  class(homoscedastic) <- "testhomosc"
  homoscedastic
}
#---------------------------------------------------------------------
#testmcar <- function(x, ...) UseImputationMethod("testmcar")
#testmcar.default <- function(data, ncases = 6, imputation.number = 10,
#                             imputation.method = "Normal", nrep = 10000)
#{
#test <- TestMCARNormality(data, ncases = 6, imputation.number = 10,
#                         imputation.method = "Normal", nrep = 10000)
#test$call <- match.call()
#class(test) <- "testmcar"
#test
#}
#---------------------------------------------------------------------
# printing format for the class "testhomosc"

#' @rdname TestMCARNormality
#' @aliases TestMCARNormality boxplot.testhomosc print.testhomosc summary.testhomosc
#' @usage NULL
#' @export
print.testhomosc <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  #cat("\nNumber of imputation:\n")
  #print(x$imputation.number)
  ni <- x$patcnt
  cat("\nNumber of Patterns: ", x$g,"\n\nTotal number of cases used in the analysis: ", sum(ni),"\n")
  cat("\n Pattern(s) used:\n")
  alpha <- x$alpha 
  disp.patt <- cbind(x$patused, ni)
  colnames(disp.patt)[ncol(disp.patt)] <- "Number of cases"
  rownames(disp.patt) <- rownames(disp.patt, do.NULL = FALSE, prefix = "group.")
  print(disp.patt, print.gap = 3) 
  method <- "Auto"
  if (is.null(x$pnormality)) method <- "Hawkins"
  if (is.null(x$pvalcomb)) method <- "Nonparametric"
  cat("\n\n    Test of normality and Homoscedasticity:\n  -------------------------------------------\n")
  if (method == "Auto") {
    cat("\nHawkins Test:\n")
    cat("\n    P-value for the Hawkins test of normality and homoscedasticity: ", x$pvalcomb[1],"\n")
    if (x$pvalcomb[1] > alpha){
      cat("\n    There is not sufficient evidence to reject normality
    or MCAR at", alpha,"significance level\n")
    }else {
      cat("\n    Either the test of multivariate normality or homoscedasticity (or both) is rejected.\n    Provided that normality can be assumed, the hypothesis of MCAR is 
    rejected at",alpha,"significance level. \n")
      cat("\nNon-Parametric Test:\n")
      cat("\n    P-value for the non-parametric test of homoscedasticity: ", x$pnormality[1],"\n")
      if (x$pnormality[1] > alpha){
        cat("\n    Reject Normality at",alpha,"significance level.
    There is not sufficient evidence to reject MCAR at",alpha,"significance level.\n")
      }else {
        cat("\n    Hypothesis of MCAR is rejected at ",alpha,"significance level.
    The multivariate normality test is inconclusive. \n")
      }
    }
  }
  if (method == "Hawkins"){
    cat("\nHawkins Test:\n")
    cat("\n    P-value for the Hawkins test of normality and homoscedasticity: ", x$pvalcomb[1],"\n")
  }
  if (method == "Nonparametric"){
    cat("\nNon-Parametric Test:\n")
    cat("\n    P-value for the non-parametric test of homoscedasticity: ", x$pnormality[1],"\n")
  }
}
#----------------------------------------------------------------------------
#' @rdname TestMCARNormality
#' @aliases TestMCARNormality boxplot.testhomosc print.testhomosc summary.testhomosc
#' @usage NULL
#' @export
summary.testhomosc <- function(object, ...) {
  ni <- object$patcnt
  cat("\nNumber of imputation: ", object$imputation.number,"\n")
  cat("\nNumber of Patterns: ", object$g,"\n\nTotal number of cases used in the analysis: ", sum(ni),"\n")
  cat("\n Pattern(s) used:\n")
  alpha <- object$alpha 
  disp.patt <- cbind(object$patused, ni)
  colnames(disp.patt)[ncol(disp.patt)] <- "Number of cases"
  rownames(disp.patt) <- rownames(disp.patt, do.NULL = FALSE, prefix = "group.")
  print(disp.patt, print.gap = 3) 
  method <- "Auto"
  if (is.null(object$pnormality)) method <- "Hawkins"
  if (is.null(object$pvalcomb)) method <- "Nonparametric"
  cat("\n\n    Test of normality and Homoscedasticity:\n  -------------------------------------------\n")
  if (method == "Auto") {
    cat("\nHawkins Test:\n")
    cat("\n    P-value for the Hawkins test of normality and homoscedasticity: ", object$pvalcomb[1],"\n")
    cat("\nNon-Parametric Test:\n")
    cat("\n    P-value for the non-parametric test of homoscedasticity: ", object$pnormality[1],"\n")
  }
  if (method == "Hawkins"){
    cat("\nHawkins Test:\n")
    cat("\n    P-value for the Hawkins test of normality and homoscedasticity: ", object$pvalcomb[1],"\n")
  }
  if (method == "Nonparametric"){
    cat("\nNon-Parametric Test:\n")
    cat("\n    P-value for the non-parametric test of homoscedasticity: ", object$pnormality[1],"\n")
  }
}
#-----------------------------------------------------------------------------
# Plot "testhomosc"
#' @rdname TestMCARNormality
#' @aliases TestMCARNormality boxplot.testhomosc print.testhomosc summary.testhomosc
#' @usage NULL
#' @importFrom graphics boxplot
#' @export
boxplot.testhomosc <- function(x, ...) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar)) 
  if (is.null(x$pnormality)) {
    graphics::par(bg = "cornsilk")
    graphics::boxplot(x$pvalsn, col="lightcyan", border = "blue", medlwd = .5, medcol = "red")
    graphics::title(main = "Boxplots of p-values corresponding to each set of the missing data patterns\n for the Neyman test of Uniformity",
          xlab = "Missing data pattern group", ylab = "P-value", font.main = 4, 
          col.main = "blue4", cex.main = 1, font.lab = 4, cex.lab = 0.8, 
          col.lab = "blue4")
    graphics::abline(h = x$alpha / x$g, col = "red", lty = 2)
  }
  if (is.null(x$pvalcomb)) {
    graphics::par(bg = "cornsilk")
    graphics::boxplot(x$adistar, col="lightcyan", border = "blue", medlwd = .5, medcol = "red")
    graphics::title(main = "Boxplots of the T-value test statistics corresponding to each set of missing\n data patterns for the non-parametric test",
          xlab = "Missing data pattern group", ylab = expression(T[i]), 
          font.main = 4, col.main = "blue4", cex.main = 1, font.lab = 4, 
          cex.lab = 0.8, col.lab = "blue4")
  }
  if (!is.null(x$pvalcomb) && !is.null(x$pnormality)) {
    graphics::par(mfrow=c(2,1), bg = "cornsilk")
    graphics::boxplot(x$pvalsn, col="lightcyan", border = "blue", medlwd = .5, medcol = "red")
    graphics::title(main = "Boxplots of p-values corresponding to each set of the missing data patterns\n for the Neyman test of Uniformity",
          xlab = "Missing data pattern group", ylab = "P-value", font.main = 4, 
          col.main = "blue4", cex.main = 1, font.lab = 4, cex.lab = 0.8, 
          col.lab = "blue4")
    graphics::abline(h = x$alpha / x$g, col = "red", lty = 2)
    graphics::boxplot(x$adistar, col="lightcyan", border = "blue", medlwd = .5, medcol = "red")
    graphics::title(main = "Boxplots of the T-value test statistics corresponding to each set of missing\n data patterns for the non-parametric test",
          xlab = "Missing data pattern group", ylab = expression(T[i]), 
          font.main = 4, col.main = "blue4", cex.main = 1, font.lab = 4, 
          cex.lab = 0.8, col.lab = "blue4")
  }
}