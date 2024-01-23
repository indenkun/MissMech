#' Removes groups with identical missing data patterns having at most a given number of cases
#' @rdname DelLessData
#' @aliases DelLessData
#' 
#' @description
#' Removes groups of missing data patterns with number of cases 
#' less than or equal to a specified value (ncases).
#' 
#' @param data A matrix consisting of at least two columns. Values must be numerical with missing data indicated by NA.
#' @param ncases Missing data pattern groups with ncases number of cases or less will be removed from the data set.
#' 
#' @returns
#' \item{data }{A matrix of reaaranged data, according to missing data patterns, with missing data patterns having less than ncases number of cases removed.}
#' \item{patused }{A matrix indicating the missing data patterns in the data set; observed variable(s) are indiated by  1's' and missing variables are indicated by NA's.}
#' \item{patcnt}{A vector consisting of the number of cases corresponding to each pattern in patused.}
#' \item{spatcnt}{Cumulative sum of elements of patcnt.}
#' \item{g}{Number of missing data patterns.}
#' \item{caseorder}{A mapping of case number indices from output data (rearranged data) to input data.}
#' \item{removedcases }{The index of cases that were removed from the original data set}
#' 
#' @author 
#' Mortaza Jamshidian, Siavash Jalal, and Camden Jansen
#' 
#' @examples
#' set.seed <- 50
#' n <- 200
#' p <- 4
#' pctmiss <- 0.2
#' y <- matrix(rnorm(n * p),nrow = n)
#' missing <- matrix(runif(n * p), nrow = n) < pctmiss
#' y[missing] <- NA
#' out <- DelLessData(data=y, ncases = 4)
#' dim(y)
#' dim(out$data)
#' 
#' @export
DelLessData <- function(data, ncases = 0) {
  # This function deletes cases of a missing pattern with less than or equal to ncases 
  if(length(data)==0)
  {
    stop("data is empty")
  }
  if (is.matrix(data)) {
    data <- OrderMissing(data)
  }
  n <- nrow(data$data)
  p <- ncol(data$data)
  ind <- which(data$patcnt <= ncases)
  spatcntz <- c(0, data$spatcnt)
  rm <- c()
  removedcases <- c()
  if(length(ind) != 0){
    # cat("cases with insufficient number of observations were removed")
    for(i in 1:length(ind))
    {
      rm <- c(rm, seq(spatcntz[ind[i]] + 1, spatcntz[ind[i] + 1]));
    }
    y <- data$data[-1 * rm, ]
    removedcases <- data$caseorder[rm]
    patused <- data$patused[-1 * ind, ]
    patcnt <- data$patcnt[-1 * ind]
    caseorder <- data$caseorder[-1 * rm]
    spatcnt <- cumsum(patcnt)
  }else {
    patused <- data$patused
    patcnt <- data$patcnt
    spatcnt <- data$spatcnt
    caseorder <- data$caseorder
    y <- data$data
  }
  
  newdata <- list(data = y, patused = patused, patcnt = patcnt,
                  spatcnt = spatcnt, g = length(patcnt), caseorder = caseorder, 
                  removedcases = removedcases)
  # colnames(newdata)<-colnames(data)
  class(newdata) <- "orderpattern"
  newdata
}