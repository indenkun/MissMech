#' Order Missing Data Pattern
#' @rdname OrderMissing
#' @aliases OrderMissing print.orderpattern summary.orderpattern
#' 
#' @description
#' This function rearranges the data based on their missing data patterns. 
#' Morever, missing data patterns consisting of fewer than the user specified number in del.lesscases is deleted from the dataset.
#' 
#' @param data A matrix or data frame consisting of at least two columns. Values must be numerical with missing data indicated by NA.
#' @param del.lesscases Missing data patterns consisting of del.lesscases number of cases or less will be removed from the data set.
#' 
#' @returns 
#' An object of class \code{orderpattern} - a list including elements:
#' \item{data }{A matrix containing the rearranged data set}
#' \item{caseorder}{A mapping of case number indices from output (ordered) data to input data. More specifically, the j-th row of the output (ordered) 
#'   data is the caseorder[j]-th (the j-th element of caseorder) row of the input data.}
#' \item{patused }{A matrix indicating the missing data patterns in the data set, using 1's' (for observed) and NA's (for missing)}
#' \item{patcnt}{A vector consisting the number of cases corresponding to each pattern in patused}
#' \item{spatcnt}{Cumulative sum of elements of patcnt}
#' \item{g}{Number of missing data patterns}
#' \item{removedcases }{The index of cases that were removed from the original data set}
#' 
#' @author 
#' Mortaza Jamshidian, Siavash Jalal, and Camden Jansen
#' 
#' @note
#' If you run the following command line:
#' out <- OrderMissing(data=y, del.lesscases=0),
#' then y[out$caseorder,] is equal to the output data (i.e. out$data). 
#' Also out$data[order(out$caseorder),] is equal to the original data y. Note that
#' if del.lesscases is greater than zero, the command out$data[order(out$caseorder),] will result in
#' a data set that has no case order correspondnce to the original data.
#' 
#' @examples
#' set.seed <- 50
#' n <- 200
#' p <- 4
#' pctmiss <- 0.2
#' y <- matrix(rnorm(n * p),nrow = n)
#' missing <- matrix(runif(n * p), nrow = n) < pctmiss
#' y[missing] <- NA
#' out <- OrderMissing(y, del.lesscases = 0)
#' a <- out$caseorder
#' z = out$data
#' y[a,] # Reverting the original data to the new output order
#' z[order(a),] # Reverting the ordered datat to the original order
#' @export
OrderMissing <- function(data, del.lesscases = 0)
{
  # case order has the order of the original data
  y <- data
  if (is.data.frame(y)) {
    y <- as.matrix(y)
  } 
  if(!is.matrix(y))
  {
    stop("data is not a matrix or data frame")
  }
  if(length(y)==0)
  {
    stop("data is empty")
  }
  names <- colnames(y)
  n <- nrow(y)
  pp <- ncol(y)
  yfinal <- c()
  patused <- c()
  patcnt <- c()
  caseorder <- c()
  removedcases <- c()
  ordertemp <- c(1:n)
  ntemp <- n
  ptemp <- pp
  done <- FALSE
  yatone <- FALSE
  while(!done)
  {
    pattemp <- is.na(y[1, ])
    indin <- c()
    indout <- c()
    done <- TRUE
    for(i in 1:ntemp)
    {
      if(all(is.na(y[i, ]) == pattemp))
      {
        indout <- c(indout, i)
      }  else {
        indin <- c(indin, i)
        done <- FALSE
      }
    }
    if(length(indin) == 1) yatone = TRUE
    yfinal <- rbind(yfinal, y[indout, ])
    y <- y[indin, ]
    caseorder <- c(caseorder, ordertemp[indout])
    ordertemp <- ordertemp[indin]
    patcnt <- c(patcnt, length(indout))
    patused <- rbind(patused, pattemp)
    if(yatone)
    {
      pattemp <- is.na(y)
      yfinal <- rbind(yfinal, matrix(y,ncol=pp))
      y <- c()
      indin <- c()
      indout <- c(1)
      caseorder <- c(caseorder, ordertemp[indout])
      ordertemp <- ordertemp[indin]
      patcnt <- c(patcnt, length(indout))
      patused <- rbind(patused, pattemp)
      done <- TRUE
    }
    
    if(!done) ntemp <- nrow(y)
  }
  #yfinal <- rbind(yfinal, y)
  caseorder <- c(caseorder, ordertemp)
  patused <- ifelse(patused, NA, 1)
  rownames(patused) <- NULL
  colnames(patused) <- names
  spatcnt <- cumsum(patcnt)
  dataorder <- list(data = yfinal, patused = patused, patcnt = patcnt, 
                    spatcnt = spatcnt, g = length(patcnt), 
                    caseorder = caseorder, removedcases = removedcases)
  dataorder$call <- match.call()
  class(dataorder) <- "orderpattern"
  if(del.lesscases > 0)
  {
    dataorder <- DelLessData(dataorder, del.lesscases)
  }   
  dataorder$patused <- matrix(dataorder$patused, ncol = pp)
  colnames(dataorder$patused) <- names
  dataorder
}
#Order <- function(x, ...) UseMethod("Order")
#Order.default <- function(x, ...) {
# temp <- OrderMissing(x)
# temp$call <- match.call()
# class(temp) <- "ordered"
# temp
#}

#' @rdname OrderMissing
#' @aliases OrderMissing print.orderpattern summary.orderpattern
#' @usage NULL
#' @export
print.orderpattern <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nNumber of Ptterns: ", x$g, "\n")
  cat("\nPttern used:\n")
  ni <- x$patcnt
  disp.patt <- cbind(x$patused, ni)
  colnames(disp.patt)[ncol(disp.patt)] <- "Number of cases"
  rownames(disp.patt) <- rownames(disp.patt, do.NULL = FALSE, prefix = "group.")
  print(disp.patt, print.gap = 3) 
}

#' @rdname OrderMissing
#' @aliases OrderMissing print.orderpattern summary.orderpattern
#' @usage NULL
#' @export
summary.orderpattern <- function(object, ...) {
  summary(object$data)
}
