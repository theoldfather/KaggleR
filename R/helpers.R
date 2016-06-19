
#' \code{adt} Cast as a data.table
#' @param x object to convert to a data.table
#' @export
adt<-function(x) as.data.table(x)

#' \code{getSize}
#' @param x the object
#' @export
getSize<-function(x) format(object.size(x),units="GB")

#' \code{sigmoid} Rescales on the sigmoid
#' @param x numeric vector
#' @export
sigmoid<-function(x){ 1/(1+exp(-x)) }

#' NxN matrix of 1s
#'
#' @param n number of dimensions
#' @export
ones<-function(n) matrix(rep(1,n*n),nrow=n)

#' NxN matrix of values
#'
#' @param n number of dimensions
#' @export
fill_nxn<-function(n,value) matrix(rep(value,n*n),nrow=n)

#' NxM matrix of values
#'
#' @param n number of rows
#' @param m number of columns
#' @export
fill_nxm<-function(n,m,value) matrix(rep(value,n*m),nrow=n)

#' Coalesce values
#'
#' @param a high priority
#' @param b low priority
#' @return return values according to priority, and NA where both a and b are missing.
#' @export
coalesce<-function(a,b){
  na<-length(a)
  nb<-length(b)
  if(na!=nb & nb>1) stop("a and b must be the same length, or b should be a scalar")
  if(na==nb){
    a[is.na(a)]<-b[is.na(a)]
  }else{
    a[is.na(a)]<-b
  }
  return(a)
}

#' Count NA
#'
#' @param x value
#' @return return values according to priority, and NA where both a and b are missing.
#' @export
count.na<-function(x){
  sum(is.na(x))
}



