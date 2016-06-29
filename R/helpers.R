
#' \code{adt} Cast as a data.table
#' @param x object to convert to a data.table
#' @export
adt<-function(x) as.data.table(x)

#' \code{adf} Cast as a data.frame
#' @param x object to convert to a data.frame
#' @export
adf<-function(x) as.data.frame(x)

#' \code{am} Cast as a matrix
#' @param x object to convert to a matrix
#' @export
am<-function(x) as.matrix(x)

#' \code{aM} Cast as a Matrix
#' @param x object to convert to a Matrix
#' @export
aM<-function(x){ as.matrix(x) %>% Matrix::Matrix() }

#' \code{aspM} Cast as a sparse Matrix
#' @param x object to convert to a sparse Matrix
#' @export
aspM<-function(x){ as.matrix(x) %>% Matrix::Matrix(sparse=T) }

#' \code{an} Cast as numeric
#' @param x object to convert to numeric
#' @export
an<-function(x) as.numeric(x)

#' \code{ai} Cast as integer
#' @param x object to convert to integer
#' @export
ai<-function(x) as.integer(x)

#' \code{ac} Cast as character
#' @param x object to convert to character
#' @export
ac<-function(x) as.character(x)

#' \code{getSize}
#' @param x the object
#' @export
getSize<-function(x) format(object.size(x),units="GB")

#' \code{sigmoid} Rescales on the sigmoid
#' @param x numeric vector
#' @export
sigmoid<-function(x){ 1/(1+exp(-x)) }

#' NxM matrix of values
#'
#' @param n number of rows
#' @param m number of columns
#' @export
nxm<-function(n,m,value) matrix(rep(value,n*m),nrow=n)

#' NxN matrix of values
#'
#' @param n number of dimensions
#' @export
nxn<-function(n,value) nxm(n,n,value)

#' NxN matrix of 1s
#'
#' @param n number of dimensions
#' @export
ones<-function(n) nxn(n,1)

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

#' Count Missing values
#'
#' @param x values
#' @return returns count of missing values
#' @export
count.na<-function(x){ sum(is.na(x)) }

#' Count Non-missing values
#'
#' @param x values
#' @return return count of non-missing values
#' @export
count.nna<-function(x){ sum(!is.na(x)) }

#' Replace conditional on given function
#' @param x values
#' @param value replacement value
#' @export
fill.f<-function(x,value,f=is.na){
  x[f(x)]<-value
  x
}

#' Replace Na
#' @param x values
#' @param value replacement value
#' @export
fill.na<-function(x,value) fill.f(x,value,is.na)

#' Replace NaN
#' @param x values
#' @param value replacement value
#' @export
fill.nan<-function(x,value) fill.f(x,value,is.nan)

#' Replace Inf
#' @param x values
#' @param value replacement value
#' @export
fill.inf<-function(x,value) fill.f(x,value,is.infinite)




