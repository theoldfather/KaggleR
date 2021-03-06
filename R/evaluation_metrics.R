## Evaluation Metrics ##

#' Root Mean Squared Log Error
#'
#' @param y.hat Fitted values
#' @param y Realized value
#' @export
rmsle<-function(y.hat,y){
  n<-length(y)
  d<-log(theta+1) - log(y+1)
  sqrt(mean(d*d))
}

#' Mean Squared Error
#'
#' @param y.hat Fitted values
#' @param y Realized value
#' @export
mse<-function(y.hat,y,na.action=na.fail ){
  r<-y.hat-y
  mean(r*r,na.rm=T)
}
