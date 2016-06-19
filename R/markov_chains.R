
#' \code{steady_state} Steady State Convergence
#' @param P A transition probability matrix
#' @export
steady_state<-function(P){
  P.<-P
  done<-F
  while(!done){
    .P<-P.%*%P
    if(isTRUE(all.equal(norm(.P-P.,"F"),0))) done <- T
    P.<-.P
  }
  return(.P[1,])
}
