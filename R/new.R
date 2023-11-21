#' Wald test for the differencce in two binomial coefficients.
#' @param y1 Number of successes from group 1
#' @param y2 Number of successes from group 1
#' @param n1 Number of trials from group 1
#' @param n2 Number of trials from group 1
#' @export
#' @importFrom stats glm binomial
fun.wald<- function(y1,y2,n1,n2){
   out<-glm(c(1,1,0,0)~c(-1,1,-1,1),
      weight=c(y1,y2,n1-y1,n2-y2), family=binomial)
   return(summary(out)$coef[2,4])
}
#' All Wald tests for the differencce in two binomial coefficients.
#' @param n1 Number of trials from group 1
#' @param n2 Number of trials from group 1
#' @export
fun.allwald<- function (n1,n2) {
   out<-array(NA,c(n1,n2)+1)
   for(y1 in 0:n1) for(y2 in 0:n2) 
      out[y1+1,y2+1]<-fun.wald(y1,y2,n1,n2)
   return(out)
}
#' Probabilities for pairs of binomial outcomes, on a grid of common success probabilities
#' @param n1 Number of trials from group 1
#' @param n2 Number of trials from group 1
#' @param npts number of grid points
#' @export
fun.probs<- function(n1,n2,npts=100) {
   out<-array(NA,c(npts,n1+1,n2+1))
   pi<-4*((1-npts)/2):((npts-1)/2)/(npts-1)
   pi<-exp(pi)/(1+exp(pi))
   for(y1 in 0:n1) for (y2 in 0:n2) 
      out[,y1+1,y2+1]<-dbinom(y1,n1,pi)*dbinom(y2,n2,pi)
   return(list(pi=pi,prob=out))
}
#' Rejection probabilities for pairs of binomial outcomes, on a grid of common success probabilities
#' @param n1 Number of trials from group 1
#' @param n2 Number of trials from group 1
#' @param npts number of grid points
#' @export
waldsize<-function(n1,n2,npts=100){
   wo<-fun.allwald(n1,n2)<=.05
   out<-rep(NA,npts)
   probs<-fun.probs(n1,n2,npts)
   for(i in seq(npts)) out[i]<-sum(probs$prob[i,,]*wo)
   return(list(pi=probs$pi,size=out))
}
