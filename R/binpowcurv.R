#' Plot power for an exact binomial test.
#' @param m sample size for binomial test.
#' @param pi0 null hypothesis value
#' @param alpha test level.
#' @export
#' @importFrom stats pbinom
#' @importFrom graphics plot lines
#' @examples
#' binpowcurve(10,.5,.05)
binpowcurve<-function(m,pi0,alpha){
   crit<-qbinom(1-alpha,m,pi0)+1
   level<-pbinom(crit-1,m,pi0,lower.tail=FALSE)
   plot(c(.5,1),c(0,1),main=paste("Power Curve for Binomial Testing, m=",m),
      sub=paste("Null pi=",pi0,", Target level=",alpha,", level=",
         round(level,4),", critical value",crit),
      ylab="Power", xlab="Alternate pi",type="n")
   piv<-pi0+((0:99)/100)*(1-pi0)
   pows<-pbinom(crit-1,m,piv,lower.tail=FALSE)
   lines(x=piv,y=pows)
   return(invisible(pows))
}
