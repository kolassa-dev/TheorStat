#' Draw graphical construction for confidence interval for continuous distribution.
#' @param dist quantile function for construction of confidence interval.
#' @param alpha 1-coverage for confidence interval.
#' @export
#' @examples
#' ctsci()
ctsci<-function(dist=qbeta,alpha=0.05){
   sp<-4
   theta<-(0:150)/5
   ql<-dist(alpha/2,theta,sp);qu<-dist(1-alpha/2,theta,sp)
   distname<-deparse(substitute(dist))
   distname<-substring(distname,2,nchar(distname))
   plot(range(theta),range(c(ql,qu)),type="n",xlab="theta",
      ylab="Observation", main="Continuous Confidence Interval Example",
      sub=paste(distname,"(theta,",sp,") Example",sep=""))
   lines(theta,ql,lty=1); lines(theta,qu,lty=1)
   if(interactive()) menu("0")
   segments( mean(theta),dist(alpha/2,mean(theta),sp),
      mean(theta),dist(1-alpha/2,mean(theta),sp),lty=4)
   if(interactive()) menu("0")
   typical=mean(range(c(ql,qu)))*1.2
   abline(h=typical,lty=2)
   if(interactive()) menu("0")
   ep<-c(approx(ql,theta,typical)$y, approx(qu,theta,typical)$y)
   for(jj in 1:2) segments(ep[jj],typical,ep[jj],0,lty=3)
   legend(mean(theta),.2,lty=1:4,legend=c(".025, .975 quantiles",
      "Typical observed value","Confidence interval endpoints",
      "Observation Probability Interval"))
}
