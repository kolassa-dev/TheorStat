#' Construct confidence intervals for upper bound of uniform variables.
#'
#' @param n number of independent observations
#' @param thetamax upper bound for horizontal axis.
#' @param alpha Error for confidence interval; interval has level 1-alpha.
#' @examples
#' uniformciplot()
#' @export
#' @importFrom graphics plot lines axis legend
uniformciplot<-function(n=10,thetamax=10,alpha=0.05){
   n<-10
   theta<-c(0,thetamax)
   l<-(alpha/2)^(1/n)*theta
   u<-(1-alpha/2)^(1/n)*theta
   examplemax<-.5*max(u)+.5*min(u)
   plot(range(theta),range(c(u,l)),type="n",
      main="Confidence Interval Construction for Independent Uniforms [0,theta]",
      sub=paste("Sample size",n,", alpha",alpha),
      xlab="Upper Support Bound for Uniform",
      ylab="Maximum",xaxs="i",yaxs="i")
   lines(x=theta,y=u,lty=2,col=2)
   lines(x=theta,y=l,lty=2,col=2)
   lines(x=theta,y=rep(examplemax,2),lty=1,col=1)
   typicaltheta<-(n+1)/n*examplemax
   atypicaltheta<-examplemax*.01^(-1/n)
   lines(x=rep(typicaltheta,2),y=range(u),lty=3,col=3)
   lines(x=rep(atypicaltheta,2),y=range(u),lty=4,col=4)
   lines(rep(examplemax*.975^(-1/10),2),c(0,examplemax),lty=5,col=5)
   lines(rep(examplemax*.025^(-1/10),2),c(0,examplemax),lty=5,col=5)
   axis(1,c(.025,.975)^(-1/10)*examplemax,labels=c("U","L"))
   legend(0,max(u),lty=c(2,1,3,4,5),col=c(2,1,3,4,5),
      legend=c("0.025, .975 quantiles as a\n funcition of theta",
   "Observed", "Plausible value of theta","Implausible value of theta",
   "CI End Points"))
}
#' Construct confidence intervals for binomial probability.
#'
#' @param n number of independent observations
#' @param alpha Error for confidence interval; interval has level 1-alpha.
#' @param npts number of grid points for cdf inversion.
#' @examples
#' binomialciplot(alpha=0.2)
