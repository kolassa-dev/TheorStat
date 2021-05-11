#' Construct confidence intervals for binomial probability.
#'
#' @param n number of independent observations
#' @param alpha Error for confidence interval; interval has level 1-alpha.
#' @param npts number of grid points for cdf inversion.
#' @examples
#' binomialciplot(alpha=0.2)
binomialciplot<-function(n=7,alpha=0.05,npts=10000){
   x<-round(n/2)
   pivec<-(0:npts)/npts
   xx<-qbinom(alpha/2,n,pivec)
   jumps<-diff(xx)>.5
   ends<-pivec[c(jumps,T)]
   begins<-pivec[c(T,jumps)]
   eps<-.1
   plot(c(0,1),c(0-eps,n+eps),xaxs="i",yaxs="i",type="n",
      xlab="pi",ylab="Number of successes",
      sub=paste("n=",n,"Confidence level=",1-alpha,"x=",x),
      main="Graphical Construction of Binomial Confidence Interval")
   segments(begins,(0:n)-eps/3,ends,(0:n)-eps/3,lty=2)
   segments(1-begins,(n:0)+eps/3,1-ends,(n:0)+eps/3,lty=2)
   segments(0,x,1,x)
   segments(1-ends[n-x+1],x,1-ends[n-x+1],0,lty=3)
   segments(ends[x+1],x,ends[x+1],0,lty=3)
   legend(0,n,lty=1:3,legend=c( "Observed Value", "Binomial Quantiles", 
   "Confidence Interval End Points"))
}
