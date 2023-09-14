#' Graphically display construction of Wilson confidence intervals for
#' binomial variables.
#' @param nn binomial sample size
#' @param alpha 1-coverage
#' @param pinull Potential null hypothesis for parameter
#' @param npts Number of points in elipse
#' @param piobs Observed proportion value.
#' @export
#' @importFrom graphics plot lines segments legend
wilsonpicture<-function(nn=10,alpha=0.05,pinull=.6,npts=100,piobs=.5){
   pivec<-(0:npts)/npts
   ll<-pivec+qnorm(alpha/2)*sqrt(pivec*(1-pivec)/nn)
   uu<-pivec-qnorm(alpha/2)*sqrt(pivec*(1-pivec)/nn)
   llc<-ll-1/(2*nn)
   uuc<-uu+1/(2*nn)
   plot(c(0,1),range(c(llc,uuc)),type="n",
      main="Wilson Binomial CI",xlab="Probability",
      ylab="Sample Proportion",sub=paste("n=",nn))
   lines(pivec,uu,lty=1); lines(pivec,ll,lty=1)
   lines(pivec,uuc,lty=2); lines(pivec,llc,lty=2)
   whichpi<-ceiling(pinull*(npts+1))
   segments(pinull,llc[whichpi],pinull,uuc[whichpi],lty=3)
   ciendi<-c(max(seq(length(uuc))[uuc<=piobs]),
     min(seq(length(llc))[llc>=piobs]))
   segments(pivec[ciendi[1]],piobs,pivec[ciendi[2]],piobs,lty=4)
   legend(pinull,llc[whichpi]-0.005,legend=c("Unadjusted Critical Values",
      "Continuity-Corrected\n Critical Values",
      "Values for which Null\n Not Rejected",
      "Confidence Interval"))
}
