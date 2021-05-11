#' Plot log likelihood for normal correlation, known expectations and variances.
#' @param Sxx average of squares of first variable
#' @param Sxy average of product of first and second variables
#' @param Syy average of squares of second variable
#' @param n sample size.
#' @examples
#' corllkp(1.5,.5,1.5,1)
corllkp<-function(Sxx,Sxy,Syy,n){
   rho<-seq(600)/1000
   lp=n*(rho - rho^ 3 + Sxy + rho^2*Sxy - rho*(Sxx + Syy))/(1 - rho^2)^2
   plot(rho,lp,type="l",
      main="Log Likelihood Derivative for Normal Correlation",
      sub=paste("Sxy=",Sxy,",Sxx=1, Syy=",Syy,", n=",n))
   abline(h=0)
   rhotilde<-Sxy/sqrt(Syy*Sxx)
   abline(v=rhotilde,lty=2)
   legend(0,-1,legend=c(paste("Empirical Correlation",round(rhotilde,3))),lty=2)
}
#' Plot of error regions for normal testing.
#' @examples
#' testerrors()
