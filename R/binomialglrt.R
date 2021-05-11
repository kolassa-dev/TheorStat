#' Display the generalized likelihood ratio statistic for a simple null hypothesis about a binomial distribution.
#' @examples
#' binomialglrt()
binomialglrt<-function(){
   fun.lr<-function(q,pi){pi^q*(1-pi)^(1-q)/(q^q*(1-q)^(1-q))}
   v<-array(NA,c(100,3))
   for(ii in 1:3) v[,ii]<-fun.lr(seq(100)/101,ii/4)
   plot(c(0,1),range(v),type="n", xlab="Binomial Proportion",ylab="LRT",
      main="Likelihood Ratio Tests for Various Simple Null Hypotheses",
      sub="Test of a Binomial distribution")
   for(ii in 1:3) lines(seq(100)/101,v[,ii],lty=ii)
   legend(.2,.4,lty=1:3,legend=paste("$\\pi_0=",format((1:3)/4),"$"))
}
