#' Calculate power for exact mean and median tests for normal variates.  
#' Critical values are cacluated via Monte Carlo.
#' @param n number of observations in sample
#' @param nsamp number of MC samples
#' @param nalt number of alternatives to consider, greater than 0 and less
#' than or equal to 1.
#' @export
#' @examples
#' meanmedianpower()
meanmedianpower<-function(n=9,nsamp=5000,nalt=100){
   xx<-array(rnorm(n*nsamp),c(n,nsamp))
   y<-sort(apply(xx,2,mean))
   z<-sort(apply(xx,2,median))
   cy<-y[.95*length(y)]
   cz<-z[.95*length(z)]
   powz<-powy<-rep(NA,nalt) 
   for(i in 1:nalt){
      powy[i]<-sum(y+i/nalt>cy)
      powz[i]<-sum(z+i/nalt>cz)
   }
   powy<-powy/length(y)
   powz<-powz/length(z)
   plot(c(0,1),range(c(powy,powz)),type="n",
     main="Power for mean and median testing",
     sub=paste("Normal sample of size",n,"One sided test of level",0.05),
     xlab="Alternative mean",ylab="Power")
   lines(x=seq(100)/100,y=powy,lty=1)
   lines(x=seq(100)/100,y=powz,lty=2)
   legend(0,.9,lty=1:2,legend=c("Mean","Median"))
}
