#' Using simulation, check the operating characteristics of two sample 
#' pooled and unpooled t tests, under various assumptions on the
#' ratio of group standard deviations.
#'
#' @param m number of independent observations in first group.
#' @param n number of independent observations in second group.
#' @param nsamp number of samples.
#' @param nrat number of standard deviation ratios to explore, beginning at 1, ending at 10
#' 
#' @return A list with components
#' out a matrix with nrat rows and two columns giving counts of samples with
#'       type I errors.
#' rat the vector of sd ratios
#' nsamp copy of the input nsamp
#' m copy of the input m
#' n copy of the input n
#' 
#' @examples
#' a<-checkdist(4,4,nsamp=1000)
checkdist<-function(m,n,nsamp=10000,nrat=5){
   out<-array(0,c(nrat,2))
   rats<-1+(seq(nrat)-1)*(10/nrat)
   for(j in 1:nsamp){
      xx<-rnorm(m); yy<-rnorm(n)
      for(j in seq(length(out[,1]))){
         out[j,1]<-out[j,1]+(t.test(xx,yy*rats[j],var.equal=FALSE)$p.value<0.05)
         out[j,2]<-out[j,2]+(t.test(xx,yy*rats[j],var.equal=TRUE)$p.value<0.05)
      }
   }
   return(list(out=out,rat=rats,nsamp=nsamp,m=m,n=n))
}
#' Plot the results of the function checkdist, which determined the true level of
#' pooled and unpooled t tests.
#'
#' @param checkdistout a list with components
#' out a matrix with nrat rows and two columns giving counts of samples with
#'       type I errors.
#' rat the vector of sd ratios
#' nsamp number of samples out of which the matrix out was recorded.
#' m number in first group.
#' n number in second group.
#' @examples
#' plotcheckdist(checkdist(4,4,nsamp=2000,nrat=5))
plotcheckdist<-function(checkdistout){
   plot(range(checkdistout$rat),range(checkdistout$out)/checkdistout$nsamp,type="n",
      main="Dependence of the Two Sample Test on Variance Ratio",
      sub=paste("Sample Levels",checkdistout$m,"and",checkdistout$n),
      xlab="Variance Ratio",ylab="Test Level")
   for(j in 1:2) lines(checkdistout$rat,checkdistout$out[,j]/checkdistout$nsamp,lty=j)
   legend(median(checkdistout$rat),median(checkdistout$out)/checkdistout$nsamp,lty=1:2,
      legend=c("Welch Approximation","Pooled Version"))
}
