#' Display the log likelihood function for location parameter for sample of 5 independent Cauchy variates.
#' @export
#' @importFrom stats rt
tllk<-function(){
   rrr<-rt(5,1)
   rrr<-c( -6.4140397,-19.8329737 ,-2.7293667,  2.3439417 ,-0.4798883)
   plot((-40:40)/10,-apply(log(1+outer(rrr,(-40:40)/10,"-")^2),2,sum),
      main="Log likelihood for Cauchy distribution location",xlab="Location Parameter",
      ylab="Log Likelihood",sub="5 observations"
   )
}
