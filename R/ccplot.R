#' Draw a plot exhibiting the continuity correction
#' @param nn Binomial sample size
#' @param x number of successes
#' @export
#' @importFrom graphics plot segments lines polygon legend
#' @importFrom stats dnorm
ccplot<-function(nn=10,x=4){
p<-dbinom(0:nn,nn,.5)
hp<-apply(cbind(c(p,0),c(0,p)),1,"max")
plot(c(0,nn+1)-.5,range(p)*c(1,1.2),type="n",
 xlab="Potential Binomial Value",
   ylab="Binomial Probabilities",
   main="Demonstration of Continuity Correction")
segments((0:(nn+1))-.5,rep(0,nn+2),(0:(nn+1))-.5,hp)
segments((0:nn)-.5,p,(0:nn)+.5,p)
xxx<-(-5:(10*nn+5))/10
lines(xxx,dnorm(xxx,.5*nn,sqrt(.5*.5*nn)))
xx<-c(rep(0:x,rep(2,(x+1)))-.5,x,x)
yy<-c(0,rep(p[1:(x+1)],rep(2,(x+1))),0)
polygon(xx,yy,border=FALSE,density=10) 
polygon(c(x,x+.5,x+.5,x),c(p[x+1],p[x+1],0,0),border=FALSE,
  density=10,angle=-45)
legend(-.5,1.2*max(p),density=10,angle=c(45,-45),
   legend=c("Area Approximated without cont. corr.",
    "Additional area represented by cont. corr."))
}
