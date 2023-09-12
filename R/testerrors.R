#' Plot of error regions for normal testing.
#' @examples
#' testerrors()
#' @export
#' @importFrom graphics plot lines segments polygon legend
testerrors<-function(){
   x<-(-40:60)/10
   plot(x,dnorm(x,-1,1),type="l",ylab="Density",
      main="Error Types in Normal Testing",
      sub="Normal(-1,1) vs. Normal(1,2)")
   lines(x,dnorm(x,1,sqrt(2)),lty=2)
   cv<- -1+1.96
   segments(cv,0,cv,dnorm(cv,-1,1))
   xx<-cv+(0:10)/5
   polygon(c(range(xx),rev(xx)),c(rep(0,2),dnorm(rev(xx),-1,1)),
      angle=45, density=20)
   xx<-cv-(0:10)/2.5
   polygon(c(rev(range(xx)),rev(xx)),c(rep(0,2),dnorm(rev(xx),1,sqrt(2))),
      angle=135, density=20)
   legend(0,.4,legend=paste("Type",c("I","II"),"Error"),density=20,
      angle=c(45,135))
}
