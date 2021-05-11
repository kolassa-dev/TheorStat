#' Diagram for an example of a Neyman-Pearson test for two simple alternatives.
#' This example illustrates null and alternative distributions on [0,1].  Null 
#' is uniform.  Alternative has density flat on [0,.5) and flat on (.5,1], and
#' then given small slope.
#'
#' @param gamma amount added to density for alternative on (.5,1], and subtracted
#' from density on [0,.5).
#' @param epsilon slope added to alternative hypothesis density
#' @examples
#' pvalpic(.5,.05)
pvalpic<-function(gamma,epsilon){
   plot(c(0,1),c(0,2-gamma+epsilon),type="n",xaxs="i",yaxs="i",
      xlab="x",ylab="Density",
      sub=paste("gamma=",gamma,"epsilon=",epsilon),
      main="Example where smaller p-values do not indicate important evidence gainst null")
   segments(0,1,1,1,lty=1)
   segments(0,gamma-epsilon/4,1/2,gamma+epsilon/4,lty=2)
   segments(1/2,2-gamma-epsilon/4,1,2-gamma+epsilon/4,lty=2)
   legend(.51,.9,legend=c("Null Hypothesis","Alternative Hypothesis"),lty=1:2)
   plot(c(0,1),c(0,1/(gamma-epsilon/2)),type="n",xaxs="i",yaxs="i",
      xlab="x",ylab="Likelihood Ratio",bty="c",
      sub=paste("gamma=",gamma,"epsilon=",epsilon),
      main="Example where smaller p-values do not indicate important evidence gainst null")
   xxx<-(0:50)/100
   lines(x=xxx,y=1/(gamma+epsilon*(xxx-1/4)))
   xxx<-.5+(0:50)/100
   lines(x=xxx,y=1/(2-gamma+epsilon*(xxx-3/4)))
   segments(0,1/gamma,1,0,lty=2)
   axis(4,at=(0:4)/(4*gamma),labels=(0:4)/4)
   legend(.51,.9/(gamma-epsilon/2),lty=1:2,
      legend=c("Likelihood Ratio (left axis)","P-value (right axis)"))
}
