#' Plot the results of simulating polychoric correlation and kappa behavior.
#' @param simout Output from fun.testpoly
#' @export
fun.plottestpoly<-function(simout){
  out<-simout$mat
  par(mfrow=c(2,1))
  db<-density(out[4,])
  plot(db,main="Correlation Estimates from Bivariate Normal",
     xlab="rho",lty=1,col=1,
     sub=paste("True correlation",simout$rho,"Sample size",simout$nsamp))
  legend(quantile(db$x,.01),max(db$y),lty=1:4,col=1:4,
     legend=c("Sample Correlation",paste("Poly.",2:4,"way")))
  for(j in 1:3) lines(density(out[j,]),lty=j+1,col=j+1)
  dbs<-list(density(out[5,]),density(out[6,]),density(out[7,]))
  rx<-range(dbs[[1]]$x)
  ry<-range(c(dbs[[1]]$y,dbs[[2]]$y,dbs[[3]]$y))
  plot(rx,ry,type="n", main="Kappa Estimates from Bivariate Normal",
     xlab="kappa",
     sub=paste("True correlation",simout$rho,"Sample size",simout$nsamp))
  legend(rx[1],ry[2],lty=2:4,col=2:4,
     legend=c("2 way","3 way","4 way"))
  for(j in 1:3) lines(dbs[[j]],lty=j+1,col=j+1)
}
