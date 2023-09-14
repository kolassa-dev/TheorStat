#' Calculate true coverage probabilities for a variety of nominal binomial confidence intervals
#' @param nn number of binomial samples.
#' @param larage range for log of nominal coverage
#' @param npts number of potential nominal coverages to consider
#' @param plotdat optional saved output for replotting.
#' @export
##' @importFrom MultNonParam util.jplot
fun.allcover<-function(nn,larange=c(-6,-1),npts=50000,plotdat=NULL){
   if(is.null(plotdat)){
      plotdat<-list( lalphav=larange[1]+(0:npts)*diff(larange)/npts,
         out=rep(NA,npts+1))
      for(jj in seq(npts+1)){
         plotdat$out[jj]<-fun.coverageplot(nn,alpha=exp(plotdat$lalphav[[jj]]),plot=FALSE)$mincover["Exact"]
      }
      keep<-c(TRUE,diff(plotdat$out)!=0)|c(diff(plotdat$out)!=0,TRUE)
      plotdat$lalphav<-plotdat$lalphav[keep] ; plotdat$out<-plotdat$out[keep]
   }
#  util.jplot(1-exp(plotdat$lalphav),plotdat$out,xlab="Nominal Coverage",
#     main="True Coverage for Various Nominal Targets",
#     ylab="True Coverage",sub=paste("Number of Binomial Trials",nn))
   plot(1-exp(plotdat$lalphav),plotdat$out,xlab="Nominal Coverage",
      type="l",
      main="True Coverage for Various Nominal Targets",
      ylab="True Coverage",sub=paste("Number of Binomial Trials",nn))
   abline(0,1,lty=2)
   return(invisible(plotdat))
}
