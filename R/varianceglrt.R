#' Diagram the generalized likelihood ratio test for the variance.
#' @export
#' @importFrom graphics plot lines legend
#' @importFrom stats qchisq
#' @examples
#' varianceglrt()
varianceglrt<-function(){
   www<-(0:100)/12.5
   uuu<-www^2
   nv<-c(2,8,20,40)
   aaa<-outer(uuu,nv)
   fun.lr<-function(uuu,nn){exp(-uuu/2)*(uuu^(nn/2))*exp(nn/2)* nn^(-nn/2)}
   for(ii in seq(length(nv))) aaa[,ii]<-fun.lr(uuu,nv[ii])
   plot(x=range(www),y=range(aaa),type="n",xlab="Variance Ratio",
      ylab="Likelihood Ratio",
      sub="Diagonal lines connect critical values for equal-tailed tests",
      main="Generalized LRT for Test of Variance for Normal Deviates")
   for(ii in seq(length(nv))){
      lines(x=www[aaa[,ii]>.001],y=aaa[aaa[,ii]>.001,ii],lty=ii)
      vvv<-qchisq(c(.025,.975),nv[ii]-1)
#     cat(paste(vvv)); cat("\n")
      lines(x=sqrt(vvv),y=fun.lr(vvv,nv[ii]),lty=ii)
   }
   legend(6,2,legend=paste("Sample size",nv),lty=seq(length(nv)))
}
