#' Function exhibiting Newton method for maximizing an exponential log likelihood.
#'
#' @param X exponential observation.
#' @param lambda0 Initial exponential parameter.
#' @examples
#' llkmaximization()
llkmaximization<-function(X=1.5,lambda0=1){
   X<-1.5 ; lambda0<-1
   llk<-function(lambda,X){
      return(list(d0=log(lambda)-lambda*X,d1=1/lambda-X,d2=-1/lambda^2))
   }
   lambdav<-((-100):100)/200+1
   llkout<-llk(lambdav,X)
   nllkout<-llk(lambda0,X)
   plot(lambdav,llkout$d1,type="l",xlab="lambda",xaxt="n",yaxt="n",
      main="Approximation to the Maximum Likelihood Estimate",
      sub=paste("Starting from lambda=",lambda0,".  Data X=",X,
         ".  MLE hat lambda=",1/X),
      ylab="log likelihood derivative")
   axis(2,at=0)
   axis(1,at=c(lambda0,1/X,lambda0-nllkout$d1/nllkout$d2),
      labels=c("lambda0","hat lambda","tilde lambda"))
   abline(h=0)
   abline(a=nllkout$d1-lambda0*nllkout$d2,b=nllkout$d2,lty=2)
   segments(lambda0,nllkout$d1,lambda0,min(llkout$d1),lty=3)
   segments(1/X,min(llkout$d1),1/X,0,lty=3)
   appr<-lambda0-nllkout$d1/nllkout$d2
   segments(appr,min(llkout$d1),appr,0,lty=3)
   legend(lambda0,max(llkout$d1),lty=1:2,
      legend=c("Log likeilhood","Tangent Approximation"))
   legend(lambda0,0,legend=c("lambda 0 Starting value",
      "hat lambda MLE","tilde lambda linear approximation") )
}
#' Plot countours of the log likelihood for a logistic regression
#' with an intercept parameter and one linear parameter.  
#' Examples apply this to data from \insertCite{mehtapatel;textual}{TheorStat}, citing \insertCite{goorinetal87;textual}{TheorStat}.
#'
#' @param dataset data set name
#' @param xvl string with name of covariate in data set.
#' @param yyl string with name of response variable representing counts of 
#'    successes in data set.
#' @param nnl string with name of variable having counts.
#' @examples
#' #Columns in table are:
#' # Lymphocytic Infiltration (1=low, 0=high)
#' # Sex (1=male, 0=female)
#' # Any Ostioid Pathology (1=yes, 0=no)
#' # Number in LI-Sex-AOP group
#' # Number in LI-Sex-AOP group with disease free interval greater than 3 y
#' goorin<-data.frame(LI=c(0,0,0,0,1,1,1,1),Sex=c(0,0,1,1,0,0,1,1),
#'    AOP=c(0,1,0,1,0,1,0,1),N=c(3,2,4,1,5,5,9,17),Y=c(3,2,4,1,5,3,5,6))
#' print(goorin)
#' glm(cbind(Y,N-Y)~Sex,data=goorin,family="binomial")
#' glm(cbind(Y,N-Y)~LI,data=goorin,family="binomial")
#' plotllkcontour(goorin,"LI","Y","N")
#' plotllkcontour(goorin,"AOP","Y","N")
#' @references
#' \insertRef{mehtapatel}{TheorStat}
#'
#' \insertRef{goorinetal87}{TheorStat}
plotllkcontour<-function(dataset,xvl,yyl,nnl){
   xv<-dataset[[xvl]]
   yy<-dataset[[yyl]]
   nn<-dataset[[nnl]]
   X<-cbind(1,xv)
   sst<-as.vector(t(X)%*%yy)
   llk<-function(beta,X,sst,nn){
      return(beta%*%sst-sum(nn*log(1+exp(X%*%beta))))
   }
   beta1<-(-100:100)/10; beta2<-(-100:100)/10
   mat<-array(NA,c(length(beta1),length(beta2)))
   for(ii in seq(length(beta1))) for(jj in seq(length(beta2))){
      mat[ii,jj]<-llk(c(beta1[ii],beta2[jj]),X,sst,nn)
   }
   contour(beta1,beta2,mat,xlab="Intercept",ylab="Slope",
      main="Logistic Regression Log Likelihood Contours",
      sub=paste("Goorin et al. Data Set.  Covariate is",xvl))
}
