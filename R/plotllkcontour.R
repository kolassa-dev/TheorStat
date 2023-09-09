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
