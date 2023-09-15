#' Exact binoial confidence intervals via the F distribution
#' @param x0 Number of successes
#' @param x1 Number of failures
#' @param level Interval level
#' @export
#' @importFrom stats qf
fun.exactbin<-function(x0,x1,level=.95){
   alpha<-1-level
   fv<-if(x1==0) 1 else fv<-qf(alpha/2,2*x0+2,2*x1,
      lower.tail=FALSE)
   fu<-if(x0==0) 1 else fv<-qf(alpha/2,2*x1+2,2*x0,
      lower.tail=FALSE)
   return(c(x0/(x0+(x1+1)*fu),((x0+1)*fv)/(x1+(x0+1)*fv)))
} 
