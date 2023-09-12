#' Give basic normal approximation to the binomial confidence interval
#' @param x number of succsses
#' @param nn number of trials
#' @param alpha 1-coverage probability
#' @param alternative character string indicating alternative
#' @export
#' @importFrom stats qnorm
fun.approxnormalci<-function(x,nn,alpha=.05,
      alternative="two.sided"){
   se<-sqrt(x/nn*(1-x/nn)/nn)
   if(substring(alternative,1,1)=="t"){
      z<-qnorm(1-alpha/2)
      ciends<-c(max(x/nn-z*se,0),min(x/nn+z*se,1))
      sides<-""
   }
   if(substring(alternative,1,1)=="l"){
      z<-qnorm(1-alpha)
      ciends<-c(0,x/nn+z*se)
      sides<-"One-sided"
   }
   if(substring(alternative,1,1)=="g"){
      z<-qnorm(1-alpha)
      ciends<-c(x/nn-z*se,1)
      sides<-"One-sided"
   }
   return(ciends)
}
