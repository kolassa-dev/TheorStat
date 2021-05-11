#' Calculate the bias incurred by when using the sample standard deviation to 
#' estimate the population value.
biasofsd<-function(){
   plot(2:100,sqrt(2)*gamma((2:100)/2)/(gamma((1:99)/2)*sqrt((1:99)))-1,
       main="Bias in estimation of popualtion standard deviation by sample standard deviation",
       xlab="Sample Size",ylab="Bias",
       sub="Calculations for n-1 in denominator with normal observations")
}
