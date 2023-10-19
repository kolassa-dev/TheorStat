#' Test the noncentral chi-square approximation to Pearson's statistic
#' @param p0 cell probabilities under null
#' @param p1 cell probabilities under alternative
#' @param size number of items classified
#' @param nsamp number of Monte Carlo samples.
#' @export
fun.testncp<-function(p0,p1,size=10,nsamp=100000){
  out<-rep(NA,nsamp)
  expected<-p0*size
  for(j in seq(length(out))){
    x<-rmultinom(1,size,p1)
    out[j]<-sum((x-expected)^2/expected)
# This could be done using the next line, but will
# trigger warnings for cells with small counts.
#   out[j]<-chisq.test(x,p=p0)$statistic
  }
  new<-sort(unique(out))
  etail<-apply(outer(out,new,">="),2,"sum")/length(out)
  plot(range(new),range(etail),type="n",# ecdf(out),
     main="Monte Carlo and Approximate Powers for the One-Sample Pearson Test",
     sub=paste("Null",paste(format(p0,digits=2),collapse=" "),
        "Alternative",paste(format(p1,digits=2),collapse=" "),
        "Sample Size",size))
  lines(new,etail,type="S")
  ncp1<-size*sum((p1-p0)^2/p0)
  x<-(0:100)*max(out)/100
  lines(x,pchisq(x,df=length(p1)-1,ncp=ncp1,lower.tail=FALSE))
}

