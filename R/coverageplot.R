#' Calculate, and plot, coverage for binomial confidence intervals.
#' @param nn number of binomial samples
#' @param alpha 1-coverage
#' @param alternative string incidating whether one- or two sided intervals are calculated
#' @param plot logical flag indicating whether to plot.
#' @param cm minimal value on vertical axis.
#' @importFrom graphics abline plot legend
#' @importFrom stats binom.test dbinom
#' @importFrom Hmisc binconf
#' @export
fun.coverageplot<-function(nn,alpha=.05,exactonly=FALSE,
      alternative="two.sided",plot=TRUE,cm=0){
   pi<-(1:999)/1000
   if(exactonly){
      ciends<-array(NA,c(2,1,nn+1))
      dimnames(ciends)<-list(c("Lower","Upper"),
         "Exact",as.character(0:nn))
   }else{
      ciends<-array(NA,c(2,3,nn+1))
      dimnames(ciends)<-list(c("Lower","Upper"),
         c("Exact","Normal","Wilson"),as.character(0:nn))
   }
   cover<-array(NA,c(length(pi),dim(ciends)[2]))
   dimnames(cover)<-list(NULL,dimnames(ciends)[[2]])
   if(alternative=="both"){
      newalpha<-alpha/2
      alternatives<-c("greater","less") 
      yaxtt<-"n"
      maxx<-1+(1-cm)
   }else{ 
      newalpha<-alpha
      alternatives<-alternative
      yaxtt<-"s"
      maxx<-1
   }
   if(plot) plot(c(0,1),c(cm,maxx),type="n", yaxt=yaxtt,xaxs="i",yaxs="i",
         xlab="True Probability",ylab="Coverage",
         main=paste("Coverage for",1-alpha,
            if(substring(alternative,1,1)=="t") "" else 
              "one-sided", "Confidence Interval"),
         sub=paste("Sample Size",nn))
   count<-0
   for(alt in alternatives){
      if(count==1) abline(h=1)
      for(x in 0:nn){
         ciends[,1,x+1]<-binom.test(x,nn,
            alternative=alt,conf.level=1-newalpha)$conf.int
         if(!exactonly){
            ciends[,2,x+1]<-fun.approxnormalci(x,nn,alpha=newalpha,
               alternative=alt)
# Binconf returns a 3-component vector, with the first component the 
# estimate.  We don't need the estimate.
            if(alt=="two.sided"){
               ciends[,3,x+1]<-binconf(x,nn,alpha=newalpha,method="wilson")[2:3]
            }
            if(alt=="greater"){
               ciends[,3,x+1]<-c(binconf(x,nn,alpha=newalpha,method="wilson")[2],1)
            }
            if(alt=="less"){
               ciends[,3,x+1]<-c(0,binconf(x,nn,alpha=newalpha,method="wilson")[3])
            }
         }
      }
      for(jj in seq(length(pi))){
         ps<-dbinom(0:nn,nn,pi[jj])
         hit<-array(FALSE,c(nn+1,dim(ciends)[2]))
         for(x in 0:nn){
            for(ii in seq(dim(ciends)[2])) if((ciends[1,ii,x+1]<=pi[jj])&(
               ciends[2,ii,x+1]>=pi[jj])) hit[x+1,ii]<-TRUE
         }
         for(ii in seq(dim(ciends)[2])) cover[jj,ii]<-sum(ps*hit[,ii])
      }
      if(plot){
         for(ii in seq(dim(ciends)[2])){
            use<-cover[,ii]>=cm
            lines(pi[use],count+cover[use,ii]-cm*count,lty=ii)
         }
         abline(h=count+1-newalpha-cm*count,lty=dim(ciends)[2]+1)
#        if(exactonly){
#           abline(v=pi[min(seq(dim(cover)[1])[cover[,ii]==min(cover[,ii])]) ],
#              lty=dim(ciends)[2]+2)
#        }
      }
      count<-count+1
   }
   if(plot){
      if(exactonly){
         cs<-apply(cover,1,sum)
         abline(v=pi[min(seq(length(cs))[cs==min(cs)])],
            lty=dim(ciends)[2]+2)
      }
      legend(.3,.4,lty=seq(dim(ciends)[2]+1),
          legend=c(dimnames(ciends)[[2]],"Nominal Level"))
   }
   return(list(ciends=ciends,mincover=apply(cover,2,"min")))
}
