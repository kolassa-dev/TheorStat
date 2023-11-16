#' Simulate data from a bivariate normal distribution, and 
#' calculate various measures of association.  
#' @param rho Correlation of marginal normals
#' @param nsamp Number of observations in each sample
#' @param nmc Number of Monte Carlo simulations
#' @param progress Logical indicating whether periodic updates of progress should be made.
#' @export
#' @return list with two components: mat, a matrix with seven columns representing polychoric correlation with 2, 3, and 4 groupings, Pearson correlation, and kappa statistic for 2, 3, and 4 categories., and settings, rho and nsamp.
fun.testpoly<-function(rho=.5,nsamp=50,nmc=10000,progress=F){
  out<-array(NA,c(7,nmc))
  for(i in seq(dim(out)[2])){
     if(progress&(i==((round(i/100)*100)))) cat("i=",i)
     x<-rnorm(nsamp)
     y<-rho*x+sqrt(1-rho^2)*rnorm(nsamp)
#To make sure the table is square, make sure all potentail
#values are classified along each direction, by adding some
#fake observations, and then subtracting them out.
     tt<-table(c(x>0,0,1),c(y>0,0,1))-diag(rep(1,2))
     out[1,i]<-polychor(tt)
     out[5,i]<-cohen.kappa(tt)$kappa
     tt<-table(c((x> -1)+(x>1),0,1,2),c((y> -1)+(y>1),0,1,2))-
        diag(rep(1,3))
     out[2,i]<-polychor(tt)
     out[6,i]<-cohen.kappa(tt)$kappa
     tt<-table(c((x> -1)+(x>1)+(x>0),1,2,3,0),
         c((y> -1)+(y>1)+(y>0),1,2,3,0))-diag(rep(1,4))
     out[3,i]<-polychor(tt)
     out[7,i]<-cohen.kappa(tt)$kappa
     out[4,i]<-cor(x,y)
  }
  return(list(mat=out,settings=c(rho,nsamp)))
}
