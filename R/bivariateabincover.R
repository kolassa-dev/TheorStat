#' Plot coverage of various confidence intervals for the difference between binomial proportions.
#' @param n1 size of first binomial sample
#' @param n2 size of second binomial sample
#' @param npts number of grid points on each side of the pi1xpi2 grid
#' @importFrom stats dbinom
#' @importFrom DescTools BinomDiffCI
#' @importFrom graphics contour
#' @export
bivariatebincover<-function(n1=10,n2=10,npts=99){
   coverm<-array(0,c(npts,npts,2))
   ciout<-array(NA,c(n1+1,n2+1,2,2))
   pivecl<-list(seq(npts)/(npts+1),seq(npts)/(npts+1))
   for(kk in 0:n1) for(ll in 0:n2){
      ciout[kk+1,ll+1,1,]<-BinomDiffCI(kk,n1,ll,n2,method="wald")[2:3]
      ciout[kk+1,ll+1,2,]<-BinomDiffCI(kk,n1,ll,n2,method="scorecc")[2:3]
   }
   for(ii in 1:npts) for(jj in 1:npts){
      pi1<-pivecl[[1]][ii]
      pi2<-pivecl[[2]][jj]
      for(mm in 1:2){
         for(kk in 0:n1) for(ll in 0:n2){
            coverm[ii,jj,mm]<-coverm[ii,jj,mm]+
               dbinom(kk,n1,pi1)*dbinom(ll,n2,pi2)*
                  ((pi1-pi2)>=ciout[kk+1,ll+1,mm,1])*
                  ((pi1-pi2)<=ciout[kk+1,ll+1,mm,2])
         }
      }
   }
   par(mfrow=c(1,2))
   contour(pivecl[[1]],pivecl[[2]],coverm[,,1],levels=c(.8,.9,.95),
      main="Coverage for wald interval")
   contour(pivecl[[1]],pivecl[[2]],coverm[,,2],levels=c(.8,.9,.95),
      main="Coverage for score interval")
   par(mfrow=c(1,1))
   return(invisible(coverm))
}
