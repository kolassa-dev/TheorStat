#' Simulate incorrect two-stage testing for effect between
#' exposure and result, by pretesting for the effect of 
#' a confounder, and adjusting if statistical evidence
#' says to.
#' @param qv vector of cell probabilities, going down column
#' @param cut Critical value.
#' @param r1 row 1 total
#' @param h table total.
#' @export
#********************************************************/
# Test the strategy of testing for confounding,         */
# and stratifying only if test says to.                 */
# Use h people at each level of the confounder, split   */
# between exposed and unexposed r1, h-r1 and  h-r1,r1   */
# The tables are                                        */
#   confounder=0                   confounder=1         */
#        Control Case             Control Case          */
# Unexp. ii       r1-ii   | r1    kk     h-r1-kk | h-r1 */
# Exp.   jj       h-r1-jj | h-r1  ll     r1-ll   | r1   */
# Total  ii+jj    h-ii-jj         kk+ll  h-kk-ll        */
#                                                       */
# Then confounder-exposure table is                     */
#            Confounder=0 Confounder=1                  */
# Unexposed  r1                   h-r1                  */
# Exposed    h-r1                 r1                    */
# We'll compare two procedures: always doing Mantel-    */
# Hanzel test, and only doing MH test when confounder   */
# is demonstrated related to case/control.  Table is    */
#       Control       Case                              */
# 0     ii+jj         h-ii-jj       | h                 */
# 1     kk+ll         h-kk-ll       | h                 */
# Total ii+jj+kk+ll   h-ii-jj-kk-ll                     */
# Do Chi-square test of H0:                             */
# 1: unit odds ratio accross strata. (CMH)              */
# 2: unit odds ratio, unstratified.                     */
# 3: no association between case/control and confounder.*/
fun.stratsize<-function(qv,cut,r1,h){
   cv<-c(1.96^2,1.96^2,cut)
   pv<-ind<-rep(NA,4)
   tt<-rep(NA,3)
   s<-c(0,0)
   m1<-h^2*(h-1)/(r1*(h-r1))
   for(ii in 0:r1){
      pv[1]<-dbinom(ii,r1,qv[1])
      for(jj in 0:(h-r1)){
         pv[2]<-dbinom(jj,h-r1,qv[2])
         e1<-r1*(ii+jj)/h
         for(kk in 0:(h-r1)){
            pv[3]<-dbinom(kk,h-r1,qv[3])
            for(ll in 0:r1){
               pv[4]<-dbinom(ll,r1,qv[4])
               tot <- ii+jj+kk+ll
               e2<-(h-r1)*(kk+ll)/h
               e3<-h*tot/(2*h)
               bot<-((ii+jj)*(h-ii-jj) +(kk+ll)*(h-kk-ll))
               tt[1]<-if(bot>0){
                  m1*((ii-e1)+(kk-e2))^2/bot}else{0}
               bv<-c(e3,h-e3,tot-e3,h-tot+e3)
               if(any(bv<=0)){
                  tt[2:3]<-0
               }else{
                  m2<-sum(1/bv)
                  tt[2]<-(ii+kk-e3)^2*m2
                  tt[3]<-(ii+jj-e3)^2*m2
               }
               for(mm in seq(3)) ind[mm]<-tt[mm]>cv[mm]
               ind[4]<-(ind[3]&ind[1])|((!ind[3])&ind[2])
               for(mm in seq(2))
                  s[mm]=s[mm]+prod(pv)*ind[3*mm-2]
            }#end for ll
         }#end for kk
      }#end for ll
   }#end for kk
   return(c(qv,r1,s))
}
