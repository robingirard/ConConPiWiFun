#leak test 

library(ConConPiWiFun)
##
#piecewise linear function
##
cat("Leak test for linear function \n")
for (i in 1:1000){
  Slopes=c(-1,2) # increasing ! convexity is required
  Breakpoints=c(-Inf,2,4) # increasing. length is number of slopes +1
  FirstNonInfBreakpointVal=3
  CCPWLfunc1=new(cplfunction,Slopes,Breakpoints,FirstNonInfBreakpointVal)
  CCPWLfunc1$get_BreakPoints_() ## return Breaks AND Slopes
  CCPWLfunc1$Etoile()
  left=-1; right=4
  CCPWLfunc1$Squeeze(left,right) # CCPWLfunc1 is now infinite (or not definite) out of [left,right]
  y=2
  CCPWLfunc1$Swap(y)
  CCPWLfunc1=new(cplfunction,c(-1,2) ,c(-Inf,2,4),0)
  CCPWLfunc2=new(cplfunction,c(-1,2),c(-Inf,1,3),0)
  CCPWLfunc1plus2=Suml(CCPWLfunc1,CCPWLfunc2)
}

##
#piecewise quadratic function
##
cat("Leak test for quadratic function \n")
for (i in 1:1000){
  CCPWLfunc1=new(cpqfunction,c(-1,2),c(0,3),c(-Inf,2,4),4)
  CCPWLfunc1$get_BreakPoints_() ## return Breaks AND Slopes
  CCPWLfunc1$Etoile()
   left=-1; right=4
   CCPWLfunc1$Squeeze(left,right) # CCPWLfunc1 is now infinite (or not definite) out of [left,right]
   y=2
   CCPWLfunc1$Swap(y)
   CCPWLfunc1=new(cplfunction,c(-1,2) ,c(-Inf,2,4),0)
   CCPWLfunc2=new(cplfunction,c(-1,2),c(-Inf,1,3),0)
   CCPWLfunc1plus2=Sumq(CCPWLfunc1,CCPWLfunc2)
}

gc()
