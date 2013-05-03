
library(microbenchmark)

Pmoins=array(-1,n);Pplus=array(1,n);Cmoins=array(0,n);Cplus=array(5,n);
CCPWLfuncList=new(cplfunction2vec) 
n=1000; Y=rnorm(n); S1=array(-1,n);S2=array(1,n); B0=array(-Inf,n); B1=rnorm(n); B2=array(Inf,n); 

res = microbenchmark(
{
  CCPWLfuncList=new(cplfunctionvec) ;
  CCPWLfuncList$SerialPush_3Breaks_Functions(S1,S2,B0,B1,B2);
  CCPWLfuncList$OptimMargInt(Pmoins,Pplus,Cmoins,Cplus)
},
{
  CCPWLfuncList=new(cplfunction2vec) ;
  CCPWLfuncList$SerialPush_3Breaks_Functions(S1,S2,B0,B1);
  CCPWLfuncList$OptimMargInt(Pmoins,Pplus,Cmoins,Cplus)
},times = 100L)

rm(list=ls())
gc()
