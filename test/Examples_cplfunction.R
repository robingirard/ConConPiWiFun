
##
#Construction of a piecewise linear function
##


Slopes=c(-1,2,Inf) # increasing ! convexity is required
Breakpoints=c(-Inf,2,4) # increasing. length is number of slopes +1
FirstNonInfBreakpointVal=3
CCPWLfunc1=new(cplfunction,Slopes,Breakpoints,FirstNonInfBreakpointVal)
plot(CCPWLfunc1) #visualisation method


###Etoile transformation (legendre transform of f)
# Changes f no return value
CCPWLfunc1$Etoile()
plot(CCPWLfunc1)  #if f = CCPWLfunc1 CCPWLfunc1 becomes is f^*(y) =inf_x {xy-f(x)}
CCPWLfunc1$Etoile()
plot(CCPWLfunc1)   ## (f^*)^* is f !


###Squeeze function 
# Changes f, no return value
left=-Inf; right=3
CCPWLfunc1$Squeeze(left,right) # CCPWLfunc1 is now infinite (or not definite) out of [left,right]
# i.e. all breakpoints out of [left,right] removed


###Swap function 
# Changes f  no return value !
y=2;
CCPWLfunc1$Swap(y)
plot(CCPWLfunc1); #now f = CCPWLfunc1 is replaced by x -> f(y-x) 


### Sum function (uses fast insertion) do not affect operands 
CCPWLfunc1=new(cplfunction,c(-1,2,Inf) ,c(-Inf,2,4),0)
CCPWLfunc2=new(cplfunction,c(-1,2,Inf),c(-Inf,1,3),0)
CCPWLfunc1plus2=Suml(CCPWLfunc1,CCPWLfunc2)
CCPWLfunc1plus2


par(mfrow=c(1,3))
plot(CCPWLfunc2,col='red'); 
plot(CCPWLfunc1,col='blue'); 
plot(CCPWLfunc1plus2);



#### Lists of CCPWLfunc
#Simple pushback
CCPWLfuncList=new(cplfunctionvec) 
CCPWLfuncList$push_back(new(cplfunction,c(-1,1) ,c(-Inf,0),0))
CCPWLfuncList$push_back(new(cplfunction,c(-1,1) ,c(-Inf,0),0))

CCPWLfuncList=new(cplfunctionvec) 
n=1000; Y=rnorm(n); S1=array(-1,n);S2=array(1,n); B0=array(-Inf,n); B1=rnorm(n); 
for (i in 1:n){
  CCPWLfuncList$push_back(new(cplfunction,c(S1[i],S2[i]) ,c(B0[i],B1[i]),0))
}
CCPWLfuncList$size() ## gives the size 
## The same but faster
CCPWLfuncList=new(cplfunctionvec) 
CCPWLfuncList$SerialPush_2Breaks_Functions(S1,S2,B0,B1);

#### method OptimMargInt solves 
#       			min_x sum_i=1^n C_i(x_i)
#                   Pmoins_i<=	x_i				<=Pplus_i 		i=1,...,n
#					Cmoins_i<=	sum_j=1^i x_j	<=Cplus_i 	i=1,...,n
n=10000; Y=runif(n,0,10);  B0=array(-Inf,n);
Pmoins=array(-1,n);Pplus=array(1,n);Cmoins=array(0,n);Cplus=array(5,n);
res=OptimPriceStorage(Y,Pmoins,Pplus,Cmoins,Cplus)
par(mfrow=c(1,2))
plot(Y,type='l',ylim=range(res$xEtoile))
lines(y=Pmoins,x=1:n,col='blue'); lines(y=Pplus,x=1:n,col='blue');
lines(y=res$xEtoile,x=1:n,col='red')
text(x=800,y=3,paste("Optimum=",signif(sum(abs(res$xEtoile-Y)),digits=6)))
plot(Y,type='l',ylim=c(min(Y),max(diffinv(res$xEtoile)[1:n+1])))
lines(y=Cmoins,x=1:n,col='blue'); lines(y=Cplus,x=1:n,col='blue');
lines(y=diffinv(res$xEtoile)[1:n+1],x=1:n,col='red')




### Price optimisation benchmarking
library(microbenchmark)
n=10000; Y=runif(n,0,10);  B0=array(-Inf,n);
Pmoins=array(-1,n);Pplus=array(1,n);Cmoins=array(0,n);Cplus=array(5,n);
res = microbenchmark(
  {
    CCPWLfuncList=new(cplfunctionvec) 
    CCPWLfuncList$SerialPush_1Breaks_Functions(Y,B0);
    CCPWLfuncList$OptimMargInt(Pmoins,Pplus,Cmoins,Cplus)
  },OptimPriceStorage(Y,Pmoins,Pplus,Cmoins,Cplus)
  )
res=OptimPriceStorage(Y,Pmoins,Pplus,Cmoins,Cplus)
CCPWLfuncList=new(cplfunctionvec) 
CCPWLfuncList$SerialPush_1Breaks_Functions(Y,B0);
res=CCPWLfuncList$OptimMargInt(Pmoins,Pplus,Cmoins,Cplus)
par(mfrow=c(1,2))
plot(Y,type='l',ylim=range(res$xEtoile))
lines(y=Pmoins,x=1:n,col='blue'); lines(y=Pplus,x=1:n,col='blue');
lines(y=res$xEtoile,x=1:n,col='red')
text(x=800,y=3,paste("Optimum=",signif(sum(abs(res$xEtoile-Y)),digits=6)))
plot(Y,type='l',ylim=c(min(Y),max(diffinv(res$xEtoile)[1:n+1])))
lines(y=Cmoins,x=1:n,col='blue'); lines(y=Cplus,x=1:n,col='blue');
lines(y=diffinv(res$xEtoile)[1:n+1],x=1:n,col='red')

rm(list=ls())
gc()



### l1 optimisation 

n=1000; Y=rnorm(n); S1=array(-1,n);S2=array(1,n); B0=array(-Inf,n); B1=rnorm(n); 
CCPWLfuncList=new(cplfunctionvec) 
CCPWLfuncList$SerialPush_2Breaks_Functions(S1,S2,B0,B1);

Pmoins=array(-1,n);Pplus=array(1,n);Cmoins=array(0,n);Cplus=array(5,n);
res=CCPWLfuncList$OptimMargInt2(Pmoins,Pplus,Cmoins,Cplus)
par(mfrow=c(1,2))
plot(Y,type='l')
lines(y=Pmoins,x=1:n,col='blue'); lines(y=Pplus,x=1:n,col='blue');
lines(y=res$xEtoile,x=1:n,col='red')
text(x=800,y=3,paste("Optimum=",signif(sum(abs(res$xEtoile-Y)),digits=6)))
plot(Y,type='l',ylim=c(min(Y),max(diffinv(res$xEtoile)[1:n+1])))
lines(y=Cmoins,x=1:n,col='blue'); lines(y=Cplus,x=1:n,col='blue');
lines(y=diffinv(res$xEtoile)[1:n+1],x=1:n,col='red')


n=1000000; Y=runif(n,0,10);  B0=array(-Inf,n);
 Pmoins=array(-1,n);Pplus=array(1,n);Cmoins=array(0,n);Cplus=array(5,n);

 rbenchmark::benchmark(OptimPriceStorage(Y,Pmoins,Pplus,Cmoins,Cplus),                     
                       replications=100,
   columns=c("test", "elapsed",
                                 "relative", "user.self", "sys.self"),
                       order="relative")


