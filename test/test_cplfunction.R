library(ConConPiWiFun)
### problem with using double as a key in a map .... 
func1=new(cplfunction,c(-0.2,0.2) ,c(-Inf ,-2),0)
func2=new(cplfunction,c(0.0,0.4) ,c( -Inf, -3),0)
func3=Suml(func1,func2)
func3$get_BreakPoints_()$Slopes-c(-0.2,0.2,0.6)


#### Testing eq
  cat("tests for eq: \n")
  F1=new(cplfunction,c(-1,2),c(-2, 2),4)
  F2=new(cplfunction,c(-1,2),c(-2, 2),4)
  cat("\t\t test 1 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-2, 2),4)
  F2=new(cplfunction,c(-1,2),c(-2, 2),2)
  cat("\t\t test 2 : ")
  if (F1$eq(F2)==FALSE){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  
  F1=new(cplfunction,c(-1,2),c(-2, 2),4)
  F2=new(cplfunction,c(-1,2),c(-2, 3),4)
  cat("\t\t test 2 : ")
  if (F1$eq(F2)==FALSE){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
 
#### Testing Squeeze
cat("tests for Squeeze (lower bound): \n")
### cut the lower bound
  F1=new(cplfunction,c(-1,2),c(-2, 3),4)
  F1$Squeeze(-1,Inf)
  F2=new(cplfunction,c(-1,2),c(-1, 3),4)
  cat("\t\t test 1 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-2, 3),4)
  F1$Squeeze(3,Inf)
  F2=new(cplfunction,c(2),c(3),4)
  cat("\t\t test 2 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-2, 3),4)
  F1$Squeeze(32,Inf)
  F2=new(cplfunction,c(2),c(32),4)
  cat("\t\t test 3 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-Inf, 3),4)
  F1$Squeeze(-1,Inf)
  F2=new(cplfunction,c(-1,2),c(-1, 3),4)
  cat("\t\t test 4 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-Inf, 3),4)
  F1$Squeeze(3,Inf)
  F2=new(cplfunction,c(2),c(3),4)
  cat("\t\t test 5 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-Inf, 3),4)
  F1$Squeeze(32,Inf)
  F2=new(cplfunction,c(2),c(32),4)
  cat("\t\t test 6 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }


cat("tests for Squeeze (upper bound): \n")
  ### cut the lower bound
  F1=new(cplfunction,c(-1,2),c(-2, 3),4)
  F1$Squeeze(-Inf,2)
  F2=new(cplfunction,c(-1,Inf),c(-2, 2),4)
  cat("\t\t test 1 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-2, 3),4)
  F1$Squeeze(3,Inf)
  F2=new(cplfunction,c(2),c(3),4)
  cat("\t\t test 2 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-2, 3),4)
  F1$Squeeze(32,Inf)
  F2=new(cplfunction,c(2),c(32),4)
  cat("\t\t test 3 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-Inf, 3),4)
  F1$Squeeze(-1,Inf)
  F2=new(cplfunction,c(-1,2),c(-1, 3),4)
  cat("\t\t test 4 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-Inf, 3),4)
  F1$Squeeze(3,Inf)
  F2=new(cplfunction,c(2),c(3),4)
  cat("\t\t test 5 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cplfunction,c(-1,2),c(-Inf, 3),4)
  F1$Squeeze(32,Inf)
  F2=new(cplfunction,c(2),c(32),4)
  cat("\t\t test 6 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }

  F1=new(cplfunction,c(-1,Inf),c(-Inf, 3),4)
  F1$Squeeze(3,4)
  F2=new(cplfunction,c(Inf),c(3),4)
  cat("\t\t test 7 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }

  
### Argmin
cat("tests for Argmin: \n")
### 
F1=new(cplfunction,c(-1,2),c(-2, 3),4)
cat("\t\t test 1 : ")
if (F1$Argmin()==3){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
}

F1=new(cplfunction,c(-2),c(-Inf),4)
cat("\t\t test 2 : ")
if (F1$Argmin()==Inf){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
}

F1=new(cplfunction,c(2),c(-Inf),4)
cat("\t\t test 3 : ")
if (F1$Argmin()==-Inf){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
}

F1=new(cplfunction,c(-2,Inf),c(-Inf,1),4)
F1$Argmin()
cat("\t\t test 4 : ")
if (F1$Argmin()==1){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
}


F1=new(cplfunction,c(Inf),c(1),4)
F1$Argmin()
cat("\t\t test 5 : ")
if (F1$Argmin()==1){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
}


F1=new(cplfunction,c(-1,2),c(-2, 3),4)
F1$Argmin()
cat("\t\t test 6 : ")
if (F1$Argmin()==3){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
}


#### Legendre
cat("tests for Legendre: \n")
F1=new(cplfunction,c(-2,Inf),c(-Inf,1),4)
F1$Legendre()
F2=new(cplfunction,c(-2,Inf),c(-Inf,1),4)
F2$Etoile()
cat("\t\t test 1: ")
if (F1$eq(F2)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F2)
}


F1=new(cplfunction,c(-1,2,3),c(-2, 3,4),4)
F1$Legendre()
F2=new(cplfunction,c(-1,2,3),c(-2, 3,4),4)
F2$Etoile()
cat("\t\t test 2 : ")
if (F1$eq(F2)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F2)
}


F1=new(cplfunction,c(-1,2),c(-2, 3),4)
F1$Legendre()
F2=new(cplfunction,c(-1,2),c(-2, 3),4)
F2$Etoile()
cat("\t\t test 2 : ")
if (F1$eq(F2)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F2)
}


### test for EpiSum_Withline
cat("tests for EpiSum_Withline (1 break): \n")
F1=new(cplfunction,c(-1),c(-Inf),4)
F1$EpiSum_Withline(-1,1,1)
F2=new(cplfunction,c(-1),c(-Inf),4)
F2$Legendre()
F3=new(cplfunction,c(1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 1 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


F1=new(cplfunction,c(-1),c(-Inf),4)
F1$EpiSum_Withline(-1,1,1)
F2=new(cplfunction,c(-1),c(-Inf),4)
F2$Legendre()
F3=new(cplfunction,c(1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 2 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


F1=new(cplfunction,c(-1),c(1),4)
F1$EpiSum_Withline(-1,1,1)
F2=new(cplfunction,c(-1),c(1),4)
F2$Legendre()
F3=new(cplfunction,c(1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 3 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}

F1=new(cplfunction,c(-1),c(1),4)
F1$EpiSum_Withline(-1,1,-2)
F2=new(cplfunction,c(-1),c(1),4)
F2$Legendre()
F3=new(cplfunction,c(-2,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 4 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


cat("tests for EpiSum_Withline (more than 1 break): \n")
F1=new(cplfunction,c(-1,Inf),c(-Inf,0),4)
F1$EpiSum_Withline(-1,1,1)
F2=new(cplfunction,c(-1,Inf),c(-Inf,0),4)
F2$Legendre()
F3=new(cplfunction,c(1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 1 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


F1=new(cplfunction,c(-1,2),c(-Inf,0),4)
F1$EpiSum_Withline(-1,1,1)
F2=new(cplfunction,c(-1,2),c(-Inf,0),4)
F2$Legendre()
F3=new(cplfunction,c(1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 2 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}

F1=new(cplfunction,c(-1,Inf),c(-Inf,1),4)
F1$EpiSum_Withline(-1,1,1)
F2=new(cplfunction,c(-1,Inf),c(-Inf,1),4)
F2$Legendre()
F3=new(cplfunction,c(1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 3 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


F1=new(cplfunction,c(-1,2),c(-Inf,1),4)
F1$EpiSum_Withline(-1,1,1)
F2=new(cplfunction,c(-1,2),c(-Inf,1),4)
F2$Legendre()
F3=new(cplfunction,c(1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 4 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}

cat("tests for EpiSum_Withline (more than 1 break, slope<=firstslope): \n")
F1=new(cplfunction,c(-1,Inf),c(-1,1),4)
F1$EpiSum_Withline(-1,1,-1)
F2=new(cplfunction,c(-1,Inf),c(-1,1),4)
F2$Etoile()
F3=new(cplfunction,c(-1,Inf),c(-1, 1),0)
F3$Etoile()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 1 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


F1=new(cplfunction,c(-1,2),c(-1,0),4)
F1$EpiSum_Withline(-1,1,-1)
F2=new(cplfunction,c(-1,2),c(-1,0),4)
F2$Legendre()
F3=new(cplfunction,c(-1,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 2 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}

F1=new(cplfunction,c(-1,Inf),c(-1,1),4)
F1$EpiSum_Withline(-1,1,-2)
F2=new(cplfunction,c(-1,Inf),c(-1,1),4)
F2$Legendre()
F3=new(cplfunction,c(-2,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 3 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


F1=new(cplfunction,c(-1,2),c(-1,1),4)
F1$EpiSum_Withline(-1,1,-2)
F2=new(cplfunction,c(-1,2),c(-1,1),4)
F2$Legendre()
F3=new(cplfunction,c(-2,Inf),c(-1, 1),0)
F3$Legendre()
F4=Suml(F3,F2);
F4$Legendre()
cat("\t\t test 4 : ")
if (F1$eq(F4)){
  cat("OK",".\n",sep="")
}else{
  cat("ERROR.\n",sep="")
  print(F1)
  print(F4)
}


## leak ?
cat("leak test",".\n",sep="")
# for (i in 1:10)
#  {
#    F1=new(cplfunction,c(-1,2),c(-2, 3),4)
#    F1$Squeeze(300.,Inf)
#  }


rm(list=ls())
gc()
