library(ConConPiWiFun)

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
  F2=new(cplfunction,c(-1,2),c(-2, 2),4)
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
  F2=new(cplfunction,c(-1,2),c(-2, 2),4)
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
  F2=new(cplfunction,c(-1,2),c(-2, 2),4)
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
  F2=new(cplfunction,c(-1,2),c(-2, 2),4)
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
  


  ## leak ?
cat("leak test",".\n",sep="")
 # for (i in 1:10)
#  {
#    F1=new(cplfunction,c(-1,2),c(-2, 3),4)
#    F1$Squeeze(300.,Inf)
#  }


rm(list=ls())
gc()
