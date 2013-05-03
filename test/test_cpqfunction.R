#TODO : write more test for other functions
library(ConConPiWiFun)

#### Testing eq
  cat("tests for eq: \n")
  F1=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  cat("\t\t test 1 : ")
  if (F1$eq(F2)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  F1=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-2, 2),2)
  cat("\t\t test 2 : ")
  if (F1$eq(F2)==FALSE){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }
  
  F1=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-2, 1),4)
  cat("\t\t test 3 : ")
  if (F1$eq(F2)==FALSE){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    print(F1)
    print(F2)
  }

#### Testing evalf
cat("tests for evalf: \n")
  ### test 1
  cat("\t\t test 1 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  trueval=2;
  if (F1$evalf(0)==trueval){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    cat(F1,": "); print(F1$evalf(0));
    cat(", should be ",trueval, ". \n")
  }
  ### test 1
  cat("\t\t test 2 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0,1),c(1,2),c(-2, 0, 2),4)
  trueval=2;
  if (F1$evalf(0)==trueval){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    cat(F1,": "); print(F1$evalf(0));
    cat(", should be ",trueval, ". \n")
  }


#### Testing the Sum
cat("tests for the sum: \n")
  ### test 1
  cat("\t\t test 1 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F3=new(cpqfunction,c(0),c(2),c(-2, 2),8);
  F4=Sumq(F1,F2);
  if (F3$eq(F4)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    cat("F3",": \n"); print(F3);
    cat("F4",": \n"); print(F4);
  }

  ### test 2
  cat("\t\t test 2 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-3, 1),9)
  F3=new(cpqfunction,c(0),c(2),c(-2, 1),10.5);
  F4=Sumq(F1,F2);
  if (F3$eq(F4)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    cat("F3",": \n"); print(F3);
    cat("F4",": \n"); print(F4);
  }


  ### test 3
  cat("\t\t test 3 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0,1),c(1,2),c(-2, 0, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F3=new(cpqfunction,c(0,1),c(2,3),c(-2, 0, 2),8);
  F4=Sumq(F1,F2);
  if (F3$eq(F4)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    cat("F3",": \n"); print(F3);
    cat("F4",": \n"); print(F4);
  }

  ### test 4
  cat("\t\t test 4 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0,1),c(0,1),c(-2, 0, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F3=new(cpqfunction,c(0,1),c(1,2),c(-2, 0, 2),8);
  F4=Sumq(F1,F2);
  if (F3$eq(F4)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    cat("F3",": \n"); print(F3);
    cat("F4",": \n"); print(F4);
  }

  ### test 5
  cat("\t\t test 5 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0,1),c(0,1),c(-2, 0, 2),4)
  F2=new(cpqfunction,c(0),c(1),c(-2, 2),4)
  F3=new(cpqfunction,c(0,1),c(1,2),c(-2, 0, 2),8);
  F4=Sumq(F1,F2);
  if (F3$eq(F4)){
    cat("OK",".\n",sep="")
  }else{
    cat("ERROR.\n",sep="")
    cat("F3",": \n"); print(F3);
    cat("F4",": \n"); print(F4);
  }


#### Testing the Etoile
cat("tests for the Etoile: \n")
  cat("\t\t test 1 : ")
  #Value of f at first non infinite break:  4
  F1=new(cpqfunction,c(0),c(1),c(-Inf,Inf),0) ## f(x)=x^2+cte
  F0=F1$clone();
  F0$Etoile()
  

rm(list=ls())
  gc()
