
### first test
#Value of f at first non infinite break:  4
F1=new("Rcpp_cpqfunction",c(0),c(1),c(-2, 2),4)
F2=new("Rcpp_cpqfunction",c(0),c(1),c(-2, 2),4)
F3=new("Rcpp_cpqfunction",c(0),c(1),c(-2, 2),8);
F4=Sum(F1,F2);
eq(F3,F4)

### second test
#Value of f at first non infinite break:  4
F1=new("Rcpp_cpqfunction",c(0,1),c(1,2),c(-2, 0, 2),4)
F2=new("Rcpp_cpqfunction",c(0),c(1),c(-2, 2),4)
F3=new("Rcpp_cpqfunction",c(0,1),c(2,3),c(-2, 0, 2),8);
F4=Sum(F1,F2);
eq(F3,F4)

### troisième test
#Value of f at first non infinite break:  4
F1=new("Rcpp_cpqfunction",c(0,1),c(0,1),c(-2, 0, 2),4)
F2=new("Rcpp_cpqfunction",c(0),c(1),c(-2, 2),4)
F3=new("Rcpp_cpqfunction",c(0,1),c(1,2),c(-2, 0, 2),8);
F4=Sum(F1,F2);
eq(F3,F4) #not implemented

### troisième test
#Value of f at first non infinite break:  4
F1=new("Rcpp_cpqfunction",c(0,1),c(0,1),c(-2, 0, 2),4)
F2=new("Rcpp_cpqfunction",c(0),c(1),c(-2, 2),4)
F3=new("Rcpp_cpqfunction",c(0,1),c(1,2),c(-2, 0, 2),8);
F4=Sum(F1,F2);
eq(F3,F4)

