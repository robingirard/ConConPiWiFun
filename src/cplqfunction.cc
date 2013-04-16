#include<cpfunction.h>


// here polynom is 1/2 ax^2+bx+c
double getSlope(pair<double,double> Coefficients,double val){
// returns the slope at val given Coefficients a and b.f
   if (val==-numeric_limits<double>::infinity()&&Coefficients.first!=0){
  	 if (Coefficients.first<0){
			 return(numeric_limits<double>::infinity());
		 }else{
			 return(-numeric_limits<double>::infinity());
		 }
	 }else{
		 if (val==numeric_limits<double>::infinity()&&Coefficients.first!=0){
			 if (Coefficients.first<0){
				 return(-numeric_limits<double>::infinity());
			 }else{
				 return(numeric_limits<double>::infinity());
			 }
		 }else{
			 return(Coefficients.first*val+Coefficients.second);
		 }
	 }
};

double getVal(pair<double,double> Coefficients,double val){
// returns the val at val given Coefficients at 0 and 1
	 if (val==-numeric_limits<double>::infinity()&&Coefficients.first!=0){
		 if (Coefficients.first<0){
			 return(numeric_limits<double>::infinity());
		 }else{
			 return(-numeric_limits<double>::infinity());
		 }
	 }else{
		 if (val==numeric_limits<double>::infinity()&&Coefficients.first!=0){
			 if (Coefficients.first<0){
				 return(-numeric_limits<double>::infinity());
			 }else{
				 return(numeric_limits<double>::infinity());
			 }
		 }else{
			 return(Coefficients.first/2*val*val+Coefficients.second*val);
		 }
	 }
};

double getXetoile(pair<double,double> Coefficients){
	 if (Coefficients.first==0){
		 if (Coefficients.second==0)
		 {
			 return(0);
		 }
		 else{
			 if (Coefficients.second<0){
				 return(numeric_limits<double>::infinity());
			 }else{
				 return(-numeric_limits<double>::infinity());
			 }
		 }
	 }else{
		 return(-Coefficients.second/Coefficients.first);
	 }
};

pair<double,double> Slopes2Coeffs(double Slope0,double Slope1){
  // returns the a and b coefficient of 1/2 ax^2+bx+c given the slopes in zero and the slopes in 1
  // a= S1-S0
  pair<double,double> res;
  res.first=Slope1-Slope0;
  res.second=Slope0;
  return(res);
}

cpqfunction Sumq(cpqfunction const & cpqfunction1,cpqfunction const & cpqfunction2){
   cpqfunction tmp1=cpqfunction1,tmp2=cpqfunction2;
   		if (cpqfunction2.Breakpoints_.size()>cpqfunction1.Breakpoints_.size()){
   			tmp2.Sumf(tmp1);
   			return(tmp2);
   		}else{
   			tmp1.Sumf(tmp2);
   			return(tmp1);
   		}
};



RCPP_MODULE(mod_cpqfunction){
  using namespace Rcpp;
  
	class_<cpqfunction>( "cpqfunction" )
	//constructors
	.constructor()
//	.constructor<double,double>()
	//.constructor<double,double,double>()
	//.constructor<double,double,double,double>()
	.constructor<Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,double>()

	.method("clone", &cpqfunction::clone)
	//.field_readonly( "Breakpoints_", &cpqfunction::get_BreakPoints_ )
	.field( "FirstBreakVal_", &cpqfunction::FirstBreakVal_ )

	//methods
	.method("get_BreakPoints_",&cpqfunction::get_BreakPoints)
	.method("Argmin",&cpqfunction::Argmin)
	.method("Squeeze",&cpqfunction::Squeeze)
	.method("Swap",&cpqfunction::Swap)
	.method("Etoile",&cpqfunction::Etoile)

 // .finalizer( &finalizer_of_cplfunction)  
	;
  
  function("Sumq",&Sumq,"This function allows to sum two functions of class Rcpp_cpqfunction. It does not modify the imput functions.")
  ;

}
