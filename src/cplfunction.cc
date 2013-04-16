#include<cpfunction.h>

bool isincreasing(Rcpp::NumericVector arg){
	int length=arg.size();
	  bool res=true;
	   for (int n=1; n<(length); n++)
		  if (arg[n]<=arg[n-1]){
			  res=false;
			  break;
		  }
	  return res;
}


cplfunction Sum(cplfunction const & cplfunction1,cplfunction const & cplfunction2){
	 cplfunction tmp1=cplfunction1,tmp2=cplfunction2;
   		if (cplfunction2.Breakpoints_.size()>cplfunction1.Breakpoints_.size()){
   			tmp2.Sumf(tmp1);
   			return(tmp2);
   		}else{
   			tmp1.Sumf(tmp2);
   			return(tmp1);
   		}
};


cplfunction InfConv(cplfunction const & cplFunction1,cplfunction const & cplFunction2){
       cplfunction tmp1=cplFunction1,tmp2=cplFunction2;
    	 tmp1.Etoile();
    	 tmp2.Etoile();
    	 cplfunction res=Sum(tmp1,tmp2);
    	 res.Etoile();
    	 return(res);
     };
// static void finalizer_of_cplfunction( cplfunction* ptr ){
//    ptr->cplfunction::~cplfunction();
    //printf("finalizer has been called\n");
// }

cplfunction InfConfFunct(cplfunction const & cplFunction1,cplfunction const & cplFunction2,double y ){
       cplfunction tmp1=cplFunction1,tmp2=cplFunction2;
    	 tmp2.Swap(y);
    	 cplfunction B=Sum(tmp1,tmp2);
    	 return(B);
     };




RCPP_MODULE(mod_cplfunction){
  using namespace Rcpp;
  
	class_<cplfunction>( "cplfunction" )
	//constructors
	.constructor()
	.constructor<double,double>()
	//.constructor<double,double,double>()
	.constructor<double,double,double,double>()
	.constructor<Rcpp::NumericVector,Rcpp::NumericVector,double>()

	.method("clone", &cplfunction::clone)
	//.field_readonly( "Breakpoints_", &cplfunction::get_BreakPoints_ )
	.field( "FirstBreakVal_", &cplfunction::FirstBreakVal_ )

	//methods
	.method("get_BreakPoints_",&cplfunction::get_BreakPoints)
	.method("Argmin",&cplfunction::Argmin)
	.method("Squeeze",&cplfunction::Squeeze)
	.method("Swap",&cplfunction::Swap)
	.method("Etoile",&cplfunction::Etoile)
  
 // .finalizer( &finalizer_of_cplfunction)  
	;
   
  class_<cplfunctionvec>( "cplfunctionvec")
  .constructor()
  .constructor<int>()
  .method( "size", &cplfunctionvec::size)
 // .method("capacity", &cplfunctionvec::capacity,"Return size of allocated storage capacity. Returns the size of the storage space currently allocated for the vector, expressed in terms of elements.")
//  .method( "max_size", &cplfunctionvec::max_size)
  .method( "push_back", &cplfunctionvec::push_back )
//  .const_method( "at", &cplfunctionvec::at )
  .method("[[",&cplfunctionvec::vec_get)
  .method("[[<-",&cplfunctionvec::vec_set)
  .method("OptimMargInt",&cplfunctionvec::OptimMargInt,"Solves optimisation problem")
  .method("SerialPush_3Breaks_Functions",&cplfunctionvec::SerialPush_3Breaks_Functions)
  ;
  
 // function("Create_3breaks_cplfunctionvec",&Create_3breaks_cplfunctionvec)
//  ;
	
  function("Sum",&Sum,"This function allows to sum two functions of class Rcpp_cplfunction. It does not modify the imput functions.")
  ;
	function("InfConv",&InfConv,"This function performs infimum convolution of two functions of class Rcpp_cplfunction.")
  ;

}
