#include "ConConPiWiFun.h"


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
	.method("eq",&cplfunction::eq)
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

  function("Suml",&Sum,"This function allows to sum two functions of class Rcpp_cplfunction. It does not modify the imput functions.")
  ;
  function("InfConv",&InfConv,"This function performs infimum convolution of two functions of class Rcpp_cplfunction.")
  ;

}


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
	.method("eq",&cpqfunction::eq)
	.method("evalf",&cpqfunction::evalf)

 // .finalizer( &finalizer_of_cplfunction)
	;

  function("Sumq",&Sumq,"This function allows to sum two functions of class Rcpp_cpqfunction. It does not modify the imput functions.")
  ;

}
