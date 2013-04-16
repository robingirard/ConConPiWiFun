/*
 * ConConPiWiFun.h
 *
 *  Created on: 16 avr. 2013
 *      Author: robin
 */

#ifndef CONCONPIWIFUN_H_
#define CONCONPIWIFUN_H_

#include <math.h>
#include <limits>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;
class cplfunction ;
class cplfunctionvec;
class cpqfunction ;

RCPP_EXPOSED_CLASS(cplfunction)
RCPP_EXPOSED_CLASS(cplfunctionvec)
RCPP_EXPOSED_CLASS(cpqfunction)

bool isincreasing(Rcpp::NumericVector arg);
double getSlope(pair<double,double> Coefficients,double val);
double getVal(pair<double,double> Coefficients,double val);
double getXetoile(pair<double,double> Coefficients);
pair<double,double> Slopes2Coeffs(double Slopes0,double Slopes1);

#include "convex_functions_tools.hpp"


#include "cplfunction.hpp"
#include "cpqfunction.hpp"
#include "convex_function_manip.hpp"

#include "cplfunctionvec.hpp"


//cplfunction InfConfFunct(cplfunction const & cplFunction1,cplfunction const & cplFunction2,double y );
//cplfunction Sum(cplfunction const & cplfunction1,cplfunction const & cplfunction2);
//cpqfunction Sumq(cpqfunction const & cpqfunction1,cpqfunction const & cpqfunction2);



#endif /* CONCONPIWIFUN_H_ */
