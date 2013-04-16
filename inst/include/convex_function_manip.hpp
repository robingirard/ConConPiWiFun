/*
 * convex_function_manip.hpp
 *
 *  Created on: 16 avr. 2013
 *      Author: robin
 */

#ifndef CONVEX_FUNCTION_MANIP_HPP_
#define CONVEX_FUNCTION_MANIP_HPP_

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




#endif /* CONVEX_FUNCTION_MANIP_HPP_ */
