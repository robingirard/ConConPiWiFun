/*
 * cplqfunctionvec.hpp
 *
 *  Created on: 2 mai 2013
 *      Author: robin
 */

#ifndef CPLQFUNCTIONVEC_HPP_
#define CPLQFUNCTIONVEC_HPP_


class cpqfunctionvec {

  private:
  std::vector<cpqfunction> MycpqfunctionList_;

  public:
  // Destructor
  ~cpqfunctionvec(){
    MycpqfunctionList_.clear();
  };

  //Constructors
  cpqfunctionvec() : MycpqfunctionList_(){};
  cpqfunctionvec(int i) : MycpqfunctionList_(i){};

  //Wrapper to base functions
  std::vector<cpqfunction>::iterator begin(){return(MycpqfunctionList_.begin());};
  std::vector<cpqfunction>::iterator end(){return(MycpqfunctionList_.end());};
  std::vector<cpqfunction>::reverse_iterator rbegin(){return(MycpqfunctionList_.rbegin());};
  void vec_set( int i,cpqfunction value ) { MycpqfunctionList_.at(i) = value; };
  cpqfunction vec_get( int i) { return(MycpqfunctionList_.at(i)); };
  int size(){ return(MycpqfunctionList_.size()); };
  void push_back(cpqfunction func){MycpqfunctionList_.push_back(func);};

  // serialized push
  void SerialPush_1Breaks_Functions(Rcpp::NumericVector S0, Rcpp::NumericVector S1,Rcpp::NumericVector B1)
  {
	  int length=S1.size();
	  Rcpp::NumericVector Slopes0(1),Slopes1(1);
	  Rcpp::NumericVector BreakPoints(2);
	  for (int compteur=0; compteur<length; compteur++){
		Slopes0[0]=S0[compteur];Slopes1[0]=S1[compteur];
		BreakPoints[0]=B1[compteur];
		BreakPoints[1]=numeric_limits<double>::infinity();
		//vectorofcpqfunctions_.push_back(cpqfunction(Slopes,BreakPoints,0));
		MycpqfunctionList_.push_back(cpqfunction(Slopes0,Slopes1,BreakPoints,0.0));
	  }
  }

  void SerialPush_0Breaks_Functions(Rcpp::NumericVector S0, Rcpp::NumericVector S1)
  {
	  int length=S1.size();
	  Rcpp::NumericVector Slopes0(1),Slopes1(1);
	  Rcpp::NumericVector BreakPoints(2);
	  for (int compteur=0; compteur<length; compteur++){
		Slopes0[0]=S0[compteur];Slopes1[0]=S1[compteur];
		BreakPoints[0]=-numeric_limits<double>::infinity();
		BreakPoints[1]=numeric_limits<double>::infinity();
		//vectorofcpqfunctions_.push_back(cpqfunction(Slopes,BreakPoints,0));
		MycpqfunctionList_.push_back(cpqfunction(Slopes0,Slopes1,BreakPoints,0.0));
	  }
  }


  //Optim problem solving
  Rcpp::List OptimMargInt(NumericVector Pmoins,NumericVector Pplus,NumericVector Cmoins,NumericVector Cplus){
      //cpqfunctionvec Couts =*Coutsptr;
      int length=Pmoins.size();
      int compteur=0;
      std::vector<double> xEtoile(length);
      std::vector<cpqfunction> f;

      cpqfunction tmpfunc,tmpfunc2,tmpfunc3;
      std::vector<cpqfunction>::iterator it = MycpqfunctionList_.begin();

      //cpqfunctionvec::iterator itprec = Couts.begin();
      tmpfunc=*it;
    	tmpfunc.Squeeze(Pmoins[compteur],Pplus[compteur]);
    	 //tmpfunc.Squeeze(Cmoins[0],Cplus[0]);
    	f.push_back(tmpfunc);
      compteur++; ++it;
    	while ( it!=MycpqfunctionList_.end())
    	{
			tmpfunc=*it;
			cpqfunction tmpfunc2= *(f.rbegin());
			tmpfunc.Squeeze(Pmoins[compteur],Pplus[compteur]);
			tmpfunc.Etoile();
			tmpfunc2.Squeeze(Cmoins[compteur-1],Cplus[compteur-1]);
			tmpfunc2.Etoile();
			cpqfunction tmpfunc3 = Sumq(tmpfunc,tmpfunc2);
			tmpfunc3.Etoile();
			f.push_back(tmpfunc3);
			compteur++; ++it;
    	}
       std::vector<cpqfunction>::reverse_iterator itr,itf;
       itr = MycpqfunctionList_.rbegin();
       itf= f.rbegin();
       compteur=length-1;
       tmpfunc= *(itf);  ++itf;
    	 tmpfunc.Squeeze(Cmoins[compteur],Cplus[compteur]);
    	 //tmpfunc.Squeeze(Pmoins[length-1],Pplus[length-1]);
    	 xEtoile[compteur]=tmpfunc.Argmin();
    	 double z=xEtoile[compteur];
		while(itf!= f.rend())
		{
			--compteur;
			tmpfunc=*itr; ++itr;
			tmpfunc2=*itf; ++itf;
			tmpfunc.Squeeze(Pmoins[compteur+1],Pplus[compteur+1]);
			tmpfunc2.Squeeze(Cmoins[compteur],Cplus[compteur]);
			cpqfunction tmpfunc3=InfConfFunctq(tmpfunc,tmpfunc2,z);
			xEtoile[compteur]=tmpfunc3.Argmin();
			z=z-xEtoile[compteur];
			xEtoile[compteur]=z;
		}
    	 double tmpval,tmpval1=0;
    	 for (int i=0;i<length;i++){
    		 tmpval=xEtoile[i];
    		 xEtoile[i]=xEtoile[i]-tmpval1;
    		 tmpval1=tmpval;
    	 }
       return Rcpp::List::create(
    		Rcpp::Named("xEtoile") = Rcpp::wrap(xEtoile));
     };
};


#endif /* CPLQFUNCTIONVEC_HPP_ */
