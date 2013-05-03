/*
 * cplfunctionvec.hpp
 *
 *  Created on: 16 avr. 2013
 *      Author: robin
 */

#ifndef CPLFUNCTIONVEC_HPP_
#define CPLFUNCTIONVEC_HPP_


class cplfunctionvec {

  private:
  std::vector<cplfunction> MycplfunctionList_;

  public:
  // Destructor
  ~cplfunctionvec(){
    MycplfunctionList_.clear();
  };

  //Constructors
  cplfunctionvec() : MycplfunctionList_(){};
  cplfunctionvec(int i) : MycplfunctionList_(i){};

  //Wrapper to base functions
  std::vector<cplfunction>::iterator begin(){return(MycplfunctionList_.begin());};
  std::vector<cplfunction>::iterator end(){return(MycplfunctionList_.end());};
  std::vector<cplfunction>::reverse_iterator rbegin(){return(MycplfunctionList_.rbegin());};
  void vec_set( int i,cplfunction value ) { MycplfunctionList_.at(i) = value; };
  cplfunction vec_get( int i) { return(MycplfunctionList_.at(i)); };
  int size(){ return(MycplfunctionList_.size()); };
  void push_back(cplfunction func){MycplfunctionList_.push_back(func);};

  // serialized push
  void SerialPush_1Breaks_Functions(Rcpp::NumericVector S1, Rcpp::NumericVector B1)
  {
	  int length=S1.size();
	  Rcpp::NumericVector Slopes(1);
	  Rcpp::NumericVector BreakPoints(1);
	  for (int compteur=0; compteur<length; compteur++){
		Slopes[0]=S1[compteur];
		BreakPoints[0]=B1[compteur];
		//vectorofcplfunctions_.push_back(cplfunction(Slopes,BreakPoints,0));
		MycplfunctionList_.push_back(cplfunction(Slopes,BreakPoints,0.));
	  }
  }

  void SerialPush_2Breaks_Functions(Rcpp::NumericVector S1,Rcpp::NumericVector S2, Rcpp::NumericVector B1,Rcpp::NumericVector B2)
  {
	  int length=S1.size();
	  Rcpp::NumericVector Slopes(2);
	  Rcpp::NumericVector BreakPoints(2);
	  for (int compteur=0; compteur<length; compteur++){
		Slopes[0]=S1[compteur];Slopes[1]=S2[compteur];
		BreakPoints[0]=B1[compteur];BreakPoints[1]=B2[compteur];
		//vectorofcplfunctions_.push_back(cplfunction(Slopes,BreakPoints,0));
		MycplfunctionList_.push_back(cplfunction(Slopes,BreakPoints,0));
	  }
  }



  //Optim problem solving
  Rcpp::List OptimMargInt(NumericVector Pmoins,NumericVector Pplus,NumericVector Cmoins,NumericVector Cplus){
      //cplfunctionvec Couts =*Coutsptr;
      int length=Pmoins.size();
      int compteur=0;
      std::vector<double> xEtoile(length);
      std::vector<cplfunction> f;

      cplfunction tmpfunc,tmpfunc2,tmpfunc3;
      std::vector<cplfunction>::iterator it = MycplfunctionList_.begin();

      //cplfunctionvec::iterator itprec = Couts.begin();
      tmpfunc=*it;
    	tmpfunc.Squeeze(Pmoins[compteur],Pplus[compteur]);
    	 //tmpfunc.Squeeze(Cmoins[0],Cplus[0]);
    	f.push_back(tmpfunc);
      compteur++; ++it;
    	while ( it!=MycplfunctionList_.end()){

    		 	tmpfunc=*it;
          cplfunction tmpfunc2= *(f.rbegin());
    		  tmpfunc.Squeeze(Pmoins[compteur],Pplus[compteur]);
    		  tmpfunc.Etoile();
    		  tmpfunc2.Squeeze(Cmoins[compteur-1],Cplus[compteur-1]);
          tmpfunc2.Etoile();
    		  cplfunction tmpfunc3 = Suml(tmpfunc,tmpfunc2);
    		  tmpfunc3.Etoile();
          f.push_back(tmpfunc3);
          compteur++; ++it;
       }

       std::vector<cplfunction>::reverse_iterator itr,itf;
       itr = MycplfunctionList_.rbegin();
       itf= f.rbegin();
       compteur=length-1;
       tmpfunc= *(itf);  ++itf;
    	 tmpfunc.Squeeze(Cmoins[compteur],Cplus[compteur]);
    	 //tmpfunc.Squeeze(Pmoins[length-1],Pplus[length-1]);
    	 xEtoile[compteur]=tmpfunc.Argmin();
    	 double z=xEtoile[compteur];
       while(itf!= f.rend()){
         --compteur;
         tmpfunc=*itr; ++itr;
         tmpfunc2=*itf; ++itf;
         tmpfunc.Squeeze(Pmoins[compteur+1],Pplus[compteur+1]);
      	 tmpfunc2.Squeeze(Cmoins[compteur],Cplus[compteur]);
         cplfunction tmpfunc3=InfConfFunct(tmpfunc,tmpfunc2,z);
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



#endif /* CPLFUNCTIONVEC_HPP_ */
