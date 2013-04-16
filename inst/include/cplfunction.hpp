/*
 * cplfunction.hpp
 *
 *  Created on: 16 avr. 2013
 *      Author: robin
 */

#ifndef CPLFUNCTION_HPP_
#define CPLFUNCTION_HPP_

class cplfunction {

    public:
    map<double,double> Breakpoints_; // breakpoints
    double FirstBreakVal_; // firstbreakval

    ~cplfunction(){
      Breakpoints_.clear();
    };

    cplfunction() : Breakpoints_(), FirstBreakVal_(0){}

    cplfunction(int NbSlopes, double * Slopes, double * BreakPoints,double FirstBreakVal) {
		  create_cplfunction(NbSlopes,Slopes,BreakPoints,FirstBreakVal);
	  }

    class emptyfunc : public std::exception {
      public:
      const char * what() { return "empty function"; }
    };

    class nonincreasingslopes : public std::exception {
      public:
      const char * what() { return "non increasing slopes"; }
    };

    class nonincreasingbreakpoints : public std::exception {
      public:
      const char * what() { return "non increasing breakpoints"; }
    };

    cplfunction(Rcpp::NumericVector Slopes, Rcpp::NumericVector BreakPoints,double FirstBreakVal){
  		int NbSlopes=  Slopes.size();
  		if (NbSlopes+1==BreakPoints.size()){
  			if (isincreasing(Slopes)){
  				if (isincreasing(BreakPoints)){
  					for (int i=0; i<NbSlopes; i++){
  						   Breakpoints_[BreakPoints[i]]=Slopes[i];
  					   }
  					   Breakpoints_[BreakPoints[NbSlopes]]=numeric_limits<double>::infinity();
  					   FirstBreakVal_= FirstBreakVal;
  				}else{
  					Rprintf( "Error: non increasing breakpoints" ) ;
  					throw nonincreasingbreakpoints() ;
  				}
  			}else{
  				Rprintf( "Error: non increasing Slopes" ) ;
  				throw nonincreasingslopes() ;
  			}
  		}else{
  			Rprintf( "Error: number of Slopes must be number of breaks -1 " ) ;
  			throw nonincreasingslopes() ;
  		}
	  }

	  cplfunction(cplfunction const & x) : Breakpoints_(x.Breakpoints_), FirstBreakVal_(x.FirstBreakVal_) {
	  }

    cplfunction* clone() const {
        return new cplfunction(*this) ;
    }

    cplfunction(double uniquebreak,double val){
	    int NbSlopes=0;
	    double Slopes [1]={numeric_limits<double>::infinity()};
	    double BreakPoints [1]={uniquebreak};
	    create_cplfunction(NbSlopes,Slopes,BreakPoints,val);
    }

    cplfunction(double uniquebreak,double val,double Slope1){
	   int NbSlopes=1;
	   double Slopes [2]={Slope1,numeric_limits<double>::infinity()};
	   double BreakPoints [2]={uniquebreak,numeric_limits<double>::infinity()};
	   create_cplfunction(NbSlopes,Slopes,BreakPoints,val);
   }

    cplfunction(double uniquebreak,double val,double Slope1, double Slope2){
   		if (Slope1<Slope2){
   		   int NbSlopes=2;
   		   double Slopes [2]={Slope1,Slope2};
   		   double BreakPoints [3]={-numeric_limits<double>::infinity(),uniquebreak,numeric_limits<double>::infinity()};
   		   create_cplfunction(NbSlopes,Slopes,BreakPoints,val);
   		}else{
			Rprintf( "Error: first Slope mustn't be greater or equal to second Slope" ) ;
			throw nonincreasingbreakpoints() ;
   		}

   }

    void create_cplfunction(int NbSlopes, double * Slopes, double * BreakPoints,double FirstBreakVal) {
	    for (int i=0; i<NbSlopes; i++){
		    Breakpoints_[BreakPoints[i]]=Slopes[i];
	    }
	    Breakpoints_[BreakPoints[NbSlopes]]=numeric_limits<double>::infinity();
	    FirstBreakVal_= FirstBreakVal;
    }

    Rcpp::List get_BreakPoints(){
      std::vector<double> Breakpoints;
  		std::vector<double> Slopes;
	 	 	map<double,double>::iterator it=Breakpoints_.begin();
	 	 	int nbSlopes=0,compteur=0;
	 	 	while(it != Breakpoints_.end()) {it++; nbSlopes++;}
	 	 	nbSlopes--;
  			it=Breakpoints_.begin();
  			compteur=0;
  			while(it != Breakpoints_.end()) {
  				Breakpoints.push_back( it->first );
  				if (compteur != (nbSlopes+1)){
  					Slopes.push_back( it->second );
  				}
  				it++; compteur++;
  			}

  			return Rcpp::List::create(
				Rcpp::Named("Breakpoints") = Rcpp::wrap(Breakpoints),
				Rcpp::Named("Slopes") = Rcpp::wrap(Slopes));
  	}

    cplfunction(double * twobreaks,double slope, double val){
 	   int NbSlopes=1;
 	   double Slopes [2]={slope, numeric_limits<double>::infinity()};
 	   create_cplfunction(NbSlopes,Slopes,twobreaks,val);
    }

    /* cplfunction(simplefunction sfunc){
 	   int NbSlopes=2;
 	   double Slopes [2]={sfunc.leftslope_, sfunc.rightslope_};
 	   double BreakPoints [3]={-numeric_limits<double>::infinity(),sfunc.breakpoint_,numeric_limits<double>::infinity()};
 	   create_cplfunction(NbSlopes,Slopes,BreakPoints,sfunc.val_);
    };*/

    cplfunction & operator = (cplfunction & s) {
     /* Cleanup current data */
     if(this != &s) {
      Breakpoints_.clear();
      /* copy needed data, call copy constructor
       * not efficient but will call copy constructor
       * */
      Breakpoints_=s.Breakpoints_;
      FirstBreakVal_=s.FirstBreakVal_;
     }
     return *this;
    }

    void AddSimple(double leftslope, double rightslope, double val, double breakpoint){
 	    map<double, double>::iterator i = Breakpoints_.begin();
      FirstBreakVal_=FirstBreakVal_+val;
 	    if (rightslope==leftslope){
   		  while(i != Breakpoints_.end()) {
   			  (*i).second=i->second+leftslope;
   		   	++i;
   		  }
 	    }else{
        if (breakpoint<=(*Breakpoints_.begin()).first){
          //BreakPoint is out of the domain, on the left
     		  while(i != Breakpoints_.end()) {
     			  (*i).second=i->second+rightslope;
     		   	++i;
     		  }
        }else{
          if (breakpoint>=(*Breakpoints_.rbegin()).first){
            while(i != Breakpoints_.end()) {
           	  (*i).second=i->second+leftslope;
           		++i;
           	}
          }else{
            /*here the new breakpoint is inside the domain of this and
            the rightslope and left slopes are different*/
         	  map<double, double>::iterator it,ittmp;
         		unsigned int initialsize=Breakpoints_.size();
         		//insert the new breakpoint
            it=Breakpoints_.insert(pair<double, double> (breakpoint, 0.0)).first;
           	it--;
            ittmp=it;
            it++;
           	if (Breakpoints_.size()!=initialsize){
              //cout<<(*it).first<<","<<(*it).second<<endl;
           		//map<double, double>::iterator it2=Breakpoints_.begin();
           	  (*it).second = (*ittmp).second;
           	}
            map<double, double>::iterator i = Breakpoints_.begin();
           	while(i != it) {
           	  (*i).second=i->second+leftslope;
           		++i;
           	}
           	while(i != Breakpoints_.end()) {
           	  (*i).second=i->second+rightslope;
           		++i;
           	}
 	        }
 	   	  }
 	    }
    }

    bool eq(cplfunction  const & cplfunction1){
 	   if (FirstBreakVal_!=cplfunction1.FirstBreakVal_){
 		   return(false);
 	   }
 	   if (Breakpoints_.size()!=cplfunction1.Breakpoints_.size()){
 		   return(false);
 	   }else{
 		   map<double, double> mybreak=Breakpoints_;
   		   map<double, double>::iterator i = Breakpoints_.begin(),i2=mybreak.begin();
   		   while(i != Breakpoints_.end()) {
   			   if (i->first==i2->first&&i->second==i2->second){
   				 ++i;++i2;
   			   }else{
   				   return(false);
   			   }
   		   }
   		   return(true);
 	   }
    }

    void Etoile(){
 	   int compteur=0;
 	   double * newSlopes;
 	   double * newBreak;
 	   cplfunction tmp(*this);
 	   Breakpoints_.clear();

 	   map<double,double>::iterator ittmp,it=tmp.Breakpoints_.begin();
 	   double firstbreak=it->first;
 	   map<double,double>::reverse_iterator rit=tmp.Breakpoints_.rbegin();
 	//   for ( rit=mybreaks.rbegin() ; rit != mybreaks.rend(); rit++ )
 	//	      cout << rit->first << " => " << rit->second << endl;
 	   rit=tmp.Breakpoints_.rbegin() ;
 	   double lastbreak=rit->first;
      double firstBreakVal=0;
 	   int NbSlopes=tmp.Breakpoints_.size()-1;
 	   it=tmp.Breakpoints_.begin();ittmp=tmp.Breakpoints_.begin();

 	   if ((firstbreak==-numeric_limits<double>::infinity())){
   		  if (NbSlopes==0){firstBreakVal=-tmp.FirstBreakVal_;
   		  }else{
   			  ittmp++;
   			  firstBreakVal=-tmp.FirstBreakVal_;
   		  }
 		   if (lastbreak==numeric_limits<double>::infinity()){
 			   //cout<<"B1"<<endl;
 			   /* B[0]=-inf B[end]=+Inf
 			    * nB=S[1:NbSlopes] nS=B[2:NbSlopes]*/
 			   newSlopes = new double [NbSlopes-1];
 			   newBreak = new double [NbSlopes];
 			   while(compteur != NbSlopes-1) {
 				   newBreak[compteur]=it->second;
 			   	   ++it;
 			   	// B[2 ..
 			   	   newSlopes[compteur]=it->first;
 			   	   compteur++;
 			   }
 			   newBreak[NbSlopes-1]=it->second;
 			   NbSlopes--;

 		   	   }else{/* B[0]=-inf B[end]!=+Inf Send=NbSlopes Bend=NbSlopes+1
 			    * nB=[S[1:NbSlopes],Inf] nS=B[2:NbSlopes+1]*/
 		   		 //cout<<"B2"<<endl;
 				   newSlopes = new double [NbSlopes];
 				   newBreak = new double [NbSlopes+1];
 				   while(compteur != NbSlopes) {
 					   newBreak[compteur]=it->second;
 					   ++it;
 					  // B[2 ..
 				   	   newSlopes[compteur]=it->first;
 				   	   compteur++;
 				   }
 		   		   newBreak[NbSlopes]=numeric_limits<double>::infinity();
 		   	   }
 	   }else{
  		  if (NbSlopes==0){firstBreakVal=-tmp.FirstBreakVal_;
  		  }else{
  			  firstBreakVal=-tmp.FirstBreakVal_;
  		  }
 		   if (lastbreak==numeric_limits<double>::infinity()){ //cout<<"B3"<<endl;
 			   /* B[0]!=-inf B[end]=+Inf Send=NbSlopes Bend=NbSlopes+1
 			    * nB=[-Inf,S[1:NbSlopes]] nS=B[1:NbSlopes]*/

 			   newSlopes= new double [NbSlopes];
 			   newBreak= new double [NbSlopes+1];
 			   while(compteur != NbSlopes) {

 				   newBreak[compteur+1]=it->second;
 			   	   newSlopes[compteur]=it->first;
 			   	   compteur++;
 			   	   ++it;

 			   }
 	   		   newBreak[0]=-numeric_limits<double>::infinity();
 		   }else{ //cout<<"B4"<<endl;
 			   /* B[0]!=-inf B[end]!=+Inf Send=NbSlopes Bend=NbSlopes+1
 			   	* nB=[-Inf,S[1:NbSlopes],Inf] nS=B[1:NbSlopes+1]*/
 			   	newSlopes= new double [NbSlopes+1];
 			   	newBreak= new double [NbSlopes+2];
 			   	while(compteur != (NbSlopes+1)) {

 			   		newBreak[compteur+1]=it->second;
 			   		newSlopes[compteur]=it->first;
 			   		compteur++;
 			   		++it;

 			   	}
 			   	newBreak[NbSlopes+1]=numeric_limits<double>::infinity();
 			   	newBreak[0]=-numeric_limits<double>::infinity();
 			   	NbSlopes++;
 		   }
 	   }
 	   if (NbSlopes==0){
 		   Breakpoints_[newBreak[NbSlopes]]=numeric_limits<double>::infinity();
 		   FirstBreakVal_= -tmp.FirstBreakVal_;
 	   }else{
 		   for (int i=0; i<NbSlopes; i++){
 			   Breakpoints_[newBreak[i]]=newSlopes[i];
 		   }
 		   Breakpoints_[newBreak[NbSlopes]]=numeric_limits<double>::infinity();
 		   FirstBreakVal_= -tmp.FirstBreakVal_;
 	   }

 	   	delete [] newSlopes;
 	   	delete [] newBreak;
 	   	//return(*this);
    }

    double Argmin(){
 	  // cout << __FUNCTION__ << endl;
 	   //this->print();
 	   double res;
 	   cplfunction tmp(*this);
 	   int NbSlopes=tmp.Breakpoints_.size()-1;
 	   if (NbSlopes<2){
 		   if (NbSlopes==1){
 		       if (tmp.Breakpoints_.begin()->second<=0){
 		    	  res =tmp.Breakpoints_.rbegin()->first;
 		       }else{
 		    	   res =tmp.Breakpoints_.begin()->first;
 		       }
 		   }else{
 			   if (NbSlopes==0){
 				   res =tmp.Breakpoints_.begin()->first;
 			   }else{

 				 //  cout<<"NbSlopes="<<NbSlopes<<endl;
 				   throw emptyfunc();
 			   }
 		   }
 	   }else{
 	       if (tmp.Breakpoints_.begin()->second>0){

 	    	   res =tmp.Breakpoints_.begin()->first;
 	       }else{
       		 map<double, double>::iterator i = tmp.Breakpoints_.begin();
       		++i;
       		   while(i != tmp.Breakpoints_.end()) {
       			 res=i->first;
       			if (i->second>0){ break;}
       			++i;
       		   }
 	       }
 	   }
 	  // cout<<"res="<<res<<endl;
 	   return(res);
    }

    void Squeeze(double leftBreak,double rightBreak){
  		 //  cout << __FUNCTION__ << "("<<leftBreak<<","<<rightBreak<<")"<<endl;
  		 //  this->print();
  	   cplfunction tmp(*this);

  	   if (tmp.Breakpoints_.size()<1 ||leftBreak>=rightBreak ||tmp.Breakpoints_.begin()->first>=rightBreak ||tmp.Breakpoints_.rbegin()->first<=leftBreak){
  		   if (tmp.Breakpoints_.begin()->first==rightBreak){
  			   Breakpoints_.clear();
  			   Breakpoints_[tmp.Breakpoints_.begin()->first]=numeric_limits<double>::infinity();
  		   }else{
  			   if (tmp.Breakpoints_.rbegin()->first==leftBreak){
  				   Breakpoints_.clear();
  				   Breakpoints_[tmp.Breakpoints_.rbegin()->first]=numeric_limits<double>::infinity();
  			   }else{
  				  // cout<<"in Squeeze"<<endl;
  				   throw emptyfunc();
  			   }
  		   }
  	   }else{
  		   if (tmp.Breakpoints_.size()==1){
  			   Breakpoints_.clear();
  			   Breakpoints_[tmp.Breakpoints_.rbegin()->first]=numeric_limits<double>::infinity();
  		   }else{
  	   		   map<double, double>::iterator itleft,itright,itb;
  			 //  unsigned int initialsize=tmp.Breakpoints_.size();

  			   //insert the new breakpoint
  			   if (tmp.Breakpoints_.begin()->first<leftBreak){

  				pair<map<double, double>::iterator,bool> breakinsertion=Breakpoints_.insert(pair<double, double> (leftBreak, 0.0));
  				itleft=breakinsertion.first;
  			   if (breakinsertion.second){
  				   --itleft; double u=itleft->second; ++itleft;
  				  // cout<<(*ittmp).second<<"left B"<<leftBreak<<endl;
  				   (*itleft).second = u;
  			   }
  			   itb=Breakpoints_.begin();
  			   Breakpoints_.erase(itb,itleft);
  			   }

  			   if (tmp.Breakpoints_.rbegin()->first>rightBreak){
  			   //initialsize=Breakpoints_.size();
  			   itright=Breakpoints_.insert(pair<double, double> (rightBreak, 0.0)).first;
  			   itright++;
  			   itb=Breakpoints_.end();
  			   if (itright!=itb) Breakpoints_.erase(itright,itb);
  			   map<double, double>::reverse_iterator irev=Breakpoints_.rbegin();
  			   irev->second=numeric_limits<double>::infinity();
  			   }
  		   }
  	   }
  	  // cout<<"out : "<<endl;
  	  // this->print();
     }

    void Sumf(cplfunction const & cplfunction1){
   	  // cout << __FUNCTION__ <<endl;
   	  // this->print();
   	  // cplfunction1.print();
   	  cplfunction tmp(*this),tmp1=cplfunction1;

   	  (*this).Squeeze(tmp1.Breakpoints_.begin()->first,tmp1.Breakpoints_.rbegin()->first);

      if (tmp1.Breakpoints_.size()<=2){
   		  if (tmp1.Breakpoints_.size()==1){
          if (tmp1.Breakpoints_.begin()->first!=Breakpoints_.begin()->first){
   				  //cout<<"in Sumf"<<endl;
   				  throw emptyfunc();
          }
        }else{
          FirstBreakVal_=FirstBreakVal_+tmp1.FirstBreakVal_;
   			  map<double,double>::iterator it=Breakpoints_.begin();
   			  double a;
   			  while (it != Breakpoints_.end()){
   				  a=it->second;
   				  (*it).second=a+tmp1.Breakpoints_.begin()->second;
   				  ++it;
   			  }
   		  }
   	  }else{
        map<double,double>::iterator it=tmp1.Breakpoints_.begin();
   		  ++it;
   		  map<double, double>::iterator itplus=it,itplus2;
   		  ++it;itplus2=it;
   		  it=tmp1.Breakpoints_.begin();
   		  //++itplus;++it;
        (*this).AddSimple(it->second,itplus->second,tmp1.FirstBreakVal_,itplus->first);
   	    ++itplus;++it;++itplus2;
   	    while (itplus2!=tmp1.Breakpoints_.end()){
   	      (*this).AddSimple(0.0,itplus->second-it->second,0.0,itplus->first);
   	      ++itplus;++it;++itplus2;
   	    }
      }
   	  //  cout<<"out :"<<endl;
   	  //  this->print();
    }

    void Swap(double y) {
 	   //cout << __FUNCTION__ << " " << y << endl;
 	   //this->print();
 	   if(Breakpoints_.size() < 1)
 		   throw emptyfunc();
 	   map<double,double>::reverse_iterator rit;
 	   cplfunction tmp(*this);
 	   Breakpoints_.clear();
 	   rit = tmp.Breakpoints_.rbegin();
 	   double last_first = rit->first;
 	   ++rit;
 	   while(rit != tmp.Breakpoints_.rend()){
 		   Breakpoints_[y-last_first] = -(rit->second);
 		   last_first = rit->first;
 		   ++rit;
 	   }
 	   Breakpoints_[y-last_first] = numeric_limits<double>::infinity();
 	   //this->print();
 	   //return(*this);
    }

 };// end of class cplfunction definition



#endif /* CPLFUNCTION_HPP_ */
