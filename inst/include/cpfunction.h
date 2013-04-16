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

// Some usefull functions applied to this class
cplfunction Sum(cplfunction const & cplfunction1,cplfunction const & cplfunction2);
cplfunction InfConv(cplfunction const & cplFunction1,cplfunction const & cplFunction2);
cplfunction InfConfFunct(cplfunction const & cplFunction1,cplfunction const & cplFunction2,double y );


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
  void SerialPush_3Breaks_Functions(Rcpp::NumericVector S1,Rcpp::NumericVector S2, Rcpp::NumericVector B0,Rcpp::NumericVector B1,Rcpp::NumericVector B2){
  int length=S1.size();
  Rcpp::NumericVector Slopes(2); 
  Rcpp::NumericVector BreakPoints(3);
  for (int compteur=0; compteur<length; compteur++){
    Slopes[0]=S1[compteur];Slopes[1]=S2[compteur];
    BreakPoints[0]=B0[compteur];BreakPoints[1]=B1[compteur];BreakPoints[2]=B2[compteur];
    //vectorofcplfunctions_.push_back(cplfunction(Slopes,BreakPoints,0));
    MycplfunctionList_.push_back(cplfunction(Slopes,BreakPoints,0));
  } 

  };

  
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
    		  cplfunction tmpfunc3 = Sum(tmpfunc,tmpfunc2);
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


class  cpqfunction {
    /* private fields */

    public:
    map<double,pair<double,double> > Breakpoints_; // breakpoints, where we have a polynom here polynom is 1/2 ax^2+bx+c a=.first b=.second
    double FirstBreakVal_; // firstbreakval

    ~cpqfunction(){
      Breakpoints_.clear();
    };
  
    cpqfunction() : Breakpoints_(), FirstBreakVal_(0){}

    cpqfunction(int NbCoefficients, pair<double,double> * Coefficients, double * BreakPoints,double FirstBreakVal) {
		  create_cpqfunction(NbCoefficients,Coefficients,BreakPoints,FirstBreakVal);
	  };

    void create_cpqfunction(int NbCoefficients, pair<double,double> * Coefficients, double * BreakPoints,double FirstBreakVal) {
  	  for (int i=0; i<NbCoefficients; i++){
  	    Breakpoints_[BreakPoints[i]]=Coefficients[i];
  	  }
  	  Breakpoints_[BreakPoints[NbCoefficients]]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
  	  FirstBreakVal_= FirstBreakVal;
    };

    cpqfunction(pair<double,double> * Coefficients,double val){
  	  // This function constructs a simple quadratic function, no breaks.
  	  int NbCoefficients=1;
  	  double BreakPoints[1]={-numeric_limits<double>::infinity()};
      create_cpqfunction(NbCoefficients,Coefficients,BreakPoints,val);
    }

    cpqfunction(Rcpp::NumericVector Slopes0,Rcpp::NumericVector Slopes1, Rcpp::NumericVector BreakPoints,double FirstBreakVal){
  	int NbSlopes=  Slopes1.size();
		if (NbSlopes+1==BreakPoints.size()){
			if (isincreasing(Slopes0)&&isincreasing(Slopes1)){
				if (isincreasing(BreakPoints)){
					for (int i=0; i<NbSlopes; i++){
              if (Slopes0[i]<=Slopes1[i]){
                Breakpoints_[BreakPoints[i]]=Slopes2Coeffs(Slopes0[i],Slopes1[i]);
              }else{
                Rprintf( "Error: non increasing Slopes" ) ;
				        throw nonincreasingslopes() ;
              }				   
					  }
					  Breakpoints_[BreakPoints[NbSlopes]]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
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
    
    cpqfunction(cpqfunction const & x) : Breakpoints_(x.Breakpoints_), FirstBreakVal_(x.FirstBreakVal_) {
	}
    
    cpqfunction* clone() const {
        return new cpqfunction(*this) ;
  }
    
    cpqfunction(double uniquebreak,pair<double,double> * Coefficients,double val){
	   // This function constructs a simple quadratic function bounded from below breaks.
	   int NbCoefficients=1;
	   double BreakPoints [1]={uniquebreak};
	   create_cpqfunction(NbCoefficients,Coefficients,BreakPoints,val);
   };

    cpqfunction(double * twobreaks,pair<double,double> Coefficient, double val){
	    // Simple quadratic function bounded from above and below
	    int NbCoefficients=1;
	    pair<double,double> Coefficients [2];
	    Coefficients[0]=Coefficient; Coefficients[1]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
	    create_cpqfunction(NbCoefficients,Coefficients,twobreaks,val);
    };
    
    //stack exceptions, implement std::exception
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
    
    Rcpp::List get_BreakPoints(){
      std::vector<double> Breakpoints;
    	std::vector<double> Slopes1;
      std::vector<double> Slopes0;
	 	 	map<double,pair<double,double> >::iterator it=Breakpoints_.begin();
	 	 	int nbSlopes=0,compteur=0;
	 	 	while(it != Breakpoints_.end()) {it++; nbSlopes++;}
	 	 	nbSlopes--;
  			it=Breakpoints_.begin();
  			compteur=0;
  			while(it != Breakpoints_.end()) {
  				Breakpoints.push_back( it->first );
  				if (compteur != (nbSlopes+1)){
  					Slopes0.push_back( it->second.second );
            Slopes1.push_back( it->second.first+it->second.second);
  				}
  				it++; compteur++;
  			}

  			return Rcpp::List::create(
				Rcpp::Named("Breakpoints") = Rcpp::wrap(Breakpoints),
				Rcpp::Named("Slopes0") = Rcpp::wrap(Slopes0),
        Rcpp::Named("Slopes1") = Rcpp::wrap(Slopes1));
  	}

    cpqfunction & operator = (cpqfunction const & s) {
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

    void AddSimple(double const & breakpoint,pair<double,double> const & left,pair<double,double> const & right,double const & val){
     map<double, pair<double,double> >::iterator i = Breakpoints_.begin();
     FirstBreakVal_=FirstBreakVal_+val;
	   if ((left.first==right.first)&&(left.second==right.second)){
		   while(i != Breakpoints_.end()) {
			   (*i).second.first=i->second.first+left.first;
			   (*i).second.second=i->second.second+left.second;
		   	   ++i;
		   }
	   	   FirstBreakVal_=FirstBreakVal_+val;
	   }else{
           if (breakpoint<=(*Breakpoints_.begin()).first){
               //BreakPoint is out of the domain, on the left
    		   while(i != Breakpoints_.end()) {
    			   (*i).second.first=i->second.first+right.first;
    			   (*i).second.second=i->second.second+right.second;
    		   	   ++i;
    		   }
           }
           else{
               if (breakpoint>=(*Breakpoints_.rbegin()).first){
          		   while(i != Breakpoints_.end()) {
        			   (*i).second.first=i->second.first+left.first;
        			   (*i).second.second=i->second.second+left.second;
          			   ++i;
          		   }
               }else{/*here the new breakpoint is inside the domain of this and
               the rightslope and left Coefficients are different*/
        		   map<double, pair<double,double> >::iterator it,ittmp;
        		   unsigned int initialsize=Breakpoints_.size();
        		   //insert the new breakpoint

          		   it=Breakpoints_.insert(pair<double, pair<double,double> > (breakpoint, pair<double,double>(0.0,0.0))).first;
          		   it--; ittmp=it; it++;
          		   if (Breakpoints_.size()!=initialsize){
      //    			   cout<<(*it).first<<","<<(*it).second<<endl;
          			 //map<double, pair<double,double> >::iterator it2=Breakpoints_.begin();
          			(*it).second = (*ittmp).second;
          		   }

          		   map<double, pair<double,double> >::iterator i = Breakpoints_.begin();
          		   while(i != it) {
        			   (*i).second.first=i->second.first+left.first;
        			   (*i).second.second=i->second.second+left.second;
          			   ++i;
          		   }
          		   while(i != Breakpoints_.end()) {
        			   (*i).second.first=i->second.first+right.first;
        			   (*i).second.second=i->second.second+right.second;
          			   ++i;
          		   }
	           }
	   	   }
	   }

   };

    void Etoile(){
	   int compteur=0;
	   double tmpslope;
	   pair<double,double>  pastCoefficients;
	   bool IsBreak; // true if current point comes from a slope discontinuity at a break point

	   cpqfunction tmp(*this);
	   Breakpoints_.clear();

	   map<double,pair<double,double> >::iterator it=tmp.Breakpoints_.begin();
	   double firstbreak=it->first;
	   map<double,pair<double,double> >::reverse_iterator rit=tmp.Breakpoints_.rbegin();
	   rit=tmp.Breakpoints_.rbegin() ;
	   double lastbreak=rit->first;
	   int NbCoefficients=2*tmp.Breakpoints_.size();

	   if ((firstbreak==-numeric_limits<double>::infinity())){
		   IsBreak=false; NbCoefficients--;
	   }else{
		   IsBreak=true;
		   pastCoefficients=pair<double,double>(-numeric_limits<double>::infinity(),0);
	   }

	   if (lastbreak==numeric_limits<double>::infinity()){
		   bool lastBreak=false; NbCoefficients--;
	   }else{
		   bool lastBreak=true;
	   }

	   it=tmp.Breakpoints_.begin();
	   pair<double,double> * newCoefficients; newCoefficients= new pair<double,double> [NbCoefficients-1];
	   double * newBreak; newBreak= new double [NbCoefficients];
	   bool remove [NbCoefficients];

	   while(compteur != NbCoefficients) {
		   if (IsBreak){
			   tmpslope=getSlope(pastCoefficients,it->first);
			   if (tmpslope!=getSlope(it->second,it->first)){
				   remove[compteur]=false;
				   newBreak[compteur]=tmpslope;
				   newCoefficients[compteur]=pair<double,double>(it->first,0);
			   }else{
				   remove[compteur]=true;
			   }
			   IsBreak=false;
		   }else{
			   // cas polynome
			   pastCoefficients=pair<double,double>(it->second.first,it->second.second);
			   if (pastCoefficients.second==0){
				   //Linear polynom first empty
				   remove[compteur]=true;
			   }else{
				   remove[compteur]=false;
				   // f_i'(x_i)
				   newBreak[compteur]=getSlope(it->second,it->first);
				   newCoefficients[compteur]=pair<double,double>(-pastCoefficients.first/pastCoefficients.second,1/pastCoefficients.second);
			   }
			   IsBreak=true;
			   ++it;
		   }
	   	   compteur++;
	   }

	   if (NbCoefficients==0){
		   Breakpoints_[newBreak[NbCoefficients]]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
		   FirstBreakVal_= tmp.FirstBreakVal_;
	   }else{
		   for (int i=0; i<NbCoefficients; i++){
			   if (!remove[i]){
				   Breakpoints_[newBreak[i]]=newCoefficients[i];
			   }
		   }
		   Breakpoints_[newBreak[NbCoefficients]]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
		   FirstBreakVal_= tmp.FirstBreakVal_;
	   }

	   	delete [] newCoefficients;
	   	delete [] newBreak;
	   	//return(*this);
   }

    bool eq(cpqfunction  const & cpqfunction1){
	   if (FirstBreakVal_!=cpqfunction1.FirstBreakVal_){
		   return(false);
	   }
	   if (Breakpoints_.size()!=cpqfunction1.Breakpoints_.size()){
		   return(false);
	   }else{
		   map<double, pair<double,double> > mybreak=Breakpoints_;
  		   map<double, pair<double,double> >::iterator i = Breakpoints_.begin(),i2=mybreak.begin();
  		   while(i != Breakpoints_.end()) {
  			   if (i->first==i2->first&&i->second.first==i2->second.first&&i->second.second==i2->second.second){
  				 ++i;++i2;
  			   }else{
  				   return(false);
  			   }
  		   }
  		   return(true);
	   }
   }

    double Argmin(){
	  // cout << __FUNCTION__ << endl;
	   //this->print();
	   double res;
	   cpqfunction tmp(*this);
	   int NbCoefficients=tmp.Breakpoints_.size()-1;
	   if (NbCoefficients<2){
		   if (NbCoefficients==1){
		       if (getSlope(tmp.Breakpoints_.rbegin()->second,tmp.Breakpoints_.rbegin()->first)<=0){
		    	  res =tmp.Breakpoints_.rbegin()->first;
		       }else{
		    	   if (getSlope(tmp.Breakpoints_.begin()->second,tmp.Breakpoints_.begin()->first)>0){
		    		   res =tmp.Breakpoints_.begin()->first;
		    	   }else{// here f'(0)<0 and f'(1)>0, in particular f'(0)!=f'(1).
		    		   res=getXetoile(tmp.Breakpoints_.begin()->second);
		    	   }
		       }
		   }else{
			   if (NbCoefficients==0){
				   res =tmp.Breakpoints_.begin()->first;
			   }else{
				   cout<<"NbCoefficients="<<NbCoefficients<<endl;
				   throw emptyfunc();
			   }
		   }
	   }else{
	       if (getSlope(tmp.Breakpoints_.begin()->second,tmp.Breakpoints_.begin()->first)>0){
	    	   res =tmp.Breakpoints_.begin()->first;
	       }else{
      		map<double, pair<double,double> >::iterator i = tmp.Breakpoints_.begin();
			if (i->second.first==i->second.second){
				res=i->first;
			}else{
				res=getXetoile(i->second);
			}

      		++i;
      		   while(i != tmp.Breakpoints_.end()) {
      			   if (res>i->first){
      				   res=i->first;
      			   }
         			if (getSlope(i->second,i->first)>0){
         				break;
         			}else{
						if (i->second.first==i->second.second){
							res=i->first;
						}else{
							res=getXetoile(i->second);
						}
         			}
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
	   cpqfunction tmp(*this);

	   if (tmp.Breakpoints_.size()<1 ||leftBreak>=rightBreak ||tmp.Breakpoints_.begin()->first>=rightBreak ||tmp.Breakpoints_.rbegin()->first<=leftBreak){
		   if (tmp.Breakpoints_.begin()->first==rightBreak){
			   Breakpoints_.clear();
			   Breakpoints_[tmp.Breakpoints_.begin()->first]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
		   }else{
			   if (tmp.Breakpoints_.rbegin()->first==leftBreak){
				   Breakpoints_.clear();
				   Breakpoints_[tmp.Breakpoints_.rbegin()->first]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
			   }else{
				   cout<<"in Squeeze"<<endl;
				   throw emptyfunc();
			   }
		   }
	   }else{
		   if (tmp.Breakpoints_.size()==1){
			   Breakpoints_.clear();
			   Breakpoints_[tmp.Breakpoints_.rbegin()->first]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
		   }else{
	   		   map<double, pair<double,double> >::iterator itleft,itright,itb;
			   unsigned int initialsize=tmp.Breakpoints_.size();

			   //insert the new breakpoint
			   if (tmp.Breakpoints_.begin()->first<leftBreak){

				pair<map<double, pair<double,double> >::iterator,bool> breakinsertion=Breakpoints_.insert(pair<double, pair<double,double> > (leftBreak, pair<double,double>(0.0,1.0)));
				itleft=breakinsertion.first;
			   if (breakinsertion.second){
				   --itleft; pair<double,double> u=itleft->second; ++itleft;
				  // cout<<(*ittmp).second<<"left B"<<leftBreak<<endl;
				   (*itleft).second = u;
			   }
			   itb=Breakpoints_.begin();
			   Breakpoints_.erase(itb,itleft);
			   }

			   if (tmp.Breakpoints_.rbegin()->first>rightBreak){
			   initialsize=Breakpoints_.size();
			   itright=Breakpoints_.insert(pair<double, pair<double,double> > (rightBreak, pair<double,double>(0.0,1.0))).first;
			   itright++;
			   itb=Breakpoints_.end();
			   if (itright!=itb) Breakpoints_.erase(itright,itb);
			   map<double, pair<double,double> >::reverse_iterator irev=Breakpoints_.rbegin();
			   irev->second=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
			   }
		   }
	   }
	  // cout<<"out : "<<endl;
	  // this->print();
   }

    void Sumf(cpqfunction  const &  cpqfunction1){
  	  // cout << __FUNCTION__ <<endl;
  	  // this->print();
  	  // cpqfunction1.print();
  	  cpqfunction tmp(*this),tmp1=cpqfunction1;
  
  	  (*this).Squeeze(tmp1.Breakpoints_.begin()->first , tmp1.Breakpoints_.rbegin()->first);
  
      if (tmp1.Breakpoints_.size()<=2){
        if (tmp1.Breakpoints_.size()==1){
  			  if (tmp1.Breakpoints_.begin()->first!=Breakpoints_.begin()->first){
  				  cout<<"in Sumf"<<endl;
  				  throw emptyfunc();
  			  }
        }else{
          //cout << tmp1.Breakpoints_.rbegin()->first;
          FirstBreakVal_=FirstBreakVal_+tmp1.FirstBreakVal_;
  			  map<double,pair<double,double> >::iterator it=Breakpoints_.begin();
  			  double a,b;
  			  while (it != Breakpoints_.end()){
  				  a=it->second.first; b=it->second.second;
  				  (*it).second.first=a+tmp1.Breakpoints_.begin()->second.first;
  				  (*it).second.second=b+tmp1.Breakpoints_.begin()->second.second;
  				  ++it;
  			  }
  		  }
      }else{
  		  double a,b;
  		  map<double,pair<double,double> >::iterator it=tmp1.Breakpoints_.begin();
  		  ++it;
  		  map<double, pair<double,double> >::iterator itplus=it,itplus2;
  		  pair<double,double> ab,zero=pair<double,double>(0.0,0.0);
  		  ++it;itplus2=it;
  		  it=tmp1.Breakpoints_.begin();
  		  //++itplus;++it;
  		  // A FAIRE !! !
  		  //double const & breakpoint,pair<double,double> const & left,pair<double,double> const & right,double const & val
  	    (*this).AddSimple(itplus->first,it->second,itplus->second,tmp1.FirstBreakVal_);
  	    ++itplus;++it;++itplus2;
  	    while (itplus2!=tmp1.Breakpoints_.end()){
  	      a=itplus->second.first-it->second.first;
  	      b=itplus->second.second-it->second.second;
  	      ab=pair<double,double>(a,b);
  	      (*this).AddSimple(itplus->first,zero,ab,0.0);
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
	   map<double,pair<double,double> >::reverse_iterator rit;
	   cpqfunction tmp(*this);
	   Breakpoints_.clear();
	   rit = tmp.Breakpoints_.rbegin();
	   double last_first = rit->first;
	   ++rit;
	   while(rit != tmp.Breakpoints_.rend()){
		   Breakpoints_[y-last_first] = pair<double,double>(-(rit->second.first),-(rit->second.second));
		   last_first = rit->first;
		   ++rit;
	   }
	   Breakpoints_[y-last_first] = pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
	   //this->print();
	   //return(*this);
   }

};// end of class cpqfunction definition

cpqfunction Sumq(cpqfunction const & cpqfunction1,cpqfunction const & cpqfunction2);