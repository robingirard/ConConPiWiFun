/*
 * cpqfunction.hpp
 *
 *  Created on: 16 avr. 2013
 *      Author: robin
 */

#ifndef CPQFUNCTION_HPP_
#define CPQFUNCTION_HPP_

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
	    std::pair<double,double>  pastCoefficients, Coef;
	    bool IsBreak ; // true if current point comes from a slope discontinuity at a break point
                     // true if  there is a slope discontinuity at current breakpoint?

	    cpqfunction tmp(*this);
	    Breakpoints_.clear();

	    map<double,pair<double,double> >::iterator it=tmp.Breakpoints_.begin();
	    map<double,pair<double,double> >::iterator itobj=Breakpoints_.begin();
	    double firstbreak=it->first;
	    map<double,pair<double,double> >::reverse_iterator rit=tmp.Breakpoints_.rbegin();
	    double lastbreak=rit->first;

	    int NbCoefficients=2*tmp.Breakpoints_.size();

	    //cout << NbCoefficients << endl ;

	    //si le premier breakpoint vaut -Inf
	    if ((firstbreak==-numeric_limits<double>::infinity()))
	    {
		    IsBreak=false;
		    NbCoefficients--;
	    }else
	    {
		    IsBreak=true;
		    //pastCoefficients=pair<double,double>(-numeric_limits<double>::infinity(),0);
		    pastCoefficients=pair<double,double>(numeric_limits<double>::infinity(),0);
	    }

	    //si le dernier breakpoint vaut +Inf
	    if (lastbreak==numeric_limits<double>::infinity())
	    {
		    //bool IsBreakLast=false;
	    	NbCoefficients--;
	    }/*else{bool IsBreakLast=true;}*/

	    it=tmp.Breakpoints_.begin();

	    vector<pair<double,double> > newCoefficients(NbCoefficients-1);
	    vector<double> newBreak(NbCoefficients);
	    vector<bool> remove(NbCoefficients);
	    //cout << NbCoefficients << endl ;

	    while(compteur != NbCoefficients) {
        //si IsBreak
		    if (IsBreak)
		    {
			  tmpslope=getSlope(pastCoefficients,it->first);
			  //Rcout << "it->fist : " << it->first << endl;
			  //cout << "getslope : " << getSlope(it->second,it->first) <<endl;
		      if (tmpslope!=getSlope(it->second,it->first))
		      {
			      remove[compteur]=false;
			      newBreak[compteur]=tmpslope;
			     // Rcout << "newBreak1 : " << newBreak[compteur] <<endl;
			     // Rcout << "newCoef   : zéro et " << it->first << endl ;
			      //newCoefficients[compteur]=pair<double,double>(it->first,0);
			      newCoefficients[compteur]=pair<double,double>(0,it->first);
		      }else
		      {
			      remove[compteur]=true;
		      }
			    IsBreak=false;
		    }else
		    {
			    pastCoefficients=pair<double,double>(it->second.first,it->second.second);
			   // Rcout << "pastCoefficients.first  : " << pastCoefficients.first  << endl;
			    //Rcout << "pastCoefficients.second : " << pastCoefficients.second  << endl;
			    //if (pastCoefficients.second==0)
			    // cas lineaire
			    if (pastCoefficients.first==0)
			    {
				    //Linear polynom first empty
				    remove[compteur]=true;
				    //cas quadratique
			    }else{
				    remove[compteur]=false;
				    // f_i'(x_i)
				    newBreak[compteur]=getSlope(it->second,it->first);
				    //Rcout << "newBreak2 : " << newBreak[compteur] <<endl;
				    //newCoefficients[compteur]=pair<double,double>(-pastCoefficients.first/pastCoefficients.second,1/pastCoefficients.second);
					if (pastCoefficients.first==numeric_limits<double>::infinity()&&pastCoefficients.second==numeric_limits<double>::infinity()){
					  newCoefficients[compteur]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
					}else{
					  newCoefficients[compteur]=pair<double,double>(1/pastCoefficients.first,-pastCoefficients.second/pastCoefficients.first);
					}
					//Rcout << "newCoef a : " << newCoefficients[compteur].first << endl ;
					//Rcout << "newCoef b : " << newCoefficients[compteur].second << endl ;
			    }
			    IsBreak=true;
			    ++it;
		    }
	   	  compteur++;
	   	  //Rcout << "compteur : " << compteur << endl ;
	    }
      //cout << "BP.size : " << newBreak.size() << endl ;

	    if (NbCoefficients==0)
	    {
	    	Rcout << "PASSSAGE" << endl;
	    	Breakpoints_[newBreak[NbCoefficients]]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());
	    	FirstBreakVal_= tmp.FirstBreakVal_;
	    }else{
		    for (int i=0; i<NbCoefficients; i++)
		    {
			    if (!remove[i])
			    {
					Rcout << i << endl;
					Breakpoints_[newBreak[i]]=newCoefficients[i];
					Rcout << "newBreak : " << newBreak[i] << endl;
					Rcout << "newcoef1 : " << newCoefficients[i].first << endl;
					Rcout << "newcoef2 : " << newCoefficients[i].second << endl;
			    }
		    }

			Rcout << "Breakpoints_.size() : " << Breakpoints_.size() << endl;
				//Breakpoints_[newBreak[NbCoefficients]]=pair<double,double>(numeric_limits<double>::infinity(),numeric_limits<double>::infinity());

			it=tmp.Breakpoints_.begin();
			itobj=Breakpoints_.begin();
			if (itobj->first==-numeric_limits<double>::infinity()){itobj++;}
			if (it->first==-numeric_limits<double>::infinity()){it++;}

			Coef=pair<double,double>(itobj->second.first,itobj->second.second);

			double c_init=tmp.FirstBreakVal_-getVal(it->second,it->first);
			FirstBreakVal_= getVal(Coef,itobj->first)-c_init+(it->second.second)*(it->second.second)/(2*it->second.first);

			Rcout << "firstbreakval " << itobj->first << endl;
			Rcout << "c_init : " << c_init << endl;
			Rcout << "Coef.first  : " << Coef.first  << endl;
			Rcout << "Coef.second : " << Coef.second  << endl;
			Rcout << "Coef.first  : " << it->second.first  << endl;
			Rcout << "Coef.second : " << it->second.second  << endl;
	    }

	   	//delete [] newCoefficients;
	   	//delete [] newBreak;
	    //return(*this);*/
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



#endif /* CPQFUNCTION_HPP_ */
