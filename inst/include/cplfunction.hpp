/*
 * cplfunction.hpp
 *
 *  Created on: 27 avr. 2013
 *      Author: robin
 */

#ifndef cplfunction_HPP_
#define cplfunction_HPP_


/*
 * cplfunction.hpp
 *
 *  Created on: 16 avr. 2013
 *      Author: robin
 */

class cplfunction {
	// this class implements the convex continuous piecewise functions with a map
	// this allows nlog(n) sum of two such function
	// FirstSlopeVal_ is the absolute slope associated to the first breakpoint
	// Breakpoints_ is
	//
	//

    public:
    map<double,double> Breakpoints_; // breakpoints
    double FirstBreakVal_; // firstbreakval
    double FirstSlopeVal_ ;

    ~cplfunction(){
      Breakpoints_.clear();
    };

    cplfunction()
    	: Breakpoints_(),FirstBreakVal_(0),
    	  FirstSlopeVal_(-numeric_limits<double>::infinity()){};

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
  		if (NbSlopes==BreakPoints.size()){
  			if (isincreasing(Slopes)){
  				if (isincreasing(BreakPoints)){
  					FirstSlopeVal_=Slopes[0];
  					Breakpoints_[BreakPoints[0]]=0;
  					for (int i=1; i<NbSlopes; i++)
  					{
  						Breakpoints_[BreakPoints[i]]=Slopes[i]-Slopes[i-1];
  					}
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
  			Rprintf( "Error: number of Slopes must be number of breaks+1  " ) ;
  			throw nonincreasingslopes() ;
  		}
	  };

	  cplfunction(cplfunction const & x) :
		  Breakpoints_(x.Breakpoints_),FirstBreakVal_(x.FirstBreakVal_),
		  FirstSlopeVal_(x.FirstSlopeVal_){};

    cplfunction* clone() const {
        return new cplfunction(*this) ;
    };

    cplfunction(double uniquebreak,double val)//a single point has 1 break and infinite FirstSlopeVal_
    	:Breakpoints_(),FirstBreakVal_(val),FirstSlopeVal_(numeric_limits<double>::infinity())
    {
    	Breakpoints_[uniquebreak]=0;
    };

    //a half line is just 1 point and a slope
    cplfunction(double uniquebreak,double val,double Slope1)
    	:Breakpoints_(),
    	 FirstBreakVal_(val),FirstSlopeVal_(Slope1)
    {
    	Breakpoints_[uniquebreak]=0;
    };


    //a "V" function is 2 points and 2 slopes the first point being infinity
    cplfunction(double uniquebreak,double val,double Slope1, double Slope2)
		:Breakpoints_(),
		 FirstBreakVal_(val),FirstSlopeVal_(Slope1)
    {
    	Breakpoints_[-numeric_limits<double>::infinity()]=0;
    	Breakpoints_[uniquebreak]=Slope2;
    };

    Rcpp::List get_BreakPoints()
    {
    	int nbBreaks=Breakpoints_.size();
    	std::vector<double> Breakpoints(nbBreaks);
  		std::vector<double> Slopes(nbBreaks);
  		int compteur=0;
  		map<double,double>::iterator Breakpoints_it=Breakpoints_.begin();
  		while(Breakpoints_it != Breakpoints_.end())
  		{
  			Breakpoints[compteur]=Breakpoints_it->first;
  			if (compteur==0)
  			{
  				Slopes[compteur]=FirstSlopeVal_;
  			}else
  			{
				Slopes[compteur]=Slopes[compteur-1]+Breakpoints_it->second;
  			}
  			Breakpoints_it++; compteur++;
  		}

		return Rcpp::List::create(
			Rcpp::Named("Breakpoints") = Breakpoints,
			Rcpp::Named("Slopes") = Slopes);
  	}

    void print()
     {
     	int nbBreaks=Breakpoints_.size();
     	std::vector<double> Breakpoints(nbBreaks);
   		std::vector<double> Slopes(nbBreaks);
   		int compteur=0;
   		map<double,double>::iterator Breakpoints_it=Breakpoints_.begin();
   		if (Breakpoints_it->second!=0)
   		{
   			Rcout<<"Warning first Slope diff non null. ";
   		}

   		while(Breakpoints_it != Breakpoints_.end())
   		{
   			Rcout<<"|"<<Breakpoints_it->first<<"|";
   			if (compteur==0)
   			{
   				Slopes[compteur]=FirstSlopeVal_;
   				Rcout<<"__"<<FirstSlopeVal_<<"__";
   			}else
   			{
   				Slopes[compteur]=Slopes[compteur-1]+Breakpoints_it->second;
   				Rcout<<"__"<<Slopes[compteur]<<"__";
   			}
   			Breakpoints_it++; compteur++;
   		}
   		Rcout<<endl;
   	}



/*
    cplfunction(double * twobreaks,double slope, double val){
 	   int NbSlopes=1;
 	   double Slopes [2]={slope, numeric_limits<double>::infinity()};
 	   create_cplfunction(NbSlopes,Slopes,twobreaks,val);
    }
*/
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
      FirstSlopeVal_=s.FirstSlopeVal_;
     }
     return *this;
    }

    void AddSimple(double leftslope, double rightslope, double val, double breakpoint)
    {
    	//Rcout << __FUNCTION__ << "("<<leftslope<< ","<<rightslope<<","<<breakpoint<<")"<<"in "<<endl;
    	//this->print();
    	map<double, double>::iterator i = Breakpoints_.begin();
    	FirstBreakVal_=FirstBreakVal_+val;

		if (breakpoint<=Breakpoints_.begin()->first)
		{//BreakPoint is out of the domain, on the left
			  FirstSlopeVal_=FirstSlopeVal_+rightslope;
		}else
		{
		  if (breakpoint>=Breakpoints_.rbegin()->first && Breakpoints_.rbegin()->second== numeric_limits<double>::infinity()){
			  FirstSlopeVal_=FirstSlopeVal_+leftslope;
		  }else
		  {
			/*here the new breakpoint is inside the domain of this*/
			FirstSlopeVal_=FirstSlopeVal_+leftslope;
			pair<map<double, double>::iterator,bool> tmp_insert=Breakpoints_.insert(pair<double, double> (breakpoint, rightslope-leftslope));
			if (!tmp_insert.second)
			{//insert the new breakpoint if it does not exist and if it exists increment :
				double tmpval=tmp_insert.first->second;
				(*tmp_insert.first).second=tmpval+rightslope-leftslope;
			}
		  }
		}
		//Rcout << __FUNCTION__ << "out "<<endl;
		//this->print();
    }

    bool eq(cplfunction  const & cplfunction1){
 	   if (FirstBreakVal_!=cplfunction1.FirstBreakVal_){
 		   return(false);
 	   }
 	   if (FirstSlopeVal_!=cplfunction1.FirstSlopeVal_){
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

    bool is_last_infinity()
    {
    	return((Breakpoints_.rbegin()->second!=numeric_limits<double>::infinity()) &&
    			FirstBreakVal_!=numeric_limits<double>::infinity());
    }

    bool is_a_point()
    {
    	return(FirstSlopeVal_==numeric_limits<double>::infinity());
    }

    bool is_an_infinite_line()
    {
    	return(FirstSlopeVal_!=numeric_limits<double>::infinity() &&
    			Breakpoints_.size()==1 &&
    			(Breakpoints_.begin())->first==-numeric_limits<double>::infinity());
    }

    void Etoile()
    {
    	//Rcout << __FUNCTION__<< " in" <<endl;
    	//this->print();
		cplfunction tmp(*this);
		Breakpoints_.clear();
		bool done=false;

		if (tmp.is_a_point())
		{// a point get transformed into a line
			FirstSlopeVal_=(tmp.Breakpoints_.begin())->first;
			Breakpoints_[-numeric_limits<double>::infinity()]=0;
			done=true;
		}

		if (tmp.is_an_infinite_line())
		{// an infinite line get transformed into a point
			Breakpoints_[(tmp.Breakpoints_.begin())->second]=numeric_limits<double>::infinity();
			FirstSlopeVal_=0;
			done=true;
		}
		if (tmp.Breakpoints_.size()==1 && !done)
		{// only one breakpoint not a line not a point : this is a half line
		 // gives a half line with 2 breaks
			FirstSlopeVal_=(tmp.Breakpoints_.begin())->first;
			Breakpoints_[-numeric_limits<double>::infinity()]=0;
			Breakpoints_[tmp.FirstSlopeVal_]=numeric_limits<double>::infinity();
			done=true;
		}
		if (done)
		{
			// do nothing
		}
		else
		{
			map<double,double>::iterator it=tmp.Breakpoints_.begin();
			map<double,double>::iterator itplus1=tmp.Breakpoints_.begin(); ++itplus1;
			FirstBreakVal_=-tmp.FirstBreakVal_;
			double NewBreak,NewSlope,PastBreakPrec,NewBreakPrec;
			if ((it->first!=-numeric_limits<double>::infinity()))
			{// first break is not infinity : this will give a slope and a first break with inf
				FirstSlopeVal_=it->first;
				Breakpoints_[-numeric_limits<double>::infinity()]=0;
			}else
			{
				FirstSlopeVal_=itplus1->first;
			}
			PastBreakPrec=FirstSlopeVal_;
			NewBreakPrec=tmp.FirstSlopeVal_;

			while (itplus1!=tmp.Breakpoints_.end())
			{
				NewSlope=itplus1->first-PastBreakPrec;
				PastBreakPrec=itplus1->first;
				NewBreak=it->second+NewBreakPrec;
				NewBreakPrec=NewBreak;
				Breakpoints_[NewBreak]=NewSlope;
				++it; ++itplus1;
			}
			if (tmp.is_last_infinity())
			{
				NewBreak=it->second+NewBreakPrec;
				Breakpoints_[NewBreak]=numeric_limits<double>::infinity();
			}
		}
		//Rcout << __FUNCTION__<< " out" <<endl;
		//this->print();
    };

    double Argmin(){
 	 // Rcout << __FUNCTION__ << endl;
 	 // this->print();
 	   double res;
 	   cplfunction tmp(*this);
 	   double precslope=FirstSlopeVal_;
 	   int NbSlopes=tmp.Breakpoints_.size();

 	   if (is_a_point()){res=tmp.Breakpoints_.begin()->first;}
 	   if (is_an_infinite_line()){
 		   if (FirstSlopeVal_==0){res=0;}
 		   else if (FirstSlopeVal_<0){res=numeric_limits<double>::infinity();}
 		   else if (FirstSlopeVal_>0){res=-numeric_limits<double>::infinity();}
 	   }

	   if (precslope>0){

		   res =tmp.Breakpoints_.begin()->first;
	   }else{
		 map<double, double>::iterator i = tmp.Breakpoints_.begin();
		++i;
		   while(i != tmp.Breakpoints_.end()) {
			 res=i->first;
			precslope=precslope+i->second;
			if (precslope>0){ break;}
			++i;
		   }
		   if (precslope<0){res= numeric_limits<double>::infinity();}
	   }
	   return(res);
 	 // Rcout<<"res="<<res<<endl;
 	 //  return(res);
    };

    void Squeeze(double leftBreak,double rightBreak)
    {
     	//   Rcout << __FUNCTION__ << "("<<leftBreak<< ","<<rightBreak<<")"<<" in "<<endl;
     	  // Rcout<< "left : "<<leftBreak<<", right : "<<rightBreak<<endl;
     	  // Rcout<< "this left "<< Breakpoints_.begin()->first << "this right : "<<Breakpoints_.rbegin()->first<<endl;
     	  //Rcout<<  "this right slope : "<<Breakpoints_.rbegin()->second<<endl;

     	 //  this->print();
     	 //  Rcout<<"FirstSlopeVal_ : "<<FirstSlopeVal_<<endl;
    	// test for empty interval or empty intersection of function and interval

		if (  (leftBreak>=rightBreak) ||
				(Breakpoints_.rbegin()->first<leftBreak && Breakpoints_.rbegin()->second==numeric_limits<double>::infinity() ) ||
				Breakpoints_.begin()->first>rightBreak )
		{
			if (leftBreak>=rightBreak){Rcout<<"leftBreak>=rightBreak"<<endl;}
			Rcout<<"Empty function thrown in Squeeze"<<endl;
			throw emptyfunc();
		}

		if (is_a_point())
		{

		}else
		{

			/// taking care of left break
			map<double, double>::iterator it=Breakpoints_.begin();
			if (it->first<leftBreak)
			{// something will have to be cut on the left
				while (it!=Breakpoints_.end() && it->first<leftBreak )
				{
					FirstSlopeVal_=FirstSlopeVal_+it->second;
					++it;
				}
				if (it==Breakpoints_.end())
				{
					Breakpoints_.erase(Breakpoints_.begin(),it);
					Breakpoints_.insert(pair<double, double> (leftBreak, 0.0));
				}else
				{
					if (it!=Breakpoints_.begin()){
						Breakpoints_.erase(Breakpoints_.begin(),it);
					}

					if (Breakpoints_.begin()->first==leftBreak){
						FirstSlopeVal_=FirstSlopeVal_+Breakpoints_.begin()->second;
						Breakpoints_.begin()->second=0.0;
					}else{
						Breakpoints_.insert(pair<double, double> (leftBreak, 0.0));
					}
				}
			}

			// taking care of right break
			if (is_last_infinity())
			{
				if (rightBreak!=numeric_limits<double>::infinity())
				{// something has to be cut on the right
					pair<map<double, double>::iterator,bool> tmp_insert=Breakpoints_.insert(pair<double, double> (rightBreak, numeric_limits<double>::infinity()));
					map<double, double>::iterator tmp_insert_it=tmp_insert.first;
					++tmp_insert_it;
					if (tmp_insert_it!=Breakpoints_.end())
					{
						Breakpoints_.erase(tmp_insert_it,Breakpoints_.end());
					}
				}
			}else
			{
				if (rightBreak!=numeric_limits<double>::infinity() && Breakpoints_.rbegin()->first>rightBreak )
				{// something has to be cut on the right
					pair<map<double, double>::iterator,bool> tmp_insert=Breakpoints_.insert(pair<double, double> (rightBreak, numeric_limits<double>::infinity()));
					map<double, double>::iterator tmp_insert_it=tmp_insert.first;
					++tmp_insert_it;
					if (tmp_insert_it!=Breakpoints_.end())
					{
						Breakpoints_.erase(tmp_insert_it,Breakpoints_.end());
					}
				}
			}
		}

		//Rcout << __FUNCTION__ << "("<<leftBreak<< ","<<rightBreak<<")"<<" out "<<endl;
		//this->print();
		//Rcout << "FirstSlopeVal_: "<<FirstSlopeVal_<<endl;
     };

    void Sumf(cplfunction & cplfunction1){
   // Rcout << __FUNCTION__ <<" in "<<endl;
   //	   this->print();
   	   //cplfunction1.print();
   	  if (cplfunction1.is_last_infinity())
   	  {
   		(*this).Squeeze(cplfunction1.Breakpoints_.begin()->first,numeric_limits<double>::infinity());

   	  }else
   	  {
   		(*this).Squeeze(cplfunction1.Breakpoints_.begin()->first,cplfunction1.Breakpoints_.rbegin()->first);
   	  }

   	  if (	(cplfunction1.Breakpoints_.size()==1 && cplfunction1.Breakpoints_.rbegin()->second!=numeric_limits<double>::infinity()) ||
   			  (cplfunction1.Breakpoints_.size()==2 && cplfunction1.Breakpoints_.rbegin()->second==numeric_limits<double>::infinity()))
   	  {// linear function ... only one slope
   		  FirstSlopeVal_=FirstSlopeVal_+cplfunction1.FirstSlopeVal_;
   		  FirstBreakVal_=FirstBreakVal_+cplfunction1.FirstBreakVal_;
   	  }else
   	  {// at least two slopes with 2 breaks or more
		map<double,double>::const_iterator it=cplfunction1.Breakpoints_.begin();
		++it;
		map<double, double>::const_iterator itplus=it;
		it=cplfunction1.Breakpoints_.begin();
		(*this).AddSimple(cplfunction1.FirstSlopeVal_,
				itplus->second+cplfunction1.FirstSlopeVal_,
				cplfunction1.FirstBreakVal_,itplus->first);
		++itplus;++it;
		while (itplus!=cplfunction1.Breakpoints_.end())
		{// enter this loop if there are more than 2 slopes (3 breaks or more)...
			(*this).AddSimple(0.0,itplus->second,0.0,itplus->first);
			++itplus;++it;
		}
      }

   //	Rcout << __FUNCTION__ <<" out "<<endl;
   //	   this->print();
    };

    void Swap(double y)
    {
    //	Rcout << __FUNCTION__ << " " << y << " in "<< endl;
 	//   this->print();
 	   cplfunction tmp(*this);
 	   Breakpoints_.clear();
 	   map<double,double>::reverse_iterator rit = tmp.Breakpoints_.rbegin();

 	   if (tmp.is_a_point())
 	   {
 		  Breakpoints_[y-rit->first]=0;
 		  FirstSlopeVal_=numeric_limits<double>::infinity();
 	   }else
 	   {
 	 	   map<double,double>::reverse_iterator ritplus1 = tmp.Breakpoints_.rbegin();
 	 	   ++ritplus1;
 			double LastSlopeVal=0;
 			if (tmp.is_last_infinity())
 			{
 				Breakpoints_[-numeric_limits<double>::infinity()]=0;
 			}else
 			{
 				Breakpoints_[y-rit->first]=0; ++rit;++ritplus1;
 			}

 			while(ritplus1 != tmp.Breakpoints_.rend()){
				Breakpoints_[y-rit->first] = rit->second;
				LastSlopeVal=LastSlopeVal+(rit->second);
				++rit;++ritplus1;
 			}
 			if (rit->first!=-numeric_limits<double>::infinity())
 			{
 				Breakpoints_[y-rit->first] = numeric_limits<double>::infinity();
 			}
 			FirstSlopeVal_=-(FirstSlopeVal_+LastSlopeVal);
 	   }

 	 // Rcout << __FUNCTION__ << " " << y << " out "<< endl;
 	 //  this->print();
 	   //return(*this);
    };

 };// end of class cplfunction definition




#endif /* cplfunction_HPP_ */
