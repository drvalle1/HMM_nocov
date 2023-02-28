#include <Rcpp.h>
#include <iostream>
#include <ctime>
#include <fstream>
using namespace Rcpp;

/***************************************************************************************************************************/
/*********************************                      UTILS          *****************************************************/
/***************************************************************************************************************************/

// This function makes multinomial draws
//Got this from https://stackoverflow.com/questions/24618370/using-rmultinom-with-rcpp
// [[Rcpp::export]]
IntegerVector rmultinom1(NumericVector probs, int size) {
  int k = probs.size();
  IntegerVector ans(k);
  rmultinom(size, probs.begin(), k, ans.begin());
  return(ans);
}

//' This function samples zs
// [[Rcpp::export]]
IntegerVector samplez(NumericMatrix Gamma, 
                      NumericMatrix ProbObs1, 
                      NumericMatrix ProbObs2, 
                      IntegerVector y1, IntegerVector y2,  
                      IntegerVector z,
                      int nobs, int nclust) {
  
  NumericVector probs(nclust);
  IntegerVector tmp(nclust);
  IntegerVector znew(nobs);
  znew=z;
  
  for(int i=0; i<nobs; i++){
    if (z[i]!=-100){
      probs=ProbObs1(_,y1[i])*ProbObs2(_,y2[i]);
      
      // if not first observation and if prior obs not missing
      if ((i!=0) & (z[i-1]!=-100)){
        probs=Gamma(z[i-1],_)*probs;  
      }
      // if not last observation and if posterior obs not missing
      if ((i!=(nobs-1)) & (z[i+1]!=-100)){
        probs=Gamma(_,z[i+1])*probs;  
      }

      //normalize and sample
      probs=probs/sum(probs);
      tmp=rmultinom1(probs,1);
      for(int j=0; j<nclust; j++){
        if (tmp[j]==1){
          znew[i]=j;
        }
      }
    }
  }
  return (znew);
}