#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix iter_r1(Rcpp::DataFrame dat, int window){
  NumericVector r1 = dat["r1"];
  int n = r1.length();
  NumericMatrix r1_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r1_matrix(i,j) = r1[j];
      }else{
        r1_matrix(i,j) = 999;
      }
    }
  }
  return r1_matrix;
}


// [[Rcpp::export]]
NumericMatrix iter_r2(Rcpp::DataFrame dat, int window){
  NumericVector r2 = dat["r2"];
  int n = r2.length();
  NumericMatrix r2_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r2_matrix(i,j) = r2[j];
      }else{
        r2_matrix(i,j) = 999;
      }
    }
  }
  return r2_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r3(Rcpp::DataFrame dat, int window){
  NumericVector r3 = dat["r3"];
  int n = r3.length();
  NumericMatrix r3_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r3_matrix(i,j) = r3[j];
      }else{
        r3_matrix(i,j) = 999;
      }
    }
  }
  return r3_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r4(Rcpp::DataFrame dat, int window){
  NumericVector r4 = dat["r4"];
  int n = r4.length();
  NumericMatrix r4_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r4_matrix(i,j) = r4[j];
      }else{
        r4_matrix(i,j) = 999;
      }
    }
  }
  return r4_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r5(Rcpp::DataFrame dat, int window){
  NumericVector r5 = dat["r5"];
  int n = r5.length();
  NumericMatrix r5_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r5_matrix(i,j) = r5[j];
      }else{
        r5_matrix(i,j) = 999;
      }
    }
  }
  return r5_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r6(Rcpp::DataFrame dat, int window){
  NumericVector r6 = dat["r6"];
  int n = r6.length();
  NumericMatrix r6_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r6_matrix(i,j) = r6[j];
      }else{
        r6_matrix(i,j) = 999;
      }
    }
  }
  return r6_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r7(Rcpp::DataFrame dat, int window){
  NumericVector r7 = dat["r7"];
  int n = r7.length();
  NumericMatrix r7_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r7_matrix(i,j) = r7[j];
      }else{
        r7_matrix(i,j) = 999;
      }
    }
  }
  return r7_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r8(Rcpp::DataFrame dat, int window){
  NumericVector r8 = dat["r8"];
  int n = r8.length();
  NumericMatrix r8_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r8_matrix(i,j) = r8[j];
      }else{
        r8_matrix(i,j) = 999;
      }
    }
  }
  return r8_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r9(Rcpp::DataFrame dat, int window){
  NumericVector r9 = dat["r9"];
  int n = r9.length();
  NumericMatrix r9_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r9_matrix(i,j) = r9[j];
      }else{
        r9_matrix(i,j) = 999;
      }
    }
  }
  return r9_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r10(Rcpp::DataFrame dat, int window){
  NumericVector r10 = dat["r10"];
  int n = r10.length();
  NumericMatrix r10_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r10_matrix(i,j) = r10[j];
      }else{
        r10_matrix(i,j) = 999;
      }
    }
  }
  return r10_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r11(Rcpp::DataFrame dat, int window){
  NumericVector r11 = dat["r11"];
  int n = r11.length();
  NumericMatrix r11_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r11_matrix(i,j) = r11[j];
      }else{
        r11_matrix(i,j) = 999;
      }
    }
  }
  return r11_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r12(Rcpp::DataFrame dat, int window){
  NumericVector r12 = dat["r12"];
  int n = r12.length();
  NumericMatrix r12_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r12_matrix(i,j) = r12[j];
      }else{
        r12_matrix(i,j) = 999;
      }
    }
  }
  return r12_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r13(Rcpp::DataFrame dat, int window){
  NumericVector r13 = dat["r13"];
  int n = r13.length();
  NumericMatrix r13_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r13_matrix(i,j) = r13[j];
      }else{
        r13_matrix(i,j) = 999;
      }
    }
  }
  return r13_matrix;
}


// [[Rcpp::export]]
NumericMatrix iter_r13_lag(Rcpp::DataFrame dat, int window){
  NumericVector r13_lag = dat["r13_lag"];
  int n = r13_lag.length();
  NumericMatrix r13_lag_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r13_lag_matrix(i,j) = r13_lag[j];
      }else{
        r13_lag_matrix(i,j) = 999;
      }
    }
  }
  return r13_lag_matrix;
}


// [[Rcpp::export]]
NumericMatrix iter_r_on(Rcpp::DataFrame dat, int window){
  NumericVector r_on = dat["r_on"];
  int n = r_on.length();
  NumericMatrix r_on_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r_on_matrix(i,j) = r_on[j];
      }else{
        r_on_matrix(i,j) = 999;
      }
    }
  }
  return r_on_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_r_onfh(Rcpp::DataFrame dat, int window){
  NumericVector r_onfh = dat["r_onfh"];
  int n = r_onfh.length();
  NumericMatrix r_onfh_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        r_onfh_matrix(i,j) = r_onfh[j];
      }else{
        r_onfh_matrix(i,j) = 999;
      }
    }
  }
  return r_onfh_matrix;
}

// pca variables
// [[Rcpp::export]]
NumericMatrix iter_dim1(Rcpp::DataFrame dat, int window){
  NumericVector dim1 = dat["dim1"];
  int n = dim1.length();
  NumericMatrix dim1_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        dim1_matrix(i,j) = dim1[j];
      }else{
        dim1_matrix(i,j) = 999;
      }
    }
  }
  return dim1_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_dim2(Rcpp::DataFrame dat, int window){
  NumericVector dim2 = dat["dim2"];
  int n = dim2.length();
  NumericMatrix dim2_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        dim2_matrix(i,j) = dim2[j];
      }else{
        dim2_matrix(i,j) = 999;
      }
    }
  }
  return dim2_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_dim3(Rcpp::DataFrame dat, int window){
  NumericVector dim3 = dat["dim3"];
  int n = dim3.length();
  NumericMatrix dim3_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        dim3_matrix(i,j) = dim3[j];
      }else{
        dim3_matrix(i,j) = 999;
      }
    }
  }
  return dim3_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_dim4(Rcpp::DataFrame dat, int window){
  NumericVector dim4 = dat["dim4"];
  int n = dim4.length();
  NumericMatrix dim4_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        dim4_matrix(i,j) = dim4[j];
      }else{
        dim4_matrix(i,j) = 999;
      }
    }
  }
  return dim4_matrix;
}

// [[Rcpp::export]]
NumericMatrix iter_dim5(Rcpp::DataFrame dat, int window){
  NumericVector dim5 = dat["dim5"];
  int n = dim5.length();
  NumericMatrix dim5_matrix(n-window+1,n);
  for( int i=0; i< n-window+1;i++){
    for(int j=0;j<n; j++){
      if(j < i+window){
        dim5_matrix(i,j) = dim5[j];
      }else{
        dim5_matrix(i,j) = 999;
      }
    }
  }
  return dim5_matrix;
}

