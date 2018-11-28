#include "RcppArmadillo.h"
using namespace Rcpp;
#include <bkmrdlmheader.h>

// [[Rcpp::depends(RcppArmadillo)]]
//' Estimates the BKMR-DLM for a multiple time-varying predictor.
//'
//' @param yz a matrix that is cbind(y,Z) where Z is a matrix of covariates that does not include an intercept and y is the vector of outcomes.
//' @param Xlist a list of matrices each for a single exposure in time order.
//' @param b1 first parameter for prior on tau^{-2} in the text.
//' @param a1 first parameter for prior on sigma^{-2}.
//' @param a2 second parameter for prior on sigma^{-2}.
//' @param kappa scale parameter, rho/kappa~chisq(1).
//' @param n_inner number of MCMC iterations to run in the inner loop.  This is equivelent the the thinning number.  n_outer*n_inner iteraction will be run and n_outer iterations will be saved.
//' @param n_outer number of MCMC iteration in the outer loop.   The output for each is saved.
//' @param n_burn number of MCMC iteration to be discarded as burn-in.
//' @export
// [[Rcpp::export]]
List bkmrdlm_multi_shrink(const arma::mat& yz, 
               const Rcpp::List& Xlist, 
               const double& b1, 
               const double& a1, 
               const double& a2, 
               const double& kappa,
               const int& n_inner, 
               const int& n_outer, 
               const int& n_burn) {
  

  // Dimension
  int n = yz.n_rows; 
  int M = Xlist.size(); 
  int p = yz.n_cols-1;
  IntegerVector Kvec = rep(0,M);
  
  arma::field<arma::mat> X(M);
  arma::field<arma::vec> theta(M);
  arma::mat  Xtheta(n,M,arma::fill::zeros);
  
  for(int m=0; m<M; m++){
    X[m] = as<arma::mat>(Xlist[m]);
    Kvec(m) = X[m].n_cols;
    theta[m] = as<arma::vec>(rnorm(Kvec(m)));
    Xtheta.col(m) = X[m] * theta[m];
  }
  
  // Starting values 
  double logtau2 = rnorm(1)(0)*sqrt(b1)/10;
  
    
    
  // Construct covariance amtrix Sigma=I+tau2*K
  arma::mat Sigma(n,n); Sigma.zeros();
  for(int i=0; i<n-1; i++){
    for(int j=i+1; j<n; j++){
      Sigma(i,j) = exp(logtau2 - arma::sum(square(vectorise(Xtheta.row(i)-Xtheta.row(j)))));
      Sigma(j,i) = Sigma(i,j);
    }  
    Sigma(i,i) = exp(logtau2)+1;
  }
  Sigma(n-1,n-1) = exp(logtau2)+1;
    
  
  // First evaluation of the log-liklihood (ll)
  arma::mat C1 = arma::chol(Sigma, "lower");

  // half of (y,Z)^T * Sigma^{-1} * (y,Z)
  arma::mat Cyx = arma::solve(trimatl(C1),yz);
  // A1 = y^T * Sigma^{-1} * y
  double A1 = arma::sum(square(vectorise(Cyx.col(0))));
  // B = z^T * Sigma^{-1} * y
  arma::vec B = Cyx.cols(1,p).t() * Cyx.col(0);
  // C2 = chol( z^T * Sigma^{-1} * z)
  arma::mat C2 = arma::chol(Cyx.cols(1,p).t() * Cyx.cols(1,p), "lower");
  arma::vec C2B = vectorise(arma::solve(trimatl(C2),B));
  double A2 = arma::sum(square(C2B));


  double ll = -logtau2*logtau2/(2*b1)
    -  arma::accu(log(C1.diag()))
    -  arma::accu(log(C2.diag()))
    - (   0.5 * (n - p) + a1 ) * log( a2 + 0.5*A1-0.5*A2 );

  // Define other values
  double logtau2_PROP, angle, amax, amin, llu, ll_PROP;
  arma::vec ons(n, arma::fill::ones);
  arma::vec nu(M, arma::fill::ones);
  arma::vec vec_PROP;
  arma::vec theta_PROP;
  arma::field<arma::vec> phi(M);
  arma::field<arma::vec> psi(M);
  arma::field<arma::vec> Tvecs(M);
  
  for(int m=0; m<M; m++){
    Tvecs[m] = arma::vec(Kvec(m), arma::fill::ones);
    psi[m] = arma::vec(Kvec(m), arma::fill::ones);
    phi[m] = arma::vec(Kvec(m), arma::fill::ones)/Kvec(m);
    nu[m] =  do_rgiglocal( 1-Kvec[m] , 2*sum(abs(theta[m])/phi[m]) , 1 );
  }
  
  // place to store posterior samples
  arma::vec tau2keep(n_outer,arma::fill::zeros);
  arma::mat rhokeep(n_outer,M,arma::fill::zeros);
  arma::vec sig2invkeep(n_outer,arma::fill::zeros);
  arma::mat betakeep(n_outer,p,arma::fill::zeros);
  Rcpp::List  thetakeep(M);
  arma::field<arma::mat>  thetakeepfield(M);
  for(int m=0; m<M; m++){
    arma::mat thetakeeptep(n_outer,Kvec(m),arma::fill::zeros);
    thetakeep[m] = thetakeeptep;
    thetakeepfield[m] = thetakeeptep;
  } 
  
  arma::mat K(n,n,arma::fill::zeros);
  arma::mat KV(n,n,arma::fill::zeros);
  arma::mat hcov(n,n,arma::fill::zeros);
  arma::vec hmean(n,arma::fill::zeros);
  arma::vec hmeantemp(n,arma::fill::zeros);
  
  // -------------------------------------
  // MCMC
  // -------------------------------------
  for(int s_outer=0; s_outer<n_outer; s_outer++){
    for(int s_inner=0; s_inner<n_inner; s_inner++){
      
      // -------------------------------------
      // Update logtau2
      logtau2_PROP = logtau2 + rnorm(1)(0);
      
      // covariance is Sigma = (I + tau2*K)
      Sigma = exp(log(Sigma) - logtau2 +logtau2_PROP);
      Sigma.diag() = ons + exp(logtau2_PROP);
        
      // First evaluation of the log-liklihood (ll)
      
      C1 = arma::chol(Sigma, "lower");
      
      // half of (y,Z)^T * Sigma^{-1} * (y,Z)
      Cyx = arma::solve(trimatl(C1),yz);
      // A1 = y^T * Sigma^{-1} * y
      A1 = arma::sum(square(vectorise(Cyx.col(0))));
      // B = z^T * Sigma^{-1} * y
      B = Cyx.cols(1,p).t() * Cyx.col(0);
      // C2 = chol( z^T * Sigma^{-1} * z)
      
      C2 = arma::chol(Cyx.cols(1,p).t() * Cyx.cols(1,p), "lower");
      C2B = vectorise(arma::solve(trimatl(C2),B));
      A2 = arma::sum(square(C2B));
       
      // proposed ll 
      ll_PROP = -logtau2_PROP*logtau2_PROP/(2*b1)
          -  arma::accu(log(C1.diag()))
          -  arma::accu(log(C2.diag()))
          - (   0.5 * (n - p) + a1 ) * log( a2 + 0.5*A1-0.5*A2 );
         
       // accept for reject    
       if(ll_PROP > ll  - rexp(1)(0) ){
         logtau2 = logtau2_PROP;
         ll = ll_PROP;
       }
        
      // -------------------------------------
      // update hyper-parameters for theta
      for(int m=0; m<M; m++){
        
        for(int j=0; j<Kvec[m]; j++){
          psi[m][j] = 1/rinvgauss(nu[m]*phi[m][j]/fabs(theta[m][j]) ,1 );
        }
        
        nu[m] =  do_rgiglocal( 1-Kvec[m] , 2*sum(abs(theta[m])/phi[m]) , 1 );
        
        for(int j=0; j<Kvec[m]; j++){
          Tvecs[m][j] =  do_rgiglocal( 1/Kvec[m]-1 , 2*fabs(theta[m][j]) , 1 );
        }
        phi[m] = Tvecs[m] / sum( Tvecs[m] );
      }
          
      
      // -------------------------------------
      // Update theta  
      Sigma.diag() = ons + exp(logtau2);  //reset this
      for(int m=0; m<M; m++){
        
        angle = runif(1)(0)*M_PI;
        amax = angle;
        amin = angle - M_PI;
        llu = log(runif(1)(0)) + ll;
        ll_PROP = llu-1;
        vec_PROP = rnorm(Kvec[m]);
        for(int j=0; j<Kvec[m]; j++){
          vec_PROP[j] = vec_PROP[j]*(nu[m]*phi[m][j]*sqrt(psi[m][j]));
        }

            // shrinking slice
            while(ll_PROP < llu){
              
              // new paroposed theta
              theta_PROP = theta[m]*cos(angle) + vec_PROP*sin(angle);
              
              // construct covariance matrix
              Xtheta.col(m) = X[m] * theta_PROP;
              
              
              for(int i=0; i<n-1; i++){
                for(int j=i+1; j<n; j++){
                  Sigma(i,j) = exp(logtau2 - arma::sum(square(vectorise(Xtheta.row(i)-Xtheta.row(j)))));
                  Sigma(j,i) = Sigma(i,j);
                }  
              }
              
              // First evaluation of the log-liklihood (ll)
              C1 = arma::chol(Sigma, "lower");
              
              // half of (y,Z)^T * Sigma^{-1} * (y,Z)
              Cyx = arma::solve(trimatl(C1),yz);
              // A1 = y^T * Sigma^{-1} * y
              A1 = arma::sum(square(vectorise(Cyx.col(0))));
              // B = z^T * Sigma^{-1} * y
              B = Cyx.cols(1,p).t() * Cyx.col(0);
              // C2 = chol( z^T * Sigma^{-1} * z)
              C2 = arma::chol(Cyx.cols(1,p).t() * Cyx.cols(1,p), "lower");
              C2B = arma::solve(trimatl(C2),B);
              A2 = arma::sum(square(C2B));
              
              // proposed ll 
              ll_PROP = -logtau2*logtau2/(2*b1)
                -  arma::accu(log(C1.diag()))
                -  arma::accu(log(C2.diag()))
                - (   0.5 * (n - p) + a1 ) * log( a2 + 0.5*A1-0.5*A2 );
                
              
              if(angle > 0){
                amax = angle;
              }else{
                amin = angle;
              }
              angle = runif(1)(0)*(amax-amin)+amin;
            }
            
            // save results
            ll =  ll_PROP;
            theta[m] = theta_PROP;
          } // end loop through update of theta
            
    }// end inner loop  
    for(int m=0; m<M; m++){
      rhokeep.submat(s_outer,m,s_outer,m) = 1/sum(square(vectorise(theta[m])));
      thetakeepfield[m].row(s_outer) =  theta[m].t()*sqrt(as_scalar(rhokeep.submat(s_outer,m,s_outer,m)));
    }
    tau2keep(s_outer) = logtau2;
    sig2invkeep(s_outer) = rgamma( 1 , a1 + .5*(n-p) , 1/(a2 + 0.5*(A1-A2)) )(0);  // function generates a gamma with mean alpha*beta;
    betakeep.row(s_outer) = arma::solve(trimatu(C2) , C2B +  as<arma::vec>(rnorm(p))/sqrt(sig2invkeep(s_outer) )).t();
    
    if(s_outer>=n_burn){
      K = Sigma;
      for(int i=0; i<n; i++){
        K(i,i) -= 1;
      }
      KV = solve(Sigma , K);
      hmeantemp = KV * (yz.col(0)-yz.cols(1,p)*betakeep.row(s_outer).t());
      
      hmean += hmeantemp/(n_outer-n_burn);
      hcov += KV/(n_outer-n_burn)/sig2invkeep(s_outer) + hmeantemp*hmeantemp.t()  /(n_outer-n_burn);
    }
    
  }// end outer loop
  
  // Convert field to list
  for(int m=0; m<M; m++){
    thetakeep[m] = thetakeepfield[m];
  }

  // return a list
  return List::create(Named("theta") = thetakeep,
                      Named("tau2") =  exp(tau2keep),
                      Named("rho") =  rhokeep,
                      Named("sigma2") =  1/sig2invkeep,
                      Named("beta") =  betakeep,
                      Named("hmean") =  hmean,
                      Named("hcov") =  hcov - ( hmean*hmean.t()));
}


