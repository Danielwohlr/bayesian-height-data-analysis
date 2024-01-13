//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //number of measurements
  int<lower=0> J; //number of families
  vector[J] y[N]; //matrix of measurements 
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[J] mu;
  real<lower=0> sigma;
  real<lower=0> sigma0;
  real mu0;
}

model {
  //PRIORS
  mu0 ~ normal(170,10);// weakly informative prior
  sigma0 ~ gamma(7.5,1);// weakly informative prior
  sigma ~ gamma(7.5,1);// weakly informative prior
  for (j in 1:J){
    mu[j] ~ normal(mu0,sigma0);// population prior with unknown parameters
  }
  //LIKELIHOOD
  for (i in 1:N){
    for (j in 1:J){
      y[i,j] ~ normal(mu[j], sigma);
    }
  }
}
  generated quantities {
    vector[J] ypred;
    vector[J] log_lik[N];
    for (j in 1:J){
      ypred[j] = normal_rng(mu[j],sigma);
    }
    for (i in 1:N){
      for (j in 1:J){
        log_lik[i,j] = normal_lpdf(y[i,j] | mu[j], sigma);
      }
    }

}
