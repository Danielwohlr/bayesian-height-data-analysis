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
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //PRIORS
  mu ~ normal(180,20);
  sigma ~ gamma(10,1);
  //Likelihood
  for (i in 1:N){
    for (j in 1:J){
      y[i,j] ~ normal(mu,sigma);
    }
  }
}
generated quantities{
    vector[J] ypred;
    vector[J] log_lik[N];
    for (j in 1:J){
      ypred[j] = normal_rng(mu,sigma);
    }
    for (i in 1:N){
      for (j in 1:J){
        log_lik[i,j] = normal_lpdf(y[i,j] | mu, sigma);
      }
    }
}


