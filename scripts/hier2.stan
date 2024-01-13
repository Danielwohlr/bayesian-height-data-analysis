
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //number of measurements
  int<lower=0> J; //number of families
  vector[J] y[N]; //matrix of measurements 
  vector[J] f; //height of father
  vector[J] m; //height of mother
  real sigmaalpha;
  real sigmabeta;
  real fatcoef;
  real matcoef;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[J] mu;
  real<lower=0> sigma;
  real<lower=0> sigma0;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //PRIORS
  sigma ~ gamma(sigmaalpha,sigmabeta);
  sigma0 ~ gamma(sigmaalpha,sigmabeta);
  for (j in 1:J){
    mu[j] ~ normal(fatcoef*f[j]+matcoef*m[j],sigma0);
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
 


