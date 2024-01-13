library(readr)
library(corrgram)
library(plyr)
library(dplyr)
library(rstan)
#Galton's height data
height <- read_csv("height.csv")

#Data cleaning
height$Father = height$Father*2.54
height$Mother = height$Mother*2.54
height$Height = height$Height*2.54
height$is_male = as.integer(height$Gender=="M")
height$Gender = NULL

zkouska <- height
zkouska = zkouska[height$Kids==8,]

hist(height$Kids)

numkids = 9

data = height[height$Kids==numkids,]

numfams = length(unique(data$Family))

plot((data$Father),data$Height)

kokot <- matrix(nrow=numkids,ncol=numfams )
j=1
for (k in unique(data$Family)){
  for(i in 1:numkids){
    kokot[i,j] = data$Height[data$Family==k][i]    
  }
  j=j+1
}
fathers <- c()
mothers <- c()
m = 1
for (j in unique(data$Family)){
  fathers[m] = unique(data$Father[data$Family==j])
  mothers[m] = unique(data$Mother[data$Family==j])
    m=m+1
}

height_data =list(N=numkids,J=numfams,y=data.frame(kokot),f=fathers,m=mothers)
model = stan_model("tryout.stan",model_name="separate")
extract_model = rstan::sampling(model,data=height_data)

la_separate = extract(extract_model)

hist(la_separate$mu[,1],xlim=c(150,190),breaks=50)

log_lik_s = cbind()
for (i in 1:nrow(kokot)){
  for (j in 1:ncol(kokot)){
    dummy = la_separate$log_lik[,i,j]
    log_lik_s = cbind(log_lik_s,dummy)
  }
}

loo_s = loo(log_lik_s)
est_s = loo_s$estimates
#PSIS-LOO
est_s[1,1]
#p_eff
est_s[2,1]

pareto_k = loo_s$diagnostics$pareto_k
plot(pareto_k)



#Correlation
#corrgram(height, upper.panel = NULL)
#pairs(height, gap = 0, upper.panel = NULL)


