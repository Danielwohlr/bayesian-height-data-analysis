#Loaded libraries and data
library(readr)
library(corrgram)
library(plyr)
library(dplyr)
library(rstan)
library(shinystan)
library(bayesplot)
library(ggplot2)
library(gridExtra)
library(MASS)
library(fitdistrplus)

#Galton's height data
height <- read_csv("height.csv")

#Permuting rows so there is no hidden structure
height <- height[sample(nrow(height)),]

#Data cleaning
height$Father = height$Father*2.54
height$Mother = height$Mother*2.54
height$Height = height$Height*2.54
height$Gender = NULL

#Plot Father-kids with mother color
p <- ggplot(height, aes(x=height$Father, y=height$Height))+
  scale_x_continuous( ) +
  scale_y_continuous() +
  labs(title = 'Father-Child', x = 'Father (cm)', y = 'Child (cm)') +
  theme(panel.grid.major = element_blank())+
  geom_jitter(aes(colour=Mother))+
  theme_gray(base_size = 18)

#Plot Mother-kids with father color
q<-ggplot(height, aes(x=height$Mother, y=height$Height))+
  scale_x_continuous( ) +
  scale_y_continuous() +
  labs(title = 'Mother-Child', x = 'Mother (cm)', y = 'Child (cm)') +
  theme(panel.grid.major = element_blank())+
  geom_jitter(aes(colour=Father))+
theme_gray(base_size = 18)

grid.arrange(p, q, nrow = 1)

#Correlation
# corr_height <- height
# corr_height$Family = NULL
# corr_height$Kids = NULL
# corr_height$Gender = NULL
# 
# 
# corrgram(corr_height, upper.panel = NULL)

#Forming a dataset for STAN model
#numkids = 5 =>> numfams = 94
numkids = 6
#filter families with more kids than numkids
data = height[height$Kids>=numkids,]

#getting number of families in my data
numfams = length(unique(data$Family))
#creating dataset which will be put into STAN
dataset<- matrix(nrow=numkids,ncol=numfams )
j=1
for (k in unique(data$Family)){
  for(i in 1:numkids){
    dataset[i,j] = data$Height[data$Family==k][i]    
  }
  j=j+1
}
#Getting height of fathers and mothers in families
fathers <- c()
mothers <- c()
m = 1
for (j in unique(data$Family)){
  fathers[m] = unique(data$Father[data$Family==j])
  mothers[m] = unique(data$Mother[data$Family==j])
  m=m+1
}
#Data frame for presentation ggplot
df <- data[-c(2,3,5)]
amrdka = df[df$Family==130,]
bmrdka = df[df$Family==75,]
cmrdka = df[df$Family==5,]
dmrdka = df[df$Family==190,]

gf <- rbind(amrdka,rbind(dmrdka,rbind(bmrdka,cmrdka)))
gf$Family = as.integer(gf$Family)
obr <- ggplot(gf,aes(Height,Family))+
  geom_point(aes(colour=Family))+ theme_gray(base_size = 18) 
print(obr+ggtitle("Heights of kids in different families")+
        labs(y="Number of the family",x="Height of the kid (cm)"))
  

# Histogram for Introduction chapter
#  intro <- height[height$Family==130,]
#  kids <- intro$Height
#  kids <- jitter(kids)
#  fit <- fitdistr(kids, "normal")
# # 
#  xname = 130
#  hist(kids, prob = TRUE,main = paste("Histogram of family No." , xname),xlim=c(160,185),xlab = "Height of children (cm)")
#  curve(dnorm(x, fit$estimate[1], fit$estimate[2]), add = TRUE,col="violet")
#  abline(v=c(174,169), col=c("blue", "red"), lty=c(2,2), lwd=c(3, 3))


#STAN MODELS
#####
#Separate model
#####
#Input for STAN
hier2_data =list(N=numkids,J=numfams,y=data.frame(dataset),f=fathers,m=mothers,
                 fatcoef =0.7,matcoef=0.3,sigmaalpha=10,sigmabeta=1)
#Compiling STAN
modelh2 = stan_model("hier2.stan",model_name="hier2")
#SAMPLING from STAN
extract_hier2 = rstan::sampling(modelh2,data=hier2_data,iter=4000)
#traceplot(extract_hier2, pars= c("mu[1]","mu[2]","mu[3]","mu[4]"))
#EXTRACTING INFO from STAN
la_hier2 = extract(extract_hier2)

#getting Rhat and ESS values for Hierarchical model
#with parental prior mean
summhier2 = summary(extract_hier2)$summary
rhat_hier2 = summhier2[,10]
ess_hier2 = summhier2[,9]
max(rhat_hier2)
min(ess_hier2)


#Getting the log_lik for LOO-PSIS
log_lik_h2 = cbind()
for (i in 1:numkids){
  for (j in 1:numfams){
    dummy = la_hier2$log_lik[,i,j]
    log_lik_h2 = cbind(log_lik_h2,dummy)
  }
}
#GETTING LOO done
loo_h2 = loo(log_lik_h2)
est_h2 = loo_h2$estimates
#PSIS-LOO
est_h2[1,1]
#p_eff
est_h2[2,1]
#plotting Pareto K-values (ideally <0.7)
pareto_k = loo_h2$diagnostics$pareto_k
plot (loo_h2, diagnostic = c ("k"), label_points = FALSE , main =
        "PSIS k-values model")
#Number of k values >0.7
length(pareto_k[pareto_k>0.7])
#####

#Pooled model
#####
#Input for STAN
pool_data =list(N=numkids,J=numfams,y=data.frame(dataset))
#Compiling STAN
modelp = stan_model("pool.stan",model_name="pooled")

#SAMPLING from STAN
extract_pool= rstan::sampling(modelp,data=pool_data,iter=4000)
#traceplot(extract_pool,pars=c("mu","sigma"))
#EXTRACTING INFO from STAN
la_pool = extract(extract_pool)

#getting Rhat and ESS values for Pooled model
summpool = summary(extract_pool)$summary
rhat_pool = summpool[,10]
ess_pool = summpool[,9]
max(rhat_pool)
min(ess_pool)


#Getting the log_lik for LOO-PSIS
log_lik_p = cbind()
for (i in 1:numkids){
  for (j in 1:numfams){
    dummy = la_pool$log_lik[,i,j]
    log_lik_p = cbind(log_lik_p,dummy)
  }
}
#GETTING LOO done
loo_p = loo(log_lik_p)
est_p = loo_p$estimates
#PSIS-LOO
est_p[1,1]
#p_eff
est_p[2,1]
#plotting Pareto K-values (ideally <0.7)
pareto_k = loo_p$diagnostics$pareto_k
plot (loo_p, diagnostic = c ("k"), label_points = FALSE , main =
        "PSIS k-values model")
length(pareto_k[pareto_k>0.7])
#####

#Hierarchical model
#####
#Input for STAN
hier_data =list(N=numkids,J=numfams,y=data.frame(dataset))
#Compiling STAN
model = stan_model("hier.stan",model_name="hier")
#SAMPLING from STAN
extract_hier= rstan::sampling(model,data=hier_data,iter=4000)
#EXTRACTING INFO from STAN
la_hier = extract(extract_hier)


#getting Rhat and ESS values for Hierarchical model
summhier = summary(extract_hier)$summary
rhat_hier = summhier[,10]
ess_hier = summhier[,9]
max(rhat_hier)
min(ess_hier)




#POSTERIOR of mu for family number (7 or anything I plugin)
hist(la_hier$mu,xlim=c(150,195),breaks=50)
#Getting the log_lik for LOO-PSIS
log_lik_h = cbind()
for (i in 1:numkids){
  for (j in 1:numfams){
    dummy = la_hier$log_lik[,i,j]
    log_lik_h = cbind(log_lik_h,dummy)
  }
}
#GETTING LOO done
loo_h = loo(log_lik_h)
est_h = loo_h$estimates
#PSIS-LOO
est_h[1,1]
#p_eff
est_h[2,1]
#plotting Pareto K-values (ideally <0.7)
pareto_k = loo_h$diagnostics$pareto_k
plot (loo_h, diagnostic = c ("k"), label_points = FALSE , main =
        "PSIS k-values model")
length(pareto_k[pareto_k>0.7])
#####

#####

#POSTERIOR of mu for family number (7 or anything I plugin)
hist(la_hier2$mu[,1],xlim=c(160,190),breaks=50)
#Posterior of mu[2]
mcmc_areas(data.frame(la_hier2$mu[,2]))

#HIERARCHICAL DENSITY COMPARSION
#####
family = 50
y_pr = as.matrix(extract_hier2,pars="ypred[50]")
y = dataset[,family]
y_pr = y_pr[1:7998]
y_pr = matrix(y_pr,ncol=6)
#Kernel estimates
color_scheme_set("pink")
q<-ppc_dens_overlay(y,y_pr[1:200,])+theme_gray(base_size = 13) + labs(x="height (cm)",y="density")
d<-ppc_stat_2d(y,y_pr,stat =c("mean","sd"))

y_pr = as.matrix(extract_hier,pars="ypred[50]")
y_pr = y_pr[1:7998]
y_pr = matrix(y_pr,ncol=6)
color_scheme_set("green")
b<-ppc_dens_overlay(y,y_pr[1:200,])+theme_gray(base_size = 13) + labs(x="height (cm)",y="density")
e<-ppc_stat_2d(y,y_pr,stat =c("mean","sd"))

y_pr = as.matrix(extract_pool,pars="ypred[50]")
y_pr = y_pr[1:7998]
y_pr = matrix(y_pr,ncol=6)
color_scheme_set("purple")
c<-ppc_dens_overlay(y,y_pr[1:200,])+theme_gray(base_size = 13) +ylim(0,0.3)+ labs(x="height (cm)",y="density")
f<-ppc_stat_2d(y,y_pr,stat =c("mean","sd"))

plot_grid(c,b,a,labels = "auto",hjust=-0.5)

#mean and std
plot_grid(f,e,d,labels="auto",hjust=-0.5)
#####
#Predictive performance
mu_drawp <- as.matrix(extract_pool,pars="mu")
mu_drawh <- as.matrix(extract_hier,pars="mu[2]")
mu_drawh2 <- as.matrix(extract_hier2,pars="mu[2]")
mus <- cbind(mu_fit_pool=mu_drawp[,1],
             mu_fit_hier=mu_drawh[,1],
             mu_fit_hier2=mu_drawh[,1])
mcmc_areas(mus,prob=0.8)
