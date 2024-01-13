library(cowplot)
#Hierarchical 2 prior sensitivity
prior1 =list(N=numkids,J=numfams,y=data.frame(dataset),f=fathers,m=mothers,sigmaalpha=7.5,sigmabeta=1)
prior2 =list(N=numkids,J=numfams,y=data.frame(dataset),f=fathers,m=mothers,sigmaalpha=10,sigmabeta=2)
prior3 =list(N=numkids,J=numfams,y=data.frame(dataset),f=fathers,m=mothers,sigmaalpha=40,sigmabeta=20)
prior4 =list(N=numkids,J=numfams,y=data.frame(dataset),f=fathers,m=mothers,sigmaalpha=5,sigmabeta=0.5)

extract_1 = rstan::sampling(modelh2,data=prior1,iter=4000)
extract_2 = rstan::sampling(modelh2,data=prior2,iter=4000)
extract_3 = rstan::sampling(modelh2,data=prior3,iter=4000)
extract_4 = rstan::sampling(modelh2,data=prior4,iter=4000)


family = 2
#You need to change "ypred[family]" accordingly inside the function
whichprior <- function(extract){
  y_pr = as.matrix(extract,pars="ypred[2]")
  y = dataset[,family]
  y_pr = y_pr[1:7998]
  y_pr = matrix(y_pr,ncol=6)
  #Kernel estimates
  # “brightblue” “green”, “red”, "purple"
      color_scheme_set("purple")
  q <-ppc_dens_overlay(y,y_pr[1:200,])+theme_gray(base_size = 13) + labs(x="height (cm)",y="density")
}

sensit1 = whichprior(extract_1)

sensit2 = whichprior(extract_2)

sensit3 = whichprior(extract_3)

sensit4 = whichprior(extract_4)


plot_grid(sensit1, sensit2,sensit3, sensit4)
#mean and std
ppc_stat_2d(y,y_pr,stat =c("mean","sd"))
