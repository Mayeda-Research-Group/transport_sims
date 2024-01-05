library(tidyverse)
path_to_data<-"/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Code/Data"


for (Scenario in 1:20){
  
  load(paste0(path_to_data,"/params_beta0_S",Scenario,".Rdata"))

  load(paste0(path_to_data,"/targetpop_S",Scenario,".Rdata"))
  
  #Y model
TrueOM<-lm(as.formula(paste0("Y~",p$Yform)), data=targetpop)
TrueMM<-lm(as.formula(paste0("Z~",p$Zform)), data=targetpop)  


#Create target pop replicates:
targpop_A1<- targetpop %>% mutate(A=1, rep="exp_copy")
targpop_A0<-targetpop %>% mutate(A=0, rep="unexp_copy")
targpopreps<-rbind(targpop_A1, targpop_A0)  

#Simulate Z in reps using MM, and predict Y using simulated Z.
targpopreps$Z<-predict.lm(TrueMM,targpopreps)
targpopreps$Y_OM<-predict.lm(TrueOM,targpopreps)

#
TrueYs<-targpopreps %>% group_by(rep) %>% summarise(Ymean=mean(Y_OM))

p$Truth_YA1<-TrueYs$Ymean[TrueYs$rep=="exp_copy"]
p$Truth_YA0<-TrueYs$Ymean[TrueYs$rep=="unexp_copy"]


save(p, file=paste0(path_to_data,"/params_beta0_S",Scenario,".Rdata"))

}
