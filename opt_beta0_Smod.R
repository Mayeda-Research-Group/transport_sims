library(tidyverse)
library(future.apply)
library(here)

path_to_data<-"/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Code/Data"
source(paste0(here(),"/genstudysamp_opt.R"))

OptimizeBeta0S<-function(Scenario, nrep=500, Lwin=-11, Uwin=-7){
    
  set.seed(12345)
  
  #Restrict parameters to scenario of interest:
  load(paste0(path_to_data,"/params_beta0_",Scenario,".Rdata"))
  load(paste0(path_to_data,"/targetpop_",Scenario,".Rdata"))


#Optimize for beta_S_0 for use in sampling later. Need to do this over and over to get best optimization. 

  plan(multisession, workers=6) ## Run in parallel on local computer. Can specify 10 cores for desktop.
  
  #start<-Sys.time()
  beta_S0_reps<-future_replicate(nrep,optimize(genstudysamp_opt, lower = Lwin, upper = Uwin,
                                              maximum = FALSE,
                                              data=targetpop, 
                                              ntarget=p$ntarget, nstudysamp=p$nstudysamp, 
                                              beta_S_A=p$beta_S_A, beta_S_C1=p$beta_S_C1, beta_S_C2=p$beta_S_C2, beta_S_Z=p$beta_S_Z, 
                                              beta_S_AC1=p$beta_S_AC1, beta_S_AC2=p$beta_S_AC2, beta_S_AZ=p$beta_S_AZ, beta_S_C1Z=p$beta_S_C1Z, beta_S_C2Z=p$beta_S_C2Z)$minimum)

  plan(sequential)
  
  p$beta_S_0<-mean(beta_S0_reps)
  save(p, file=paste0(path_to_data,"/params_beta0_",Scenario,".Rdata"))
  
  #Uncomment this when testing to find range. 
  #return(beta_S0_reps)
} 
#To get new search windows for nsamp=3000 instead of nsamp=1500,
#subtract logit(1500/6M/(1-1500/6M))-logit(3000/6M/(1-3000/6M)) ~= +0.7
OptimizeBeta0S("S1", nrep=500, Lwin=-8.2, Uwin=-8.05)
OptimizeBeta0S("S2", nrep=500, Lwin=-8.75, Uwin=-8.63)
OptimizeBeta0S("S3", nrep=500, Lwin=-8.2, Uwin=-8.05)
OptimizeBeta0S("S4", nrep=500, Lwin=-8.55, Uwin=-8.35)
OptimizeBeta0S("S5", nrep=500, Lwin=-10.3, Uwin=-6.3)
OptimizeBeta0S("S6", nrep=500, Lwin=-8.55, Uwin=-8.35)
OptimizeBeta0S("S7", nrep=500, Lwin=-10.3, Uwin=-6.3)
OptimizeBeta0S("S8", nrep=500, Lwin=-8.55, Uwin=-8.35)
OptimizeBeta0S("S9", nrep=500, Lwin=-8.6, Uwin=-8.45)
OptimizeBeta0S("S10", nrep=500,  Lwin=-8.63, Uwin=-8.5)  

OptimizeBeta0S("S11", nrep=500, Lwin=-7.8, Uwin=-7.7)
OptimizeBeta0S("S12", nrep=500, Lwin=-10.3, Uwin=-6.3)
OptimizeBeta0S("S13", nrep=500, Lwin=-7.8, Uwin=-7.7)
OptimizeBeta0S("S14", nrep=500, Lwin=-8.2, Uwin=-8.1)
OptimizeBeta0S("S15", nrep=500, Lwin=-7.75, Uwin=-7.6)
OptimizeBeta0S("S16", nrep=500, Lwin=-8.25, Uwin=-8.1)
OptimizeBeta0S("S17", nrep=500, Lwin=-7.75, Uwin=-7.6)
OptimizeBeta0S("S18", nrep=500, Lwin=-8.23, Uwin=-8.05)
OptimizeBeta0S("S19", nrep=500, Lwin=-8.25, Uwin=-8.1)
OptimizeBeta0S("S20", nrep=500,  Lwin=-8.25, Uwin=-8.1)  



#For testing to find range
#test<-OptimizeBeta0S("S10", nrep=50,  Lwin=-11, Uwin=-7)  
#plot(test)
# 
# # #Check creation
# paramcheck<-data.frame(matrix(ncol=42))
# for (i in 1:20){
#   load(paste0(path_to_data,"/params_beta0_S",i,".Rdata"))
#   #p<-p %>% select(selectcols)
#   paramcheck[i,]<-p
#   colnames(paramcheck)<-colnames(p)
#}
# 
# 
# 
# # 
# # 
# #update parameter file without re-writing from beginning
# 
# for (i in 1:10){
#   load(paste0(path_to_data,"/params_beta0_S",i,".Rdata"))
#   p$YformnoZ<-"A+C1+C2+A*C1+A*C2"
#   save(p, file=paste0(path_to_data,"/params_beta0_S",i,".Rdata"))
#   
# }




#Check local min/running avgs. 
# beta_S0_reps<-OptimizeBeta0S("S1")
# beta_S0_reps2<- beta_S0_reps %>% data.frame()
# beta_S0_reps2$betaS0<-beta_S0_reps2[,"."]
# 
# beta_S0_reps2<-beta_S0_reps2 %>% mutate(rec = 1) %>%
#   mutate(runavg = cumsum(betaS0)/cumsum(rec),
#          run = c(1:100)) %>%
#   select(-rec, -.)
# 
# plot(x=beta_S0_reps2$run, y=beta_S0_reps2$runavg)
# plot(x=beta_S0_reps2$run, y=beta_S0_reps2$betaS0)
