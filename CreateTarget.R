library("future.apply")


CreateTarget<-function(Scenario){
  
  
  #Restric parameters to scenario of interest:
  p<-params %>% filter(SimNo==Scenario)
  
  #Optimize A, Z, Y sequentially
  set.seed(12345)
  beta_A0_opt<-optimize(gen_target_optA, lower = -10, upper = 0,
                          maximum = FALSE, ntarget=p$ntarget, 
                          beta_A_C1=p$beta_A_C1, beta_A_C2=p$beta_A_C2, 
                          Aprop=p$Aprop, C1prev=p$C1prev, C2form=p$C2form)
    
    
    
  set.seed(12345)
  beta_Z0_opt<-optimize(gen_target_optZ, lower = -5, upper = 1,
                          maximum = FALSE, 
                          ntarget=p$ntarget, 
                          beta_A_0=beta_A0_opt$minimum, beta_A_C1=p$beta_A_C1, beta_A_C2=p$beta_A_C2,
                          beta_Z_A=p$beta_Z_A, 
                          Zmean=p$Zmean, C1prev=p$C1prev, C2form=p$C2form)
    
    
  
  set.seed(12345)
  beta_Y0_opt<-optimize(gen_target_optY, lower = -10, upper = 10,
                          maximum = FALSE, 
                          ntarget=p$ntarget, 
                          beta_A_0=beta_A0_opt$minimum, beta_A_C1=p$beta_A_C1, beta_A_C2=p$beta_A_C2, 
                          beta_Z_0=beta_Z0_opt$minimum, beta_Z_A=p$beta_Z_A, 
                          beta_Y_A=p$beta_Y_A, beta_Y_C1=p$beta_Y_C1, beta_Y_C2=p$beta_Y_C2, 
                          beta_Y_Z=p$beta_Y_Z, 
                          beta_Y_AC1=p$beta_Y_AC1, beta_Y_AC2=p$beta_Y_AC2, beta_Y_AZ=p$beta_Y_AZ, beta_Y_C1Z=p$beta_Y_C1Z, beta_Y_C2Z=p$beta_Y_C2Z, 
                          Ymean=p$Ymean, C1prev=p$C1prev, C2form=p$C2form)
    
  

set.seed(12345)
#Generate target pop
    targetpop<-gen_target(
        #Parameters:
        #sample sizes
        ntarget=p$ntarget,
        # A model
        beta_A_0=beta_A0_opt$minimum, beta_A_C1=p$beta_A_C1, beta_A_C2=p$beta_A_C2,
        # Z model
        beta_Z_0=beta_Z0_opt$minimum, beta_Z_A=p$beta_Z_A,
        # Y model
        beta_Y_0=beta_Y0_opt$minimum, beta_Y_A=p$beta_Y_A, beta_Y_C1=p$beta_Y_C1, beta_Y_C2=p$beta_Y_C2, beta_Y_Z=p$beta_Y_Z, 
        beta_Y_AC1=p$beta_Y_AC1, beta_Y_AC2=p$beta_Y_AC2, beta_Y_AZ=p$beta_Y_AZ, beta_Y_C1Z=p$beta_Y_C1Z, beta_Y_C2Z=p$beta_Y_C2Z,
        C1prev=p$C1prev, C2form=p$C2form)
    
  #Save with scenario name in filename
    save(targetpop, file=paste0(path_to_data,"/targetpop_",Scenario,".Rdata"))
    
  #Calculate True total effect of A on Y with g-computation
    
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
    Truth<-as.numeric(TrueYs$Ymean[TrueYs$rep=="exp_copy"] - 
                         TrueYs$Ymean[TrueYs$rep=="unexp_copy"])
    
  
    
    #Save truth and optimized betas     
    p$Truth<-Truth
    p$beta_A_0<-beta_A0_opt$minimum
    p$beta_Z_0<-beta_Z0_opt$minimum
    p$beta_Y_0<-beta_Y0_opt$minimum
    
  save(p, file=paste0(path_to_data,"/params_beta0_",Scenario,".Rdata"))
}
  




#test
#load(file=paste0(path_to_data,"/params_beta0_","S1",".Rdata"))

#load(file=paste0(path_to_data,"/params_beta0_","S2",".Rdata"))
