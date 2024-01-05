library(tidyverse)
library(openxlsx)


path_to_data<-"/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Code/Data"

#########################################
#Pull parameters for supplement tables
#########################################

paramcheck<-data.frame(matrix(ncol=44))
for (i in 1:9){
  load(paste0(path_to_data,"/params_beta0_S",i,".Rdata"))
  paramcheck[i,]<-p
  colnames(paramcheck)<-colnames(p)
}


tableA1<-paramcheck %>% select(SimNo, beta_A_0, beta_A_C1, beta_A_C2)
tableA2<-paramcheck %>% select(SimNo, beta_Z_0, beta_Z_A)
tableA3<-paramcheck %>% select(SimNo, beta_Y_0, beta_Y_A, beta_Y_C1, beta_Y_C2, 
                               beta_Y_Z, beta_Y_AC1, beta_Y_AC2, beta_Y_AZ, 
                               beta_Y_C1Z, beta_Y_C2Z)
tableA4<-paramcheck %>% select(SimNo, beta_S_0, beta_S_A, beta_S_C1, beta_S_C2, 
                               beta_S_Z, beta_S_AC1, beta_S_AC2, beta_S_AZ, 
                               beta_S_C1Z, beta_S_C2Z)


paramtables<-list(tableA1=tableA1, tableA2=tableA2, tableA3=tableA3, tableA4=tableA4)

write.xlsx(paramtables, file = "/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/paramtables.xlsx")

rm(list=ls())


#########################################
#Do one simulation for explaining bias in supplement
#########################################


#Here I have copied and pasted from the "Analysis". 
#I couldn't just source the analysis function because of the way the script 
#   is set up for Hoffman runs, and I wanted to be able to run the analysis one step at a time. 

#local machine
setwd("/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims")
codesubpath<-"/Code/Transport-sims"
path_to_data<-paste0(getwd(),"/Code/Data")
path_to_output<-paste0(getwd(),"/Output")


source(paste0(getwd(),codesubpath,"/genstudysamp.R"))
source(paste0(getwd(),codesubpath,"/CreateWeights.R"))
source(paste0(getwd(),codesubpath,"/ApplyWeights.R"))
source(paste0(getwd(),codesubpath,"/RunOutcomeModels.R"))
source(paste0(getwd(),codesubpath,"/GetStdPop.R"))
source(paste0(getwd(),codesubpath,"/SumStats.R"))




#Run 1 iteration of 1 simulation scenario 4 to be able to analyze data for supplement:

Scenario<-4

#load target population and selection model parameters
load(paste0(path_to_data,"/targetpop_S",Scenario,".Rdata"))
load(paste0(path_to_data,"/params_beta0_S",Scenario,".Rdata"))

set.seed(10)
  
  #select random sample of target pop
  targetsamp<-slice_sample(targetpop, n=p$ntargetsamp)
  targetsamp$S<-0
  
  #generate summary stats for random sample of target pop
  targetstats<-SumStats(targetsamp)
  
  #Calculate standard population by C1, C2
  StddistC1C2<-GetStdPop(targetsamp)
  
  #use optimized S0 parameters to select study sample
  studysamp<-genstudysamp(data=targetpop, ntarget=p$ntarget,
                          beta_S_0=p$beta_S_0, beta_S_A=p$beta_S_A, 
                          beta_S_C1=p$beta_S_C1, beta_S_C2=p$beta_S_C2, 
                          beta_S_Z=p$beta_S_Z, 
                          beta_S_AC1=p$beta_S_AC1, beta_S_AC2=p$beta_S_AC2, 
                          beta_S_AZ=p$beta_S_AZ, 
                          beta_S_C1Z=p$beta_S_C1Z, beta_S_C2Z=p$beta_S_C2Z)
  
  
  #estimate effect of C1 on A and C2 on A in study sample:
  modA_C1<-glm(A~C1, family=binomial(link="logit"), data=studysamp)
  summary(modA_C1)
  
  modA_C2<-glm(A~C2, family=binomial(link="logit"), data=studysamp)
  summary(modA_C2)
  
  dat<-rbind(targetsamp, studysamp)
  dat2<-CreateWeights(dat, studysamp, p$Sform, p$Aform)
  
  #Truth in target sample
  modA_C1<-glm(A~C1, family=binomial(link="logit"), data=targetsamp)
  summary(modA_C1)
  
  modA_C2<-glm(A~C2, family=binomial(link="logit"), data=targetsamp)
  summary(modA_C2)
  
  #In weighted study sample there's a negative association instead of no association
  studysampwt<-dat2 %>% filter(S==1) 
  modA_C1<-glm(A~C1, family=quasibinomial(link="logit"), data=studysampwt, weights=wt_2)
  summary(modA_C1)
  
  modA_C2<-glm(A~C2, family=quasibinomial(link="logit"), data=studysampwt, weights=wt_2)
  summary(modA_C2)
  
  #Run 1 iteration of 1 simulation scenario 9 to be able to analyze data for supplement:
  
  Scenario<-9
  
  #load target population and selection model parameters
  load(paste0(path_to_data,"/targetpop_S",Scenario,".Rdata"))
  load(paste0(path_to_data,"/params_beta0_S",Scenario,".Rdata"))
  
  set.seed(10)
  
  #select random sample of target pop
  targetsamp<-slice_sample(targetpop, n=p$ntargetsamp)
  targetsamp$S<-0
  
  #generate summary stats for random sample of target pop
  targetstats<-SumStats(targetsamp)
  
  #Calculate standard population by C1, C2
  StddistC1C2<-GetStdPop(targetsamp)
  
  #use optimized S0 parameters to select study sample
  studysamp<-genstudysamp(data=targetpop, ntarget=p$ntarget,
                          beta_S_0=p$beta_S_0, beta_S_A=p$beta_S_A, 
                          beta_S_C1=p$beta_S_C1, beta_S_C2=p$beta_S_C2, 
                          beta_S_Z=p$beta_S_Z, 
                          beta_S_AC1=p$beta_S_AC1, beta_S_AC2=p$beta_S_AC2, 
                          beta_S_AZ=p$beta_S_AZ, 
                          beta_S_C1Z=p$beta_S_C1Z, beta_S_C2Z=p$beta_S_C2Z)
  
  
  
  
  #estimate effect of C1 on A and C2 on A in study sample:
  
  modZ_A_study<-lm(Z~A, data=studysamp)
  summary(modZ_A_study)

  # 
  # modZ_A_targetsamp<-lm(Z~A, data=targetsamp)
  # summary(modZ_A_targetsamp)
  #  
  modZ_A_target<-lm(Z~A, data=targetpop)
  summary(modZ_A_target)
  
  #Generate predictions using studysamp and targetpop
  OM_studysamp<-lm(as.formula(paste0("Y~",p$Yform)), data=studysamp)
  MM_study<-lm(as.formula(paste0("Z~",p$Zform)), data=studysamp)  
  MM_target<-lm(as.formula(paste0("Z~",p$Zform)), data=targetpop)
  
  
  
  #Create target samp replicates:
  
  targ_A1<- targetpop %>% mutate(A=1, rep="exp_copy")
  targ_A0<-targetpop %>% mutate(A=0, rep="unexp_copy")
  targreps<-rbind(targ_A1, targ_A0)  
  
  #True distribution of Z in targetpop by A
  summary(targetpop$Z[targetpop$A==1])
  summary(targetpop$Z[targetpop$A==0])
  
  #True distribution of Z in study by A
  summary(studysamp$Z[studysamp$A==1])
  summary(studysamp$Z[studysamp$A==0])
  
  
  #Overwrite Z using MM in study samp, then predict Y using predicted Z
  targreps$predZ_studymod<-predict.lm(MM_study,targreps)
  targreps$predZ_targmod<-predict.lm(MM_target,targreps)


  rbind(
    targetpop %>% group_by(A) %>% 
      summarise(mean_Z = mean(Z)) %>% 
      mutate(type = "true, target"), 
    studysamp %>% group_by(A) %>% 
      summarise(mean_Z = mean(Z)) %>% 
      mutate(type = "true, study sample"), 
    targreps %>% group_by(rep) %>% 
      summarise(mean_Z = mean(predZ_targmod)) %>% 
      mutate(type = "estimated, target", 
             A = ifelse(rep == "exp_copy", 1, 0)) %>% 
      select(-rep), 
    targreps %>% group_by(rep) %>% 
      summarise(mean_Z = mean(predZ_studymod)) %>% 
      mutate(type = "estimated, study sample",
             A = ifelse(rep == "exp_copy", 1, 0)) %>% 
      select(-rep)
  ) %>% 
    arrange(desc(type), desc(A)) %>% 
    mutate(mean_Z = round(mean_Z, 3)) %>% 
    write.xlsx(file = "/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/TableA5.xlsx")
  
