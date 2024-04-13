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
source(paste0(getwd(),codesubpath,"/GetDRestimates.R"))
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
  
  
  #Do transport analyses
  dat<-rbind(targetsamp, studysamp)
  dat2<-CreateWeights(dat, studysamp, targetsamp, p$Sform, p$Aform)
  WTres<-ApplyWeights(data=dat2, stdpop=StddistC1C2)
  OMres<-RunOutcomeModels(targetdata=targetsamp, studydata=studysamp, 
                          stdpop=StddistC1C2, Yform=p$Yform, Zform=p$Zform, 
                          YformnoZ=p$YformnoZ, Aform=p$Aform)
  DRres<-GetDRestimates(targetdata=targetsamp, studydata=studysamp, 
                        combineddata=dat2, Aform=p$Aform, Zform=p$Zform, Yform=p$Yform, 
                        YformnoZ=p$YformnoZ)
  
  
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
  
  
  
  #Do transport analyses
  dat<-rbind(targetsamp, studysamp)
  dat2<-CreateWeights(dat, studysamp, targetsamp, p$Sform, p$Aform)
  WTres<-ApplyWeights(data=dat2, stdpop=StddistC1C2)
  OMres<-RunOutcomeModels(targetdata=targetsamp, studydata=studysamp, 
                          stdpop=StddistC1C2, Yform=p$Yform, Zform=p$Zform, 
                          YformnoZ=p$YformnoZ, Aform=p$Aform)
  DRres<-GetDRestimates(targetdata=targetsamp, studydata=studysamp, 
                        combineddata=dat2, Aform=p$Aform, Zform=p$Zform, 
                        Yform=p$Yform, YformnoZ=p$YformnoZ)
    
  
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
  

  #Adding bootstrap code from analysis section
  
  #CHange this if need to overwrite # of bootstraps (n=2500 in full runs)
  #p$nboots<-2500
  
        #Create dataframe to save bootstrap estimates
        boot_res<-data.frame(STUD=rep(NA, p$nboots), 
                             STUD_YA1=rep(NA,p$nboots), 
                             STUD_YA0=rep(NA,p$nboots),
                             WT_1=rep(NA, p$nboots), 
                             WT_2=rep(NA, p$nboots), 
                             WT_3=rep(NA, p$nboots),  
                             WT_4=rep(NA, p$nboots),  
                             WT_1_YA1=rep(NA,p$nboots), 
                             WT_2_YA1=rep(NA,p$nboots), 
                             WT_3_YA1=rep(NA,p$nboots),
                             WT_4_YA1=rep(NA,p$nboots),
                             WT_1_YA0=rep(NA,p$nboots), 
                             WT_2_YA0=rep(NA,p$nboots), 
                             WT_3_YA0=rep(NA,p$nboots),
                             WT_4_YA0=rep(NA,p$nboots),
                             OM_1=rep(NA, p$nboots),  
                             OM_2=rep(NA, p$nboots),  
                             OM_3=rep(NA, p$nboots),  
                             OM_4=rep(NA, p$nboots), 
                             OM_5=rep(NA, p$nboots),
                             OM_6=rep(NA, p$nboots),
                             OM_1_YA1=rep(NA,p$nboots), 
                             OM_2_YA1=rep(NA,p$nboots), 
                             OM_3_YA1=rep(NA,p$nboots), 
                             OM_4_YA1=rep(NA,p$nboots), 
                             OM_5_YA1=rep(NA,p$nboots), 
                             OM_6_YA1=rep(NA,p$nboots), 
                             OM_1_YA0=rep(NA,p$nboots),
                             OM_2_YA0=rep(NA,p$nboots),
                             OM_3_YA0=rep(NA,p$nboots),
                             OM_4_YA0=rep(NA,p$nboots),
                             OM_5_YA0=rep(NA,p$nboots),
                             OM_6_YA0=rep(NA,p$nboots),
                             DR_1=rep(NA, p$nboots), 
                             DR_2=rep(NA, p$nboots), 
                             DR_3=rep(NA, p$nboots), 
                             DR_1_YA1=rep(NA,p$nboots), 
                             DR_2_YA1=rep(NA,p$nboots), 
                             DR_3_YA1=rep(NA,p$nboots),
                             DR_1_YA0=rep(NA,p$nboots), 
                             DR_2_YA0=rep(NA,p$nboots), 
                             DR_3_YA0=rep(NA,p$nboots))
        
        
        #run bootstrap
        for (j in 1:p$nboots){
          boot_targetsamp<-sample_n(targetsamp, nrow(targetsamp), replace=T) %>% mutate(S=0)
          boot_studysamp<-sample_n(studysamp, nrow(studysamp), replace=T)
          
          #ATE in bootstrap study sample
          #Y model and Z model
          boot_sampOM<-lm(as.formula(paste0("Y~",p$Yform)), data=boot_studysamp)
          boot_sampMM<-lm(as.formula(paste0("Z~",p$Zform)), data=boot_studysamp)  
          
          
          #Create boot study samp replicates:
          boot_studysamp_A1<- boot_studysamp %>% mutate(A=1, rep="exp_copy")
          boot_studysamp_A0<-boot_studysamp %>% mutate(A=0, rep="unexp_copy")
          boot_studysampreps<-rbind(boot_studysamp_A1, boot_studysamp_A0)  
          
          #Simulate Z in reps using MM, and predict Y using simulated Z.
          boot_studysampreps$Z<-predict.lm(boot_sampMM,boot_studysampreps)
          boot_studysampreps$Y_OM<-predict.lm(boot_sampOM,boot_studysampreps)
          
          #Calculate ATE
          boot_SampYs<-boot_studysampreps %>% group_by(rep) %>% summarise(Ymean=mean(Y_OM))
          boot_SMres<-data.frame(Study=as.numeric(boot_SampYs$Ymean[boot_SampYs$rep=="exp_copy"] - 
                                                    boot_SampYs$Ymean[boot_SampYs$rep=="unexp_copy"]))
          
          boot_SMres_YA1<-data.frame(Study=boot_SampYs$Ymean[boot_SampYs$rep=="exp_copy"])
          boot_SMres_YA0<-data.frame(Study=boot_SampYs$Ymean[boot_SampYs$rep=="unexp_copy"])
          
          
          #Run other transport analyses in boot samples
          boot_dat<-rbind(boot_targetsamp, boot_studysamp)
          boot_dat2<-CreateWeights(boot_dat, boot_studysamp,boot_targetsamp, p$Sform,p$Aform)
          boot_WTres<-ApplyWeights(boot_dat2, StddistC1C2)
          boot_OMres<-RunOutcomeModels(targetdata=boot_targetsamp,
                                       studydata=boot_studysamp,
                                       stdpop = StddistC1C2,
                                       Yform=p$Yform,
                                       Zform=p$Zform,
                                       YformnoZ=p$YformnoZ,
                                       Aform=p$Aform)
          boot_DRres<-GetDRestimates(targetdata=boot_targetsamp, studydata=boot_studysamp, 
                                     combineddata=boot_dat2, Aform=p$Aform, Zform=p$Zform, 
                                     Yform=p$Yform, YformnoZ=p$YformnoZ)
          #save boot estimates
          boot_res[j,]<-cbind(boot_SMres, boot_SMres_YA1, boot_SMres_YA0, boot_WTres, boot_OMres, boot_DRres)
          
        }
        
        
       #plotting
       boot_res_plot<-boot_res %>% select(STUD, WT_4, OM_6, OM_3, DR_2, DR_3) %>% mutate(boot_rep=c(1:2500)) %>%
                      pivot_longer(., cols=c("STUD", "WT_4", "OM_6", "OM_3", "DR_2", "DR_3"), names_to="Estimator")
       
       boot_res_plot$Estimator2<-factor(boot_res_plot$Estimator, levels=c("STUD", "WT_4", "OM_6","OM_3", "DR_2", "DR_3"), 
                                labels=c("SATE estimate", "IOSW", "OM1", "OM2", "DR1", "DR2"))
       
       hist_2500<-ggplot(boot_res_plot, aes(x=value)) +
         geom_histogram(position="identity", fill="grey40", colour="grey40", bins = 30) +
         facet_grid(. ~ Estimator2, scales="fixed") +
          geom_hline(yintercept = 0)+theme_bw() + ylab("Frequency")+xlab("Estimate")
       hist_2500
       
       ggsave(plot=hist_2500, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/boot_hist.pdf", width = 9, height=4, units="in")
       
       #Save CIs
        CIs<-apply(boot_res,2,quantile, probs=c(0.025, 0.975)) %>% data.frame() %>% mutate(n_boots=2500)
        bootSEs<-apply(boot_res,2,sd)
        
      
  
        
        
        #Re-run with  5000 bootstraps to compare CIs. 
        
        p$nboots<-5000
        set.seed(23456)
        
        
        #Create dataframe to save bootstrap estimates
        boot_res_5000<-data.frame(STUD=rep(NA, p$nboots), 
                             STUD_YA1=rep(NA,p$nboots), 
                             STUD_YA0=rep(NA,p$nboots),
                             WT_1=rep(NA, p$nboots), 
                             WT_2=rep(NA, p$nboots), 
                             WT_3=rep(NA, p$nboots),  
                             WT_4=rep(NA, p$nboots),  
                             WT_1_YA1=rep(NA,p$nboots), 
                             WT_2_YA1=rep(NA,p$nboots), 
                             WT_3_YA1=rep(NA,p$nboots),
                             WT_4_YA1=rep(NA,p$nboots),
                             WT_1_YA0=rep(NA,p$nboots), 
                             WT_2_YA0=rep(NA,p$nboots), 
                             WT_3_YA0=rep(NA,p$nboots),
                             WT_4_YA0=rep(NA,p$nboots),
                             OM_1=rep(NA, p$nboots),  
                             OM_2=rep(NA, p$nboots),  
                             OM_3=rep(NA, p$nboots),  
                             OM_4=rep(NA, p$nboots), 
                             OM_5=rep(NA, p$nboots),
                             OM_6=rep(NA, p$nboots),
                             OM_1_YA1=rep(NA,p$nboots), 
                             OM_2_YA1=rep(NA,p$nboots), 
                             OM_3_YA1=rep(NA,p$nboots), 
                             OM_4_YA1=rep(NA,p$nboots), 
                             OM_5_YA1=rep(NA,p$nboots), 
                             OM_6_YA1=rep(NA,p$nboots), 
                             OM_1_YA0=rep(NA,p$nboots),
                             OM_2_YA0=rep(NA,p$nboots),
                             OM_3_YA0=rep(NA,p$nboots),
                             OM_4_YA0=rep(NA,p$nboots),
                             OM_5_YA0=rep(NA,p$nboots),
                             OM_6_YA0=rep(NA,p$nboots),
                             DR_1=rep(NA, p$nboots), 
                             DR_2=rep(NA, p$nboots), 
                             DR_3=rep(NA, p$nboots), 
                             DR_1_YA1=rep(NA,p$nboots), 
                             DR_2_YA1=rep(NA,p$nboots), 
                             DR_3_YA1=rep(NA,p$nboots),
                             DR_1_YA0=rep(NA,p$nboots), 
                             DR_2_YA0=rep(NA,p$nboots), 
                             DR_3_YA0=rep(NA,p$nboots))
        
        
        #run bootstrap
        for (j in 1:p$nboots){
          boot_targetsamp<-sample_n(targetsamp, nrow(targetsamp), replace=T) %>% mutate(S=0)
          boot_studysamp<-sample_n(studysamp, nrow(studysamp), replace=T)
          
          #ATE in bootstrap study sample
          #Y model and Z model
          boot_sampOM<-lm(as.formula(paste0("Y~",p$Yform)), data=boot_studysamp)
          boot_sampMM<-lm(as.formula(paste0("Z~",p$Zform)), data=boot_studysamp)  
          
          
          #Create boot study samp replicates:
          boot_studysamp_A1<- boot_studysamp %>% mutate(A=1, rep="exp_copy")
          boot_studysamp_A0<-boot_studysamp %>% mutate(A=0, rep="unexp_copy")
          boot_studysampreps<-rbind(boot_studysamp_A1, boot_studysamp_A0)  
          
          #Simulate Z in reps using MM, and predict Y using simulated Z.
          boot_studysampreps$Z<-predict.lm(boot_sampMM,boot_studysampreps)
          boot_studysampreps$Y_OM<-predict.lm(boot_sampOM,boot_studysampreps)
          
          #Calculate ATE
          boot_SampYs<-boot_studysampreps %>% group_by(rep) %>% summarise(Ymean=mean(Y_OM))
          boot_SMres<-data.frame(Study=as.numeric(boot_SampYs$Ymean[boot_SampYs$rep=="exp_copy"] - 
                                                    boot_SampYs$Ymean[boot_SampYs$rep=="unexp_copy"]))
          
          boot_SMres_YA1<-data.frame(Study=boot_SampYs$Ymean[boot_SampYs$rep=="exp_copy"])
          boot_SMres_YA0<-data.frame(Study=boot_SampYs$Ymean[boot_SampYs$rep=="unexp_copy"])
          
          
          #Run other transport analyses in boot samples
          boot_dat<-rbind(boot_targetsamp, boot_studysamp)
          boot_dat2<-CreateWeights(boot_dat, boot_studysamp,boot_targetsamp, p$Sform,p$Aform)
          boot_WTres<-ApplyWeights(boot_dat2, StddistC1C2)
          boot_OMres<-RunOutcomeModels(targetdata=boot_targetsamp,
                                       studydata=boot_studysamp,
                                       stdpop = StddistC1C2,
                                       Yform=p$Yform,
                                       Zform=p$Zform,
                                       YformnoZ=p$YformnoZ,
                                       Aform=p$Aform)
          boot_DRres<-GetDRestimates(targetdata=boot_targetsamp, studydata=boot_studysamp, 
                                     combineddata=boot_dat2, Aform=p$Aform, Zform=p$Zform, 
                                     Yform=p$Yform, YformnoZ=p$YformnoZ)
          #save boot estimates
          boot_res_5000[j,]<-cbind(boot_SMres, boot_SMres_YA1, boot_SMres_YA0, boot_WTres, boot_OMres, boot_DRres)
          
        }
        
        #Save CIs and some histograms
        CIs_5000<-apply(boot_res_5000,2,quantile, probs=c(0.025, 0.975)) %>% data.frame() %>% mutate(n_boots=5000)
        
        bootSEs_5000<-apply(boot_res_5000,2,sd)
        
        
        #Compare CIs for 2500 vs. 5000 bootstraps
        CIs_all<-bind_rows(CIs, CIs_5000) %>% select(STUD, WT_4, OM_6, OM_3, DR_2, DR_3, n_boots) %>% mutate(bound=rep(c("LCI", "UCI"),2)) %>% 
          pivot_longer(., cols=c("STUD", "WT_4", "OM_6", "OM_3", "DR_2", "DR_3"), names_to="Estimator") %>% 
          pivot_wider(.,names_from = "bound")
        
        CIs_all$Estimator2<-factor(CIs_all$Estimator, levels=c("STUD", "WT_4", "OM_6","OM_3", "DR_2", "DR_3"), 
                                         labels=c("SATE estimate", "IOSW", "OM1", "OM2", "DR1", "DR2"))
        
        CIs_all$n_boots=factor(CIs_all$n_boots)
        
        CI_comp<-ggplot(CIs_all, aes(grp=n_boots, x=Estimator2, ymin=LCI, ymax=UCI, color=n_boots)) +
          geom_errorbar( aes(ymin = LCI, ymax = UCI), position = position_dodge(width = 0.5)) + ylim(0.2,0.6)+
          theme_bw() + xlab("Estimator") + ylab("Confidence interval") +labs(color="# Bootstraps")
        CI_comp
        
        
        ggsave(plot=CI_comp, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/boot_CIcomp.pdf", width = 9, height=4, units="in")
        
        
        rm(list = ls())
        
        
        #load in estimates to compare first 2500 and second 2500 iterations. 
        
        ##Code for looking at results
        substrRight <- function(x, nrmv){
          substr(x, nrmv, nchar(x))
        }
        
        options(scipen = 100)
        
        
        #Calculate summaries of results and samples
        i<-"S9"
          
          results<-read_csv(paste0("/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Output/sim_runs_",i,".csv"))
          colnames(results)<-c("Truth", "Truth_YA1", "Truth_YA0", 
                               
                               "STUD_PtEst", "STUD_LCI", "STUD_UCI", "STUD_bootSE",
                               "WT_1_PtEst", "WT_1_LCI", "WT_1_UCI", "WT_1_bootSE",
                               "WT_2_PtEst", "WT_2_LCI", "WT_2_UCI", "WT_2_bootSE",      
                               "WT_3_PtEst", "WT_3_LCI", "WT_3_UCI", "WT_3_bootSE",
                               "WT_4_PtEst", "WT_4_LCI", "WT_4_UCI", "WT_4_bootSE",
                               "OM_1_PtEst", "OM_1_LCI", "OM_1_UCI", "OM_1_bootSE",
                               "OM_2_PtEst", "OM_2_LCI","OM_2_UCI", "OM_2_bootSE",
                               "OM_3_PtEst", "OM_3_LCI", "OM_3_UCI", "OM_3_bootSE",
                               "OM_4_PtEst", "OM_4_LCI", "OM_4_UCI", "OM_4_bootSE",
                               "OM_5_PtEst", "OM_5_LCI", "OM_5_UCI", "OM_5_bootSE",
                               "OM_6_PtEst", "OM_6_LCI", "OM_6_UCI", "OM_6_bootSE",
                               "DR_1_PtEst", "DR_1_LCI", "DR_1_UCI", "DR_1_bootSE",
                               "DR_2_PtEst", "DR_2_LCI", "DR_2_UCI", "DR_2_bootSE",
                               "DR_3_PtEst", "DR_3_LCI", "DR_3_UCI", "DR_3_bootSE",
                               
                               "STUD_YA1_PtEst", "STUD_YA1_LCI", "STUD_YA1_UCI", "STUD_YA1_bootSE",
                               "WT_1_YA1_PtEst", "WT_1_YA1_LCI", "WT_1_YA1_UCI", "WT_1_YA1_bootSE",
                               "WT_2_YA1_PtEst", "WT_2_YA1_LCI", "WT_2_YA1_UCI", "WT_2_YA1_bootSE",      
                               "WT_3_YA1_PtEst", "WT_3_YA1_LCI", "WT_3_YA1_UCI", "WT_3_YA1_bootSE",
                               "WT_4_YA1_PtEst", "WT_4_YA1_LCI", "WT_4_YA1_UCI", "WT_4_YA1_bootSE",
                               "OM_1_YA1_PtEst", "OM_1_YA1_LCI", "OM_1_YA1_UCI", "OM_1_YA1_bootSE",
                               "OM_2_YA1_PtEst", "OM_2_YA1_LCI", "OM_2_YA1_UCI", "OM_2_YA1_bootSE",
                               "OM_3_YA1_PtEst", "OM_3_YA1_LCI", "OM_3_YA1_UCI", "OM_3_YA1_bootSE",
                               "OM_4_YA1_PtEst", "OM_4_YA1_LCI", "OM_4_YA1_UCI", "OM_4_YA1_bootSE",
                               "OM_5_YA1_PtEst", "OM_5_YA1_LCI", "OM_5_YA1_UCI", "OM_5_YA1_bootSE",
                               "OM_6_YA1_PtEst", "OM_6_YA1_LCI", "OM_6_YA1_UCI", "OM_6_YA1_bootSE",
                               "DR_1_YA1_PtEst", "DR_1_YA1_LCI", "DR_1_YA1_UCI", "DR_1_YA1_bootSE",
                               "DR_2_YA1_PtEst", "DR_2_YA1_LCI", "DR_2_YA1_UCI", "DR_2_YA1_bootSE",
                               "DR_3_YA1_PtEst", "DR_3_YA1_LCI", "DR_3_YA1_UCI", "DR_3_YA1_bootSE",
                               
                               "STUD_YA0_PtEst", "STUD_YA0_LCI", "STUD_YA0_UCI", "STUD_YA0_bootSE",
                               "WT_1_YA0_PtEst", "WT_1_YA0_LCI", "WT_1_YA0_UCI", "WT_1_YA0_bootSE",
                               "WT_2_YA0_PtEst", "WT_2_YA0_LCI", "WT_2_YA0_UCI", "WT_2_YA0_bootSE",      
                               "WT_3_YA0_PtEst", "WT_3_YA0_LCI", "WT_3_YA0_UCI", "WT_3_YA0_bootSE",
                               "WT_4_YA0_PtEst", "WT_4_YA0_LCI", "WT_4_YA0_UCI", "WT_4_YA0_bootSE",
                               "OM_1_YA0_PtEst", "OM_1_YA0_LCI", "OM_1_YA0_UCI", "OM_1_YA0_bootSE",
                               "OM_2_YA0_PtEst", "OM_2_YA0_LCI", "OM_2_YA0_UCI", "OM_2_YA0_bootSE",
                               "OM_3_YA0_PtEst", "OM_3_YA0_LCI", "OM_3_YA0_UCI", "OM_3_YA0_bootSE",
                               "OM_4_YA0_PtEst", "OM_4_YA0_LCI", "OM_4_YA0_UCI", "OM_4_YA0_bootSE",
                               "OM_5_YA0_PtEst", "OM_5_YA0_LCI", "OM_5_YA0_UCI", "OM_5_YA0_bootSE",
                               "OM_6_YA0_PtEst", "OM_6_YA0_LCI", "OM_6_YA0_UCI", "OM_6_YA0_bootSE",
                               "DR_1_YA0_PtEst", "DR_1_YA0_LCI", "DR_1_YA0_UCI", "DR_1_YA0_bootSE",
                               "DR_2_YA0_PtEst", "DR_2_YA0_LCI", "DR_2_YA0_UCI", "DR_2_YA0_bootSE",
                               "DR_3_YA0_PtEst", "DR_3_YA0_LCI", "DR_3_YA0_UCI", "DR_3_YA0_bootSE",
                               "n_targsamp", 
                               "C1prev_A0_targsamp", "C1prev_A1_targsamp", "C2mean_A0_targsamp", "C2mean_A1_targsamp", 
                               "Aprev_targsamp", 
                               "Zmean_targsamp", "Zsd_targsamp",  
                               "Ymean_targsamp", "Ysd_targsamp",
                               "n_studysamp", 
                               "C1prev_A0_studysamp", "C1prev_A1_studysamp", "C2mean_A0_studysamp", "C2mean_A1_studysamp", 
                               "Aprev_studysamp", 
                               "Zmean_studysamp", "Zsd_studysamp",  
                               "Ymean_studysamp", "Ysd_studysamp", "time_min", "batch", "seed")
          
          results <- results %>% na.omit 
          
          results_supp<-read_csv(paste0("/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Output/sim_runs_",i,"_supp.csv"))
          colnames(results_supp)<-colnames(results)
          results_supp <- results_supp %>% na.omit
          
          #Supplement missing rows
          results<-rbind(results, results_supp[1:(5000-nrow(results)),])
          
          results <- results %>% na.omit %>% 
            mutate(STUD_CIcov = case_when(STUD_LCI <= Truth & STUD_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   WT_1_CIcov = case_when(WT_1_LCI <= Truth & WT_1_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   WT_2_CIcov = case_when(WT_2_LCI <= Truth & WT_2_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   WT_3_CIcov = case_when(WT_3_LCI <= Truth & WT_3_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   WT_4_CIcov = case_when(WT_4_LCI <= Truth & WT_4_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   OM_1_CIcov = case_when(OM_1_LCI <= Truth & OM_1_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   OM_2_CIcov = case_when(OM_2_LCI <= Truth & OM_2_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   OM_3_CIcov = case_when(OM_3_LCI <= Truth & OM_3_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   OM_4_CIcov = case_when(OM_4_LCI <= Truth & OM_4_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   OM_5_CIcov = case_when(OM_5_LCI <= Truth & OM_5_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   OM_6_CIcov = case_when(OM_6_LCI <= Truth & OM_6_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   DR_1_CIcov = case_when(DR_1_LCI <= Truth & DR_1_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   DR_2_CIcov = case_when(DR_2_LCI <= Truth & DR_2_UCI>=Truth ~ 1,
                                          TRUE ~ 0),
                   DR_3_CIcov = case_when(DR_3_LCI <= Truth & DR_3_UCI>=Truth ~ 1,
                                          TRUE ~ 0))
          
          
          
          #Split results into first and second half.
          results_1<-results[1:2500,]
          results_2<-results[2501:5000,]
          
          
          results_clean<-function(res_dat){
          
          results
          #HOw to calculate MSE/RMSE? Use CIs to obtain variance or use SD of estimates directly?
          SDs <- res_dat %>% summarise(STUD_empSE = sd(STUD_PtEst),
                                       WT_1_empSE = sd(WT_1_PtEst),
                                       WT_2_empSE = sd(WT_2_PtEst),
                                       WT_3_empSE = sd(WT_3_PtEst),
                                       WT_4_empSE = sd(WT_4_PtEst),
                                       OM_1_empSE = sd(OM_1_PtEst),
                                       OM_2_empSE = sd(OM_2_PtEst),
                                       OM_3_empSE = sd(OM_3_PtEst),
                                       OM_4_empSE = sd(OM_4_PtEst),
                                       OM_5_empSE = sd(OM_5_PtEst),
                                       OM_6_empSE = sd(OM_6_PtEst),
                                       DR_1_empSE = sd(DR_1_PtEst),
                                       DR_2_empSE = sd(DR_2_PtEst),
                                       DR_3_empSE = sd(DR_3_PtEst))
          
          summary<-SDs %>% cbind(data.frame(t(colMeans(res_dat))), .) %>% 
            
            mutate(STUD_bias = (STUD_PtEst-Truth),
                          WT_1_bias = (WT_1_PtEst-Truth),
                          WT_2_bias = (WT_2_PtEst-Truth),
                          WT_3_bias = (WT_3_PtEst-Truth),
                          WT_4_bias = (WT_4_PtEst-Truth),
                          OM_1_bias = (OM_1_PtEst-Truth),
                          OM_2_bias = (OM_2_PtEst-Truth),
                          OM_3_bias = (OM_3_PtEst-Truth),
                          OM_4_bias = (OM_4_PtEst-Truth),
                          OM_5_bias = (OM_5_PtEst-Truth),
                          OM_6_bias = (OM_6_PtEst-Truth),
                          DR_1_bias = (DR_1_PtEst-Truth),
                          DR_2_bias = (DR_2_PtEst-Truth),
                          DR_3_bias = (DR_3_PtEst-Truth),
                          
                          STUD_pctbias = 100*(STUD_PtEst-Truth)/Truth, 
                          WT_1_pctbias = 100*(WT_1_PtEst-Truth)/Truth,
                          WT_2_pctbias = 100*(WT_2_PtEst-Truth)/Truth,
                          WT_3_pctbias = 100*(WT_3_PtEst-Truth)/Truth,
                          WT_4_pctbias = 100*(WT_4_PtEst-Truth)/Truth,
                          OM_1_pctbias = 100*(OM_1_PtEst-Truth)/Truth,
                          OM_2_pctbias = 100*(OM_2_PtEst-Truth)/Truth,
                          OM_3_pctbias = 100*(OM_3_PtEst-Truth)/Truth,
                          OM_4_pctbias = 100*(OM_4_PtEst-Truth)/Truth, 
                          OM_5_pctbias = 100*(OM_5_PtEst-Truth)/Truth, 
                          OM_6_pctbias = 100*(OM_6_PtEst-Truth)/Truth, 
                          DR_1_pctbias = 100*(DR_1_PtEst-Truth)/Truth, 
                          DR_2_pctbias = 100*(DR_2_PtEst-Truth)/Truth, 
                          DR_3_pctbias = 100*(DR_3_PtEst-Truth)/Truth, 
                          
                          STUD_SEfromCI= (STUD_UCI-STUD_LCI)/3.92,
                          WT_1_SEfromCI= (WT_1_UCI-WT_1_LCI)/3.92,
                          WT_2_SEfromCI= (WT_2_UCI-WT_2_LCI)/3.92,
                          WT_3_SEfromCI= (WT_3_UCI-WT_3_LCI)/3.92,
                          WT_4_SEfromCI= (WT_4_UCI-WT_4_LCI)/3.92,
                          OM_1_SEfromCI= (OM_1_UCI-OM_1_LCI)/3.92,
                          OM_2_SEfromCI= (OM_2_UCI-OM_2_LCI)/3.92,
                          OM_3_SEfromCI= (OM_3_UCI-OM_3_LCI)/3.92,
                          OM_4_SEfromCI= (OM_4_UCI-OM_4_LCI)/3.92,
                          OM_5_SEfromCI= (OM_5_UCI-OM_5_LCI)/3.92,
                          OM_6_SEfromCI= (OM_6_UCI-OM_6_LCI)/3.92,
                          DR_1_SEfromCI= (DR_1_UCI-DR_1_LCI)/3.92,
                          DR_2_SEfromCI= (DR_2_UCI-DR_2_LCI)/3.92,
                          DR_3_SEfromCI= (DR_3_UCI-DR_3_LCI)/3.92,
                          
                          STUD_RMSE = sqrt((STUD_PtEst - Truth)^2 + STUD_empSE^2),
                          WT_1_RMSE = sqrt((WT_1_PtEst - Truth)^2 + WT_1_empSE^2),
                          WT_2_RMSE = sqrt((WT_2_PtEst - Truth)^2 + WT_2_empSE^2),
                          WT_3_RMSE = sqrt((WT_3_PtEst - Truth)^2 + WT_3_empSE^2),
                          WT_4_RMSE = sqrt((WT_4_PtEst - Truth)^2 + WT_4_empSE^2),
                          OM_1_RMSE = sqrt((OM_1_PtEst - Truth)^2 + OM_1_empSE^2),
                          OM_2_RMSE = sqrt((OM_2_PtEst - Truth)^2 + OM_2_empSE^2),
                          OM_3_RMSE = sqrt((OM_3_PtEst - Truth)^2 + OM_3_empSE^2),
                          OM_4_RMSE = sqrt((OM_4_PtEst - Truth)^2 + OM_4_empSE^2),
                          OM_5_RMSE = sqrt((OM_5_PtEst - Truth)^2 + OM_5_empSE^2),
                          OM_6_RMSE = sqrt((OM_6_PtEst - Truth)^2 + OM_6_empSE^2),
                          DR_1_RMSE = sqrt((DR_1_PtEst - Truth)^2 + DR_1_empSE^2),
                          DR_2_RMSE = sqrt((DR_2_PtEst - Truth)^2 + DR_2_empSE^2),
                          DR_3_RMSE = sqrt((DR_3_PtEst - Truth)^2 + DR_3_empSE^2),
            )
          
          summary2<-summary %>% select(grep("STUD", names(.)),
                                       grep("WT_", names(.)),
                                       grep("OM_", names(.)),
                                       grep("DR_", names(.))) %>% t() %>%  data.frame() 
          
          summary2$Model <-substr(rownames(summary2),1,4)
          summary2$Metric<-substrRight(rownames(summary2), 6) 
          
          summary3<-summary2 %>% pivot_wider(.,id_cols=c("Model"), 
                                             names_from="Metric", values_from=".") %>% 
            mutate(Truth=mean(results$Truth), 
                   YA1_Truth=mean(results$Truth_YA1), 
                   YA0_Truth=mean(results$Truth_YA0)) %>%
            mutate(Scenario = i) %>%
            select(Scenario, Model, PtEst, LCI, UCI, empSE, bootSE, 
                   SEfromCI, bias, pctbias, CIcov, RMSE)
          
         Final_res<-summary3

         return(Final_res)}

          Final_res1<-results_clean(results_1) %>% mutate(Split="1 (first n=2500)")          
          Final_res2<-results_clean(results_2) %>% mutate(Split="2 (second n=2500)")          
          Final_res_overall<-results_clean(results) %>% mutate(Split="Combined (n=5000)")
          
          Final_res_comp<-bind_rows(Final_res1, Final_res2, Final_res_overall) %>% select(Split, Model, bias, pctbias) %>%    
              filter(Model!="Truth")  %>% filter(Model %in% c("STUD", "WT_4", "OM_6", "OM_3", "DR_2", "DR_3"))
          
          Final_res_comp$Model2<-factor(Final_res_comp$Model, levels=c("STUD", "WT_4", "OM_6","OM_3", "DR_2", "DR_3"), 
                                     labels=c("SATE estimate", "IOSW", "OM1", "OM2", "DR1", "DR2"))
          
          Final_res_comp$Split=factor(Final_res_comp$Split)
          
          n_iterations_comp<-ggplot(Final_res_comp) + 
            geom_point(aes(group=Split, color=Split, x=Model2, y=bias), 
                       position=position_dodge(width=0.5)) + theme_bw() +
            xlab("Estimator")+ylab("Bias in TATE estimate")+labs(color="Replicate split")
          n_iterations_comp
          
          ggsave(plot=n_iterations_comp, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/niter_comp.pdf", width = 9, height=4, units="in")
          
          
          
          #plot showing normal distributions of estimates
          
          results_hist<-results[,str_detect(colnames(results), "PtEst")] %>% 
            rename_with(~ str_remove(., "_PtEst"), everything()) %>% 
                              select(STUD, WT_4, OM_6, OM_3, DR_2, DR_3) %>% 
            pivot_longer(., cols=c("STUD", "WT_4", "OM_6", "OM_3", "DR_2", "DR_3"), names_to="Estimator")
          
          results_hist$Estimator2<-factor(results_hist$Estimator, levels=c("STUD", "WT_4", "OM_6","OM_3", "DR_2", "DR_3"), 
                                        labels=c("SATE estimate", "IOSW", "OM1", "OM2", "DR1", "DR2"))
          
          
          hist_niter<-ggplot(results_hist, aes(x=value)) +
            geom_histogram(position="identity", fill="grey40", colour="grey40", bins = 30) +
            facet_grid(. ~ Estimator2, scales="fixed") + geom_hline(yintercept=0)+
            theme_bw() + ylab("Frequency")+xlab("Estimate")
          hist_niter
          
          ggsave(plot=hist_niter, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/iter_hist.pdf", width = 9, height=4, units="in")
          
