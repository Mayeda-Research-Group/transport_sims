################
#Filepaths and other initializations
#################

#Analysis 
library(tidyverse)


#Update directories for hoffman runs

#local machine
# setwd("/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims")
# codesubpath<-"/Code/Transport-sims"
# path_to_data<-paste0(getwd(),"/Code/Data")
# path_to_output<-paste0(getwd(),"/Output")

#hoffman
setwd("/u/home/e/ehayesla/Transport_sims/")
codesubpath<-"/Scripts"
path_to_data<-paste0(getwd(),"/Data")
path_to_output<-paste0(getwd(),"/Results")

source(paste0(getwd(),codesubpath,"/genstudysamp.R"))
source(paste0(getwd(),codesubpath,"/CreateWeights.R"))
source(paste0(getwd(),codesubpath,"/ApplyWeights.R"))
source(paste0(getwd(),codesubpath,"/RunOutcomeModels.R"))
source(paste0(getwd(),codesubpath,"/GetStdPop.R"))
source(paste0(getwd(),codesubpath,"/SumStats.R"))




##############
#Function to run
###############

RunSim<-function(Scenario){


#start timing
start<-Sys.time()



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


#Get estimate of ATE in study sample for A on Y with g-computation
    
    #Y model and Z model
      sampOM<-lm(as.formula(paste0("Y~",p$Yform)), data=studysamp)
      sampMM<-lm(as.formula(paste0("Z~",p$Zform)), data=studysamp)  
      
    
    #Create study sample replicates:
      studysamp_A1<- studysamp %>% mutate(A=1, rep="exp_copy")
      studysamp_A0<-studysamp %>% mutate(A=0, rep="unexp_copy")
      studysampreps<-rbind(studysamp_A1, studysamp_A0)  
      
    #Simulate Z in reps using MM, and predict Y using simulated Z.
      studysampreps$Z<-predict.lm(sampMM,studysampreps)
      studysampreps$Y_OM<-predict.lm(sampOM,studysampreps)
      
    #Calculate ATE
      SampYs<-studysampreps %>% group_by(rep) %>% summarise(Ymean=mean(Y_OM))
      SMres<-data.frame(STUD=as.numeric(SampYs$Ymean[SampYs$rep=="exp_copy"] - 
                          SampYs$Ymean[SampYs$rep=="unexp_copy"]))
      
      SMres_YA1<-data.frame(STUD_YA1=SampYs$Ymean[SampYs$rep=="exp_copy"])
      SMres_YA0<-data.frame(STUD_YA0=SampYs$Ymean[SampYs$rep=="unexp_copy"])
      
#Generate summary stats for study sample
  samplestats<-SumStats(studysamp)
    
    
#Do transport analyses
  dat<-rbind(targetsamp, studysamp)
  dat2<-CreateWeights(dat, studysamp, p$Sform, p$Aform)
  WTres<-ApplyWeights(data=dat2, stdpop=StddistC1C2)
  OMres<-RunOutcomeModels(targetdata=targetsamp, studydata=studysamp, 
                          stdpop=StddistC1C2, Yform=p$Yform, Zform=p$Zform, 
                          YformnoZ=p$YformnoZ)
  PtEst<-cbind(SMres, SMres_YA1, SMres_YA0, WTres, OMres)

#Bootstrap to get CIs.
  
  #CHange this if need to overwrite # of bootstraps (n=2500 in full runs)
  p$nboots<-2500
  
      #Create dataframe to save bootstrap estimates
        boot_res<-data.frame(STUD=rep(NA, p$nboots), 
                             STUD_YA1=rep(NA,p$nboots), 
                             STUD_YA0=rep(NA,p$nboots),
                             WT_1=rep(NA, p$nboots), 
                             WT_2=rep(NA, p$nboots), 
                             WT_3=rep(NA, p$nboots),  
                             WT_1_YA1=rep(NA,p$nboots), 
                             WT_2_YA1=rep(NA,p$nboots), 
                             WT_3_YA1=rep(NA,p$nboots),
                             WT_1_YA0=rep(NA,p$nboots), 
                             WT_2_YA0=rep(NA,p$nboots), 
                             WT_3_YA0=rep(NA,p$nboots),
                             OM_1=rep(NA, p$nboots),  
                             OM_2=rep(NA, p$nboots),  
                             OM_3=rep(NA, p$nboots),  
                             OM_4=rep(NA, p$nboots), 
                             OM_5=rep(NA, p$nboots),
                             OM_1_YA1=rep(NA,p$nboots), 
                             OM_2_YA1=rep(NA,p$nboots), 
                             OM_3_YA1=rep(NA,p$nboots), 
                             OM_4_YA1=rep(NA,p$nboots), 
                             OM_5_YA1=rep(NA,p$nboots), 
                             OM_1_YA0=rep(NA,p$nboots),
                             OM_2_YA0=rep(NA,p$nboots),
                             OM_3_YA0=rep(NA,p$nboots),
                             OM_4_YA0=rep(NA,p$nboots),
                             OM_5_YA0=rep(NA,p$nboots))
        
        
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
        boot_dat2<-CreateWeights(boot_dat, boot_studysamp,p$Sform,p$Aform)
        boot_WTres<-ApplyWeights(boot_dat2, StddistC1C2)
        boot_OMres<-RunOutcomeModels(targetdata=boot_targetsamp,
                                     studydata=boot_studysamp,
                                     stdpop = StddistC1C2,
                                     Yform=p$Yform,
                                     Zform=p$Zform,
                                     YformnoZ=p$YformnoZ)
        
        #save boot estimates
        boot_res[j,]<-cbind(boot_SMres, boot_SMres_YA1, boot_SMres_YA0, boot_WTres, boot_OMres)
        
        }
        
      #estimate CIs from bootstraps
      CIs<-apply(boot_res,2,quantile, probs=c(0.025, 0.975))
      
      bootSEs<-apply(boot_res,2,sd)

#Aggregate, format, and save results      
    results<-rbind(PtEst, CIs, bootSEs) %>% cbind(data.frame(est=c("PtEst", "LCI", "UCI", "SE"))) %>% 
      pivot_wider(.,names_from=est, values_from=c("STUD", "WT_1", "WT_2", "WT_3", "OM_1", "OM_2", "OM_3", "OM_4", "OM_5",
                                                  "STUD_YA1", "WT_1_YA1", "WT_2_YA1", "WT_3_YA1", "OM_1_YA1", "OM_2_YA1", "OM_3_YA1", "OM_4_YA1", "OM_5_YA1",
                                                  "STUD_YA0", "WT_1_YA0", "WT_2_YA0", "WT_3_YA0", "OM_1_YA0", "OM_2_YA0", "OM_3_YA0", "OM_4_YA0", "OM_5_YA0")) 
    
    #Stop timiming and calculate run time
    time_min<-as.numeric(difftime(Sys.time(), start, units = "mins"))   
    
    results<-cbind(data.frame(data.frame(p$Truth), data.frame(p$Truth_YA1), data.frame(p$Truth_YA0),
                              results, targetstats, samplestats, time_min, row, seed))
    colnames(results)<-c("Truth", "Truth_YA1", "Truth_YA0", 
                         
                         "STUD_PtEst", "STUD_LCI", "STUD_UCI", "STUD_bootSE",
                         "WT_1_PtEst", "WT_1_LCI", "WT_1_UCI", "WT_1_bootSE",
                         "WT_2_PtEst", "WT_2_LCI", "WT_2_UCI", "WT_2_bootSE",      
                         "WT_3_PtEst", "WT_3_LCI", "WT_3_UCI", "WT_3_bootSE",
                         "OM_1_PtEst", "OM_1_LCI", "OM_1_UCI", "OM_1_bootSE",
                         "OM_2_PtEst", "OM_2_LCI","OM_2_UCI", "OM_2_bootSE",
                         "OM_3_PtEst", "OM_3_LCI", "OM_3_UCI", "OM_3_bootSE",
                         "OM_4_PtEst", "OM_4_LCI", "OM_4_UCI", "OM_4_bootSE",
                         "OM_5_PtEst", "OM_5_LCI", "OM_5_UCI", "OM_5_bootSE",
                         
                         "STUD_YA1_PtEst", "STUD_YA1_LCI", "STUD_YA1_UCI", "STUD_YA1_bootSE",
                         "WT_1_YA1_PtEst", "WT_1_YA1_LCI", "WT_1_YA1_UCI", "WT_1_YA1_bootSE",
                         "WT_2_YA1_PtEst", "WT_2_YA1_LCI", "WT_2_YA1_UCI", "WT_2_YA1_bootSE",      
                         "WT_3_YA1_PtEst", "WT_3_YA1_LCI", "WT_3_YA1_UCI", "WT_3_YA1_bootSE",
                         "OM_1_YA1_PtEst", "OM_1_YA1_LCI", "OM_1_YA1_UCI", "OM_1_YA1_bootSE",
                         "OM_2_YA1_PtEst", "OM_2_YA1_LCI", "OM_2_YA1_UCI", "OM_2_YA1_bootSE",
                         "OM_3_YA1_PtEst", "OM_3_YA1_LCI", "OM_3_YA1_UCI", "OM_3_YA1_bootSE",
                         "OM_4_YA1_PtEst", "OM_4_YA1_LCI", "OM_4_YA1_UCI", "OM_4_YA1_bootSE",
                         "OM_5_YA1_PtEst", "OM_5_YA1_LCI", "OM_5_YA1_UCI", "OM_5_YA1_bootSE",
                         
                         "STUD_YA0_PtEst", "STUD_YA0_LCI", "STUD_YA0_UCI", "STUD_YA0_bootSE",
                         "WT_1_YA0_PtEst", "WT_1_YA0_LCI", "WT_1_YA0_UCI", "WT_1_YA0_bootSE",
                         "WT_2_YA0_PtEst", "WT_2_YA0_LCI", "WT_2_YA0_UCI", "WT_2_YA0_bootSE",      
                         "WT_3_YA0_PtEst", "WT_3_YA0_LCI", "WT_3_YA0_UCI", "WT_3_YA0_bootSE",
                         "OM_1_YA0_PtEst", "OM_1_YA0_LCI", "OM_1_YA0_UCI", "OM_1_YA0_bootSE",
                         "OM_2_YA0_PtEst", "OM_2_YA0_LCI", "OM_2_YA0_UCI", "OM_2_YA0_bootSE",
                         "OM_3_YA0_PtEst", "OM_3_YA0_LCI", "OM_3_YA0_UCI", "OM_3_YA0_bootSE",
                         "OM_4_YA0_PtEst", "OM_4_YA0_LCI", "OM_4_YA0_UCI", "OM_4_YA0_bootSE",
                         "OM_5_YA0_PtEst", "OM_5_YA0_LCI", "OM_5_YA0_UCI", "OM_5_YA0_bootSE",
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

    #Save CSV output, with column headers if file does not exist, without if it does.
    if (file.exists(paste0(path_to_output,"/sim_runs_S",Scenario,".csv"))==F){ 
        write.table(results, paste0(path_to_output,"/sim_runs_S",Scenario,".csv"), 
                    col.names=T, row.names=F, sep=",", append = TRUE)                            
    } else {
      
      write.table(results, 
                  paste0(path_to_output,"/sim_runs_S",Scenario,".csv"), col.names=F, row.names=F, sep=",", append = TRUE)
    }                            
}


#################
#Prep to run on Hoffman!
################
    ## Read in the arguments listed in the:
    ## R CMD BATCH --no-save --no-restore '--args ...'
    ## expression:
    args=(commandArgs(TRUE))
    
    ## args is now a list of character vectors
    
    ## Check to see if arguments are passed and set default values if not,
    ## then cycle through each element of the list and evaluate the expressions.
    if(length(args)==0){
      print("No arguments supplied.")
      ##supply default values
      row = "1"
    }else{
      for(i in 1:length(args)){
        eval(parse(text=args[[i]]))
      }
    }
    ## Now print values just to make sure:
    print(row)

##################
  #Run code!
##################
  
  scen_seed<-read_csv(paste0(path_to_data,"/scen_seed_list.csv"))
  Scenario<-as.numeric(scen_seed[row,"Scenario"])
  seed<-as.numeric(scen_seed[row,"seed"])
  
  #load target population and selection model parameters
  load(paste0(path_to_data,"/targetpop_S",Scenario,".Rdata"))
  load(paste0(path_to_data,"/params_beta0_S",Scenario,".Rdata"))
    
  set.seed(seed)
  replicate(20, RunSim(Scenario=Scenario))





#Unit tests
# 
# Scenarios<-c("S1")#, "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")
# Scenario<-"6"
# for (j in Scenarios){
#   for (i in 1:10){
#     set.seed(12345)
# replicate(5, RunSim(Scenario=Scenario))
#   }
# }
# 

