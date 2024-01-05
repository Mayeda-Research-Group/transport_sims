#RunTargetCreation

require(tidyverse)
require(here)

source(paste0(here(),"/gentargetpop.R"))
source(paste0(here(),"/CreateTarget.R"))


#Load parameters
path_to_data<-"/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Code/Data"
load(paste0(path_to_data,"/params.Rdata"))

CreateTarget(Scenario="S1")
CreateTarget(Scenario="S2")
CreateTarget(Scenario="S3")
CreateTarget(Scenario="S4")
CreateTarget(Scenario="S5")
CreateTarget(Scenario="S6")
CreateTarget(Scenario="S7")
CreateTarget(Scenario="S8")
CreateTarget(Scenario="S9")
CreateTarget(Scenario="S10")

CreateTarget(Scenario="S11")
CreateTarget(Scenario="S12")
CreateTarget(Scenario="S13")
CreateTarget(Scenario="S14")
CreateTarget(Scenario="S15")
CreateTarget(Scenario="S16")
CreateTarget(Scenario="S17")
CreateTarget(Scenario="S18")
CreateTarget(Scenario="S19")
CreateTarget(Scenario="S20")

#check
# paramcheck<-data.frame(matrix(ncol=41))
# for (i in 1:20){
#   load(paste0(path_to_data,"/params_beta0_S",i,".Rdata"))
#   paramcheck[i,]<-p
#   colnames(paramcheck)<-colnames(p)
# }
# 
# 
# paramcheck2<-data.frame(matrix(ncol=39))
# for (i in 1:10){
#   load(paste0(path_to_data,"/Old/params_beta0_S",i,".Rdata"))
#   paramcheck2[i,]<-p
#   colnames(paramcheck2)<-colnames(p)
# }
# 
# 
# comp<-cbind(paramcheck2$beta_S_0, paramcheck$beta_S_0)
# 
