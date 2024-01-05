#ScenarioParams

rm(list = ls())

params<-data.frame(SimNo=NA, 
                   # A model
                   beta_A_C1=NA, beta_A_C2=NA, Aform=NA,
                   # Z model
                   beta_Z_A=NA, Zform=NA,
                   # Y model
                   beta_Y_A=NA, beta_Y_Z=NA, beta_Y_C1=NA, beta_Y_C2=NA, 
                   beta_Y_AC1=NA, beta_Y_AC2=NA, beta_Y_AZ=NA, beta_Y_C1Z=NA, beta_Y_C2Z=NA, Yform=NA, YformnoZ=NA,
                   # S model
                   beta_S_A=NA, beta_S_C1=NA, beta_S_C2=NA, beta_S_Z=NA, 
                   beta_S_AC1=NA, beta_S_AC2=NA, beta_S_AZ=NA, beta_S_C1Z=NA, beta_S_C2Z=NA, Sform=NA)
                

params[1,]<-c("S1", 
              0, 0, "1",
              0, "1", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2", 
              0, 1.5, 0.00075, 0, 0, 0, 0, 0, 0, "C1+C2"
              )

params[2,]<-c("S2", 
              0, 0, "1",
              0, "1", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              1, 1.5, 0.00075, 0, 0, 0, 0, 0, 0, "A+C1+C2+A*C1+A*C2"
              )

params[3,]<-c("S3", 
              0.65, 0.01, "C1+C2",
              0, "1", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              0, 1.5, 0.00075, 0, 0, 0, 0, 0, 0, "C1+C2"
              )

params[4,]<-c("S4", 
              0.65, 0.01, "C1+C2",
              0, "1", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              0.75, 1, 0.0075, 0, 0.4, 0.005, 0, 0, 0, "A+C1+C2+A*C1+A*C2"
              )

params[5,]<-c("S5",
              0.65, 0.01, "C1+C2",
              0, "1",
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              0, 1, 0.0075, 0.01, 0, 0, 0, 0, 0, "C1+C2+Z"
              )

params[6,]<-c("S6",
              0.65, 0.01, "C1+C2",
              0, "1",
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              .75, 1, 0.0075, 0.01, 0.4, 0.005, 0.005, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z"
              )

params[7,]<-c("S7", 
              0.65, 0.01, "C1+C2",
              10, "A", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              0, 1, 0.0075, 0, 0, 0, 0, 0, 0, "C1+C2"
              )

params[8,]<-c("S8", 
              0.65, 0.01, "C1+C2",
              10, "A", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              0.75, 1, 0.0075, 0, 0.4, 0.005, 0, 0, 0, "A+C1+C2+A*C1+A*C2"
              )
params[9,]<-c("S9", 
              0.65, 0.01, "C1+C2",
              10, "A", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0.03, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", "A+C1+C2+A*C1+A*C2",
              0.75, 1, 0.0075, 0.01, 0.4, 0.005, 0.005, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z"
              )


params[10,]<-c("S10", 
              0.65, 0.01, "C1+C2",
              10, "A", 
              0.15, 0.005, 0.5, 0.002, 0.1, 0.005, 0, 0, 0, "A+C1+C2+Z+A*C1+A*C2", "A+C1+C2+A*C1+A*C2",
              0.75, 1, 0.0075, 0.01, 0.4, 0.005, 0.005, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z"
)

#In case I want to add more scenarios later. 
# params[10,]<-c("S10", 
#               0.5, 0.001, "C1+C2",
#               0, "1", 
#               0.13, 0.005, 0.5, 0.002, 0.2, 0.0005, 0.005, 0, 0, "A+C1+C2+Z+A*C1+A*C2+A*Z", 
#               1, 1.5, 0.00075, 0, 0, 0, 0, 0, 0, "A+C1+C2"
# )
# 

#Params that are the same across scenarios
params$ntarget<-6000000
params$ntargetsamp<-20000
params$nstudysamp<-3000
params$niter<-10
params$nboots<-2500

params$Aprop<-.45
params$Zmean<-0
params$Ymean<-0

params$C1prev<-0.2
params$C2form<-"runif(ntarget, -15, 15)"

for (i in grep("beta",colnames(params))){
  params[,i]<-as.numeric(params[,i])
}

#Double the parameters list to make sensitivity analysis with identical parameters except distribution of C1, C2
params<-rbind(params,params)
params$SimNo[11:20]<-c("S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18", "S19","S20")
params$C1prev[11:20]<-0.1
params$C2form[11:20]<-"rnorm(ntarget,0,4)"



path_to_data<-"/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Code/Data"

save(params, file=paste0(path_to_data,"/params.Rdata"))
