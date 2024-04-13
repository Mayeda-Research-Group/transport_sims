CreateWeights<-function(combineddata, studydata, targetdata, Sform, Aform){
  
  #Create S model
  Smod<-glm(as.formula(paste0("S~",Sform)), data=combineddata, family = binomial())
  
  #unconditional odds of S
  odds_S<-(sum(combineddata$S)/nrow(combineddata))/(1-(sum(combineddata$S)/nrow(combineddata)))
  
  #Create A model
  #in study sample
  Amod<-glm(as.formula(paste0("A~",Aform)), data=studydata, family = binomial())
  
  #new a model in target sample
  Amod_target<-glm(as.formula(paste0("A~",Aform)), data=targetdata, family = binomial())
  
  #Create weights
  combineddata$pS<-predict.glm(Smod, combineddata, type="response") #predicted probability of S=1 | C, A, Z, A*Z
  combineddata$predA<-predict.glm(Amod, combineddata, type="response") #predicted probability of getting A
  combineddata$pA<-ifelse(combineddata$A==1, combineddata$predA, 1-combineddata$predA) #predicted probability of treatment you actually got. 
  combineddata$wt_1<-((1-combineddata$pS)/combineddata$pS)*odds_S
  combineddata$wt_2_uns<-((1-combineddata$pS)/combineddata$pS)*(1/combineddata$pA)

  stb_factor1<- combineddata %>% filter(S==1) %>% summarise(mean_wts_uns = mean(wt_2_uns)) %>% as.numeric()

  combineddata$wt_2<-combineddata$wt_2_uns/stb_factor1
  
  #Adding new IPTW developed in target sample. 
  combineddata$predA_targmodel<-predict.glm(Amod_target, combineddata, type="response") #predicted probability of getting A
  combineddata$pA_targmodel<-ifelse(combineddata$A==1, combineddata$predA_targmodel, 1-combineddata$predA_targmodel) #predicted probability of treatment you actually got.

  combineddata$wt_4_uns<-((1-combineddata$pS)/combineddata$pS)*(1/combineddata$pA_targmodel)
  
  stb_factor2<- combineddata %>% filter(S==1) %>% summarise(mean_wts_uns = mean(wt_4_uns)) %>% as.numeric()
  
  combineddata$wt_4<-combineddata$wt_4_uns/stb_factor2

  return(combineddata)
}


# #unit testing:
# Sform<-"A+Z+C"
# Aform<-"C"
# 
# Smod<-glm(as.formula(paste0("S~",Sform)), data=dat, family = binomial())
# summary(Smod)
# 
# #unconditional odds of S
# odds_S<-(sum(dat$S)/nrow(dat))/(1-(sum(dat$S)/nrow(dat)))
# 
# #Create A model
# Amod<-glm(as.formula(paste0("A~",Aform)), data=studysamp, family = binomial())
# 
# #Create weights
# dat$pS<-predict.glm(Smod, dat, type="response") #predicted probability of S=1 | C, A, Z, A*Z
# dat$predA<-predA<-predict.glm(Amod, dat, type="response") #predicted probability of getting A
# dat$pA<-ifelse(dat$A==1, dat$predA, 1-dat$predA) #predicted probability of treatment you actually got. 
# dat$wt_DW<-((1-dat$pS)/dat$pS)*odds_S
# dat$wt_ID<-dat$wt_DW*(1/dat$pA)
