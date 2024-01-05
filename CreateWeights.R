CreateWeights<-function(combineddata, studydata, Sform, Aform){
  
  #Create S model
  Smod<-glm(as.formula(paste0("S~",Sform)), data=combineddata, family = binomial())
  
  #unconditional odds of S
  odds_S<-(sum(combineddata$S)/nrow(combineddata))/(1-(sum(combineddata$S)/nrow(combineddata)))
  
  #Create A model
  Amod<-glm(as.formula(paste0("A~",Aform)), data=studydata, family = binomial())
  
  #Create weights
  combineddata$pS<-predict.glm(Smod, combineddata, type="response") #predicted probability of S=1 | C, A, Z, A*Z
  combineddata$predA<-predict.glm(Amod, combineddata, type="response") #predicted probability of getting A
  combineddata$pA<-ifelse(combineddata$A==1, combineddata$predA, 1-combineddata$predA) #predicted probability of treatment you actually got. 
  combineddata$wt_1<-((1-combineddata$pS)/combineddata$pS)*odds_S
  combineddata$wt_2<-combineddata$wt_1*(1/combineddata$pA)
  
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
