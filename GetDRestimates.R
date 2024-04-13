#DR estimators
GetDRestimates<-function(targetdata, studydata, combineddata, Aform, Zform, Yform, YformnoZ){
#test
# targetdata=targetsamp
# studydata=studysamp
# combineddata=dat2
# Aform=p$Aform
# Yform=p$Yform
# YformnoZ=p$YformnoZ
# Zform=p$Zform

#Run OM models
OM_studysamp<-lm(as.formula(paste0("Y~",Yform)), data=studydata)
MM_studysamp<-lm(as.formula(paste0("Z~",Zform)), data=studydata)  
MM_targetsamp<-lm(as.formula(paste0("Z~",Zform)), data=targetdata)

OMnoZ_studysamp<-lm(as.formula(paste0("Y~",YformnoZ)), data=studydata)


#Create target samp replicates:

targsamp_A1<- targetdata %>% mutate(A=1, rep="exp_copy")
targsamp_A0<-targetdata %>% mutate(A=0, rep="unexp_copy")
targsampreps<-rbind(targsamp_A1, targsamp_A0)  

#Predict Y using OM with and without Z
targsampreps$Y_OM1<-predict.lm(OM_studysamp,targsampreps)
targsampreps$Y_OM5<-predict.lm(OMnoZ_studysamp,targsampreps)

#Overwrite Z using MM in study samp, then predict Y using predicted Z
targsampreps$Z<-predict.lm(MM_studysamp,targsampreps)
targsampreps$Y_OM2<-predict.lm(OM_studysamp,targsampreps)

targsampreps$Z<-predict.lm(MM_targetsamp,targsampreps)
targsampreps$Y_OM3<-predict.lm(OM_studysamp,targsampreps)


#DR estimator using OM1
combineddata$OM_1<-predict.lm(OM_studysamp, combineddata)
combineddata$Yresid<-combineddata$Y-combineddata$OM_1

S1A0<-combineddata %>% filter(S==1, A==0)
DR1_A0_IO<-weighted.mean(S1A0$Yresid, S1A0$wt_4)
DR1_A0_OM<-mean(targsampreps$Y_OM1[targsampreps$A==0])

DR1_A0<-DR1_A0_IO+DR1_A0_OM
DR1_A0


S1A1<-combineddata %>% filter(S==1, A==1)
DR1_A1_IO<-weighted.mean(S1A1$Yresid, S1A1$wt_4)
DR1_A1_OM<-mean(targsampreps$Y_OM1[targsampreps$A==1])

DR1_A1<-DR1_A1_IO+DR1_A1_OM
DR1_A1

DR_1<-DR1_A1-DR1_A0 #This works for S4, not for S9, as expected!



#Now trying with OM_4
combineddata$OM_4<-predict.lm(OM_studysamp, combineddata)
combineddata$Yresid<-combineddata$Y-combineddata$OM_4

S1A0<-combineddata %>% filter(S==1, A==0)
DR2_A0_IO<-weighted.mean(S1A0$Yresid, S1A0$wt_4)

S0A0<-combineddata %>% filter(S==0, A==0)
DR2_A0_OM<-weighted.mean(S0A0$OM_4, 1/S0A0$pA_targmodel)

DR2_A0<-DR2_A0_IO+DR2_A0_OM
DR2_A0


S1A1<-combineddata %>% filter(S==1, A==1)
DR2_A1_IO<-weighted.mean(S1A1$Yresid, S1A1$wt_4)

S0A1<-combineddata %>% filter(S==0, A==1)
DR2_A1_OM<-weighted.mean(S0A1$OM_4, 1/S0A1$pA_targmodel)

DR2_A1<-DR2_A1_IO+DR2_A1_OM
DR2_A1

DR_2<-DR2_A1-DR2_A0 #This works for S4 and for S9, as expected.


#Now trying with OM_3 (OM2 in paper)
combineddata$OM_3<-predict.lm(OM_studysamp, combineddata)
combineddata$Yresid<-combineddata$Y-combineddata$OM_3

S1A0<-combineddata %>% filter(S==1, A==0)
DR3_A0_IO<-weighted.mean(S1A0$Yresid, S1A0$wt_4)

S0A0<-combineddata %>% filter(S==0, A==0)
DR3_A0_OM<-mean(targsampreps$Y_OM3[targsampreps$A==0])

DR3_A0<-DR3_A0_IO+DR3_A0_OM
DR3_A0


S1A1<-combineddata %>% filter(S==1, A==1)
DR3_A1_IO<-weighted.mean(S1A1$Yresid, S1A1$wt_4)

S0A1<-combineddata %>% filter(S==0, A==1)
DR3_A1_OM<-mean(targsampreps$Y_OM3[targsampreps$A==1])


DR3_A1<-DR3_A1_IO+DR3_A1_OM
DR3_A1

DR_3<-DR3_A1-DR3_A0 #This works for S4 and for S9, as expected!

DRres<-data.frame(DR_1=DR_1, DR_2=DR_2, DR_3=DR_3, 
                  DR_1_YA1=DR1_A1, DR_2_YA1=DR2_A1, DR_3_YA1=DR3_A1,
                  DR_1_YA0=DR1_A0, DR_2_YA0=DR2_A0, DR_3_YA0=DR3_A0)
return(DRres)
}



# stdpop=StddistC1C2
#combineddata=dat