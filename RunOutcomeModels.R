#Create outcome models

RunOutcomeModels<-function(targetdata, studydata, stdpop, Yform, Zform, YformnoZ){

#Create OMs
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


#Calculate RDs:
RD_gcomp<-targsampreps %>% group_by(rep) %>% summarise(OM_1=mean(Y_OM1),
                                                       OM_2=mean(Y_OM2),
                                                       OM_3=mean(Y_OM3),
                                                       OM_5=mean(Y_OM5)) 
RD_gcomp <- rbind(RD_gcomp, cbind(data.frame(rep="RD"), 
                              data.frame((RD_gcomp[RD_gcomp$rep=="exp_copy",
                                       c("OM_1", "OM_2", "OM_3", "OM_5") ] - 
                              RD_gcomp[RD_gcomp$rep=="unexp_copy", 
                                       c("OM_1", "OM_2", "OM_3", "OM_5")]))))

RD_gcomp_YA1<-RD_gcomp %>% filter(rep=="exp_copy") %>% select(-rep)
colnames(RD_gcomp_YA1) <- c("OM_1_YA1", "OM_2_YA1", "OM_3_YA1", "OM_5_YA1")
RD_gcomp_YA0<-RD_gcomp %>% filter(rep=="unexp_copy") %>% select(-rep)
colnames(RD_gcomp_YA0) <- c("OM_1_YA0", "OM_2_YA0", "OM_3_YA0", "OM_5_YA0")

#Alternate approach: use OM to predict Y for observed A,Z, and  standardize for C1, C2. 
targetdata$Y_OM4<-predict.lm(OM_studysamp, targetdata)
OM_4_a<-targetdata %>% group_by(A,C1, C2_cat) %>% summarise(Yexp_OM4=mean(Y_OM4)) %>%
pivot_wider(.,names_from=c(A), values_from=Yexp_OM4, names_prefix="A") %>% select(C1, C2_cat,A0, A1) %>% 
  left_join(.,stdpop, by=c("C1", "C2_cat")) %>% ungroup() %>% summarise(A1 = crossprod(A1, prop_C1C2),
                                                                        A0 = crossprod(A0, prop_C1C2))

OM_4 <- OM_4_a %>% mutate(RD=A1-A0) %>% select(RD) %>% as.numeric()

OM_4_YA1<- OM_4_a %>% select(A1) %>% rename(OM_4_YA1 = A1)
OM_4_YA0<- OM_4_a %>% select(A0) %>% rename(OM_4_YA0 = A0)


RDs<-cbind(RD_gcomp[RD_gcomp$rep=="RD", c("OM_1", "OM_2", "OM_3")], OM_4, RD_gcomp[RD_gcomp$rep=="RD", c("OM_5")],
           RD_gcomp_YA1 %>% select("OM_1_YA1", "OM_2_YA1", "OM_3_YA1"), OM_4_YA1, RD_gcomp_YA1 %>% select("OM_5_YA1"),
           RD_gcomp_YA0 %>% select("OM_1_YA0", "OM_2_YA0", "OM_3_YA0"), OM_4_YA0, RD_gcomp_YA0 %>% select("OM_5_YA0"))
return(RDs)
}


#test
# targetdata=targetsamp
# studydata=studysamp
# Yform=p$Yform
# Zform=p$Zform
# stdpop=StddistC1C1
