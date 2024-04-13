options(dplyr.summarise.inform = FALSE)

ApplyWeights<-function(data, stdpop){
  #Apply DW weights
  WT_1_a <-  data %>% filter(S==1) %>% group_by(A) %>% 
    summarise(Yexp=weighted.mean(Y, wt_1)) %>% 
    pivot_wider(.,names_from=A, values_from=Yexp, names_prefix="A") 
  
  WT_1<- WT_1_a %>% 
    mutate(RD=A1-A0) %>% select(RD) %>% as.numeric()
  
  WT_1_YA1<- WT_1_a %>% select(A1) %>% rename(WT_1_YA1 = A1)
  WT_1_YA0<- WT_1_a %>% select(A0) %>% rename(WT_1_YA0 = A0)
  
  #Apply ID weights (already stdized)
  WT_2_a<-data %>% filter(S==1) %>% group_by(A) %>% summarise(Yexp=weighted.mean(Y, wt_2)) %>% 
    pivot_wider(.,names_from=A, values_from=Yexp, names_prefix="A")
  
  WT_2 <- WT_2_a %>% mutate(RD=A1-A0) %>% select(RD) %>% as.numeric()
  
  WT_2_YA1<- WT_2_a %>% select(A1) %>% rename(WT_2_YA1 = A1)
  WT_2_YA0<- WT_2_a %>% select(A0) %>% rename(WT_2_YA0 = A0)
  
  #Apply DW weights and stdize
  WT_3_a<-data %>% filter(S==1) %>% group_by(A,C1, C2_cat) %>% 
    summarise(Yexp=weighted.mean(Y, wt_1)) %>%
    pivot_wider(.,names_from=c(A), values_from=Yexp, names_prefix="A") %>% 
    select(C1, C2_cat,A0, A1) %>% 
    left_join(.,stdpop, by=c("C1", "C2_cat")) %>% ungroup() %>% summarise(A1 = crossprod(A1, prop_C1C2),
                                                                          A0 = crossprod(A0, prop_C1C2)) 
  
  WT_3 <- WT_3_a %>% mutate(RD=A1-A0) %>% select(RD) %>% as.numeric()
  
  WT_3_YA1<- WT_3_a %>% select(A1) %>% rename(WT_3_YA1 = A1)
  WT_3_YA0<- WT_3_a %>% select(A0) %>% rename(WT_3_YA0 = A0)

  
  
  #Apply new WT_3 with IPTW (already stdized), called WT_4
  WT_4_a<-data %>% filter(S==1) %>% group_by(A) %>% summarise(Yexp=weighted.mean(Y, wt_4)) %>%
    pivot_wider(.,names_from=A, values_from=Yexp, names_prefix="A")

  WT_4 <- WT_4_a %>% mutate(RD=A1-A0) %>% select(RD) %>% as.numeric()

  WT_4_YA1<- WT_4_a %>% select(A1) %>% rename(WT_4_YA1 = A1)
  WT_4_YA0<- WT_4_a %>% select(A0) %>% rename(WT_4_YA0 = A0)
  
  wtdest<-data.frame(WT_1=WT_1, WT_2=WT_2, WT_3=WT_3, WT_4=WT_4, 
                     WT_1_YA1=WT_1_YA1, WT_2_YA1=WT_2_YA1, WT_3_YA1=WT_3_YA1, WT_4_YA1=WT_4_YA1,
                     WT_1_YA0=WT_1_YA0, WT_2_YA0=WT_2_YA0, WT_3_YA0=WT_3_YA0, WT_4_YA0=WT_4_YA0)
  
  
return(wtdest)
  }



#unit test

#data<-dat2

# data<-dat
# WT_1 <-  data %>% filter(S==1) %>% group_by(A) %>% 
#   summarise(Yexp=weighted.mean(Y, wt_DW)) %>% 
#   pivot_wider(.,names_from=A, values_from=Yexp, names_prefix="A") %>% 
#   mutate(RD=A1-A0) %>% select(RD) %>% as.numeric()
# 
# #Apply ID weights (already stdized)
# WT_2<-data %>% filter(S==1) %>% group_by(A) %>% summarise(Yexp=weighted.mean(Y, wt_ID)) %>% 
#   pivot_wider(.,names_from=A, values_from=Yexp, names_prefix="A") %>% 
#   mutate(RD=A1-A0) %>% select(RD) %>% as.numeric()
# 
# #Apply DW weights and stdize
# WT_3<-data %>% filter(S==1) %>% group_by(A,C) %>% 
#   summarise(Yexp=weighted.mean(Y, wt_DW)) %>%
#   pivot_wider(.,names_from=c(A), values_from=Yexp, names_prefix="A") %>% 
#   mutate(RD=A1-A0) %>% select(C,RD) %>%
#   mutate(RD_C=case_when(C==1 ~ RD*pC_S0,
#                         C==0 ~ RD*(1-pC_S0))) %>% select(RD_C) %>% colSums() %>% as.numeric()