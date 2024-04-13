library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(grid)
library(gridExtra)


##Code for looking at results
substrRight <- function(x, nrmv){
  substr(x, nrmv, nchar(x))
}

options(scipen = 100)


#Calculate summaries of results and samples
Scenarios<-c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10") 

for (i in Scenarios){
  #Unit testing
    #i<-"S1"
  
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
    
    #HOw to calculate MSE/RMSE? Use CIs to obtain variance or use SD of estimates directly?
    SDs <- results %>% summarise(STUD_empSE = sd(STUD_PtEst),
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
    
    summary<-SDs %>% cbind(data.frame(t(colMeans(results))), .) %>% 
      
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
      mutate(Scenario = i) 
    
    sumstats<-results %>% select(starts_with("C"),
                                 starts_with("A"),
                                 starts_with("Z"),
                                 starts_with("Y"),
                                 starts_with("n"),
                                 starts_with("time")) %>% colMeans() %>% t() %>% 
      data.frame() %>% mutate(Scenario=i)
    
    sumstats$nruns<-nrow(results)
    
    
    if(i=="S1") {Final_res<-summary3
    Final_sumstats<-sumstats} else {
      
      Final_res<-rbind(Final_res, summary3)
      Final_sumstats<-rbind(Final_sumstats,sumstats)}
    
}



#Plot results for paper:
#Drop old version of IOSW3, OM2
Final_res<-Final_res %>% filter(Model!="WT_3", Model!="OM_4", Model!="DR_1", Scenario != "S10")
Final_res$Scenario<-factor(Final_res$Scenario, levels=c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9"))
Final_res$Scenario2<-ifelse(Final_res$Scenario=="S1", "Supp 1", 
                            ifelse(Final_res$Scenario=="S2", "Supp 2",
                                   ifelse(Final_res$Scenario=="S3", "Supp 3",
                                          ifelse(Final_res$Scenario=="S4", "Scenario 1",
                                                 ifelse(Final_res$Scenario=="S5", "Supp 4",
                                                        ifelse(Final_res$Scenario=="S6", "Supp 5",
                                                               ifelse(Final_res$Scenario=="S7", "Supp 6",
                                                                      ifelse(Final_res$Scenario=="S8", "Supp 7",
                                                                             ifelse(Final_res$Scenario=="S9", "Scenario 2",NA)))))))))



#RELABELING OM estimators so they make sense in the Paper! 
#Need to be very careful about this! WT_4 is now IOSW3, and OM_6 is now OM2.
#Adding DR estimatators
Final_res$Model2<-factor(Final_res$Model, levels=c("STUD", "WT_4",          "OM_6","OM_3", "DR_2", "DR_3", "WT_1", "WT_2",   "OM_5","OM_1", "OM_2"), 
                                          labels=c("Study sample", "IOSW", "OM1", "OM2", "DR1", "DR2", "IOSW2", "IOSW3", "OM3",   "OM4",  "OM5"))



Final_res_long <- Final_res %>% pivot_longer(., cols=c("PtEst", "LCI", "UCI", 
                                                       "YA1_PtEst", "YA1_LCI", "YA1_UCI", 
                                                       "YA0_PtEst", "YA0_LCI", "YA0_UCI",
                                                        "Truth", "YA1_Truth", "YA0_Truth")) %>%
                                               
                                                  mutate(Estimand=case_when(str_detect(name, "YA1") ~ "Y^1",
                                                                            str_detect(name, "YA0") ~ "Y^0",
                                                                            TRUE ~ "TATE"),
                                                         name2 = str_remove_all(name, "YA1_"),
                                                         name3 = str_remove_all(name2, "YA0_"),
                                                         MainEst = case_when(Model2 %in% c("Study sample", "IOSW", "OM1", "OM2", "DR1", "DR2") ~ "Main text",
                                                                             TRUE ~ "Supplemental")
                                                  ) %>% select(-name, -name2) %>%
                                                  pivot_wider(., names_from = name3, values_from=value)
#Other things needed for plots


IOSWcolors<-brewer.pal(8,"Greens")[c(3,5,7)]
OMcolors<-c(brewer.pal(11,"RdYlBu")[c(8:11)], "navy")
DRcolors<-c("orchid", "darkorchid", "purple4")


#Two scenarios for main paper figure
Final_res_main <- Final_res_long %>% filter(Scenario2 %in% c("Scenario 1", "Scenario 2"), 
                                            MainEst== "Main text") 
hline_dat = Final_res_main %>% select(Scenario2, Estimand, Truth)

plot_main<- ggplot(data=Final_res_main)+
  geom_pointrange(aes(x="", y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
                      color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
  geom_hline(data=hline_dat, aes(yintercept=Truth))+ylab("Estimate")+xlab("")+
  scale_color_manual(name="Estimator", values=c("darkgray", IOSWcolors[3], OMcolors[c(2,5)], DRcolors[c(1,2)],
                                                            IOSWcolors[c(1,2)], OMcolors[c(1,3:4)])) + #guides(colour = FALSE) +
  facet_grid(rows=vars(Estimand), cols=vars(Scenario2), scales="free", 
             labeller=labeller(Estimand=label_parsed, Scenario2=label_value)) + #ylim(0,0.6)+
  theme_bw()
plot_main

##---Supplemental figures

#Main scenarios, all estimators
  Final_res_supp <- Final_res_long %>% filter(Scenario2 %in% c("Scenario 1", "Scenario 2")) 
  hline_dat = Final_res_supp %>% select(Scenario2, Estimand, Truth)
  
  plot_supp1<- ggplot(data=Final_res_supp)+
    geom_pointrange(aes(x=MainEst, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
                        color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
    geom_hline(data=hline_dat, aes(yintercept=Truth))+ylab("Estimate")+xlab("")+
    scale_color_manual(name="Estimator", values=c("darkgray", IOSWcolors[3], OMcolors[c(2,5)], DRcolors[c(1,2)],
                                                  IOSWcolors[c(1,2)], OMcolors[c(1,3:4)])) + #guides(colour = FALSE) +
    facet_grid(rows=vars(Estimand), cols=vars(Scenario2), scales="free", 
               labeller=labeller(Estimand=label_parsed, Scenario2=label_value)) + #ylim(0,0.6)+
    theme_bw()
  plot_supp1

#Supp scenarios no mediation, all estimators
  Final_res_supp_nomed <- Final_res_long %>% filter(Scenario2 %in% c("Supp 1", "Supp 2", "Supp 3", "Supp 4", "Supp 5")) 
  hline_dat = Final_res_supp_nomed %>% select(Scenario2, Estimand, Truth)
  
  plot_supp2<- ggplot(data=Final_res_supp_nomed)+
    geom_pointrange(aes(x=MainEst, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
                        color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
    geom_hline(data=hline_dat, aes(yintercept=Truth))+ylab("Estimate")+xlab("")+
    scale_color_manual(name="Estimator", values=c("darkgray", IOSWcolors[3], OMcolors[c(2,5)], DRcolors[c(1,2)],
                                                  IOSWcolors[c(1,2)], OMcolors[c(1,3:4)])) + #guides(colour = FALSE) +
    facet_grid(rows=vars(Estimand), cols=vars(Scenario2), scales="free", 
               labeller=labeller(Estimand=label_parsed, Scenario2=label_value)) + #ylim(0,0.6)+
    theme_bw()
  plot_supp2

#Supp scenarios no mediation, all estimators
  Final_res_supp_med <- Final_res_long %>% filter(Scenario2 %in% c("Supp 6", "Supp 7")) 
  hline_dat = Final_res_supp_med %>% select(Scenario2, Estimand, Truth)
  
  plot_supp3<- ggplot(data=Final_res_supp_med)+
    geom_pointrange(aes(x=MainEst, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
                        color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
    geom_hline(data=hline_dat, aes(yintercept=Truth))+ylab("Estimate")+xlab("")+
    scale_color_manual(name="Estimator", values=c("darkgray", IOSWcolors[3], OMcolors[c(2,5)], DRcolors[c(1,2)],
                                                  IOSWcolors[c(1,2)], OMcolors[c(1,3:4)])) + #guides(colour = FALSE) +
    facet_grid(rows=vars(Estimand), cols=vars(Scenario2), scales="free", 
               labeller=labeller(Estimand=label_parsed, Scenario2=label_value)) + #ylim(0,0.6)+
    theme_bw()
  plot_supp3

#Table of performance results, all scenarios and all estimators
Perf_results<-Final_res %>% select(Scenario2, Model2, bias, pctbias, CIcov, empSE) %>% 
      mutate(bias=sprintf("%1.2f", bias),
             pctbias=sprintf("%1.1f%%", pctbias),
             CIcov = sprintf("%1.1f%%", 100*CIcov),
             empSE = sprintf("%1.3f", empSE)) %>%
      arrange(Scenario2, Model2) 
      


#Output results
ggsave(plot=plot_main, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_mainR1.jpg", width = 6, height=5.75, units="in")
ggsave(plot=plot_supp1, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_supp1R1.jpg", width = 8, height=5.75, units="in")
ggsave(plot=plot_supp2, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_supp2R1.jpg", width = 15, height=5.75, units="in")
ggsave(plot=plot_supp3, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_supp3R1.jpg", width = 8, height=5.75, units="in")
write.csv(Perf_results, file="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/Performance_stats_TATE.csv")

# 
# 
# 
# 
# #Old code below!
# 
# 
# #Relabeling for R1 where we take out S5-S8.
# Final_res$med<-ifelse(Final_res$Scenario %in% c("S1", "S2", "S3", "S4", "S5", "S6"), "No mediation", "With mediation")                                                                       
#                                                                                            
# 
# #colors<-brewer.pal(11,"RdYlBu")[c(4,3,2,8:11)]
# 
# #IOSWcolors<-brewer.pal(8,"OrRd")[c(3,5,7)]
# IOSWcolors<-brewer.pal(8,"Greens")[c(3,5,7)]
# OMcolors<-c(brewer.pal(11,"RdYlBu")[c(8:11)], "navy")
# DRcolors<-c("orchid", "darkorchid", "purple4")
# 
# Final_res_main1<-Final_res %>% 
#   filter((Scenario2 %in% c("1", "2", "3", "4") & Model!="Truth" ))#& Model!="OM_2" & Model!="OM_3" & Model!="OM_5")) 
# 
# Final_res_main2<-Final_res %>% 
#   filter((Scenario2 %in% c("5") & Model!="Truth")) 
# 
# #Check colors since we don't want OM3-5 in this one.
# plot_main1<- ggplot(data=Final_res_main1)+
#   geom_pointrange(aes(x=Scenario2, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
#                       color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
#   geom_hline(yintercept=Final_res_main1$Truth)+ylab("Estimated effect of A on Y (95% CI)")+xlab("")+
# #  scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy", "pink", "orchid", "darkorchid")) + guides(colour = FALSE) +
#   scale_color_manual(name="Estimator", values=c("darkgray", IOSWcolors, OMcolors, DRcolors)) + guides(colour = FALSE) +
#   facet_grid(cols=vars(med), scales="free") + ylim(0,0.6)+
#   theme_bw()
# plot_main1
# 
# plot_main2<- ggplot(data=Final_res_main2)+
#   geom_pointrange(aes(x=Scenario2, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
#                       color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
#   geom_hline(yintercept=Final_res_main2$Truth)+ylab("")+xlab("")+
#  # scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy", "orchid", "darkorchid")) + 
#   scale_color_manual(name="Estimator", values=c("darkgray", IOSWcolors, OMcolors, DRcolors)) + #guides(colour = FALSE) +
#   facet_grid(cols=vars(med), scales="free") + ylim(0,0.6)+
#   theme_bw()
# plot_main2
# 
# main_plot_final<-plot_grid(plot_main1,plot_main2, ncol=2, rel_widths = c(1.3,1))
# 
# ggsave(plot=main_plot_final, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_mainR1.jpg", width = 9.5, height=5.75, units="in")
# 
# 
# #Supplemental
# Final_res_supp1<-Final_res %>% 
#   filter((Scenario2 %in% c("Supp 1", "Supp 2") & Model!="Truth" & Model!="OM_2" & Model!="OM_3" & Model!="OM_5")) 
# 
# Final_res_supp2<-Final_res %>% 
#   filter((Scenario2 %in% c("Supp 3", "Supp 4") & Model!="Truth")) 
# 
# #Check colors since we don't want OM3-5 in this one.
# plot_supp1<- ggplot(data=Final_res_supp1)+
#   geom_pointrange(aes(x=Scenario2, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
#                       color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
#   geom_hline(yintercept=Final_res_supp1$Truth)+ylab("Estimated effect of A on Y (95% CI)")+xlab("")+
#   scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy")) + guides(colour = FALSE) +
#   facet_grid(cols=vars(med), scales="free") + ylim(0,0.6)+
#   theme_bw()
# plot_supp1
# 
# plot_supp2<- ggplot(data=Final_res_supp2)+
#   geom_pointrange(aes(x=Scenario2, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
#                       color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
#   geom_hline(yintercept=Final_res_supp2$Truth)+ylab("")+xlab("")+
#   scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy")) + 
#   facet_grid(cols=vars(med), scales="free") + ylim(0,0.6)+
#   theme_bw()
# plot_supp2
# 
# supp_plot_final<-plot_grid(plot_supp1,plot_supp2, ncol=2, rel_widths = c(0.55,1))
# 
# ggsave(plot=supp_plot_final, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_suppR1.jpg", width = 9.5, height=5.75, units="in")
# 
# 
# 
# Perf_results<-Final_res %>% select(Scenario, Model, bias, pctbias, CIcov, empSE) %>% filter(Model!="Truth")
# 
# Perf_results2<-Perf_results %>% mutate(Model2 = case_when(Model == "STUD" ~ "STUD",
#                                                           Model == "WT_1" ~ "IOSW1",
#                                                           Model == "WT_2" ~ "IOSW2",
#                                                           Model == "WT_3" ~ "IOSW3",
#                                                           Model == "OM_1" ~ "OM1",
#                                                           Model == "OM_2" ~ "OM4",
#                                                           Model == "OM_3" ~ "OM5",
#                                                           Model == "OM_4" ~ "OM2",
#                                                           Model == "OM_5" ~ "OM3")) %>%
#   select(Scenario, Model2, bias, pctbias, CIcov, empSE)
# 
# Perf_results2$Model2<-factor(Perf_results2$Model2, levels=c("Truth", "STUD", "IOSW1", "IOSW2", "IOSW3", "OM1", "OM2", "OM3", "OM4", "OM5"))
#                                   
# Perf_results2<-Perf_results2 %>% arrange(.,Scenario, Model2)
#                         
#                 #  (Final_res$Model, levels=c("Truth", "STUD", "WT_1", "WT_2","WT_3", "OM_1", "OM_4", "OM_5", "OM_2","OM_3"), 
#                  #        labels=c("Truth", "Selected study", "IOSW1", "IOSW2", "IOSW3", "OM1", "OM2", "OM3", "OM4", "OM5"))
# 
# write.csv(Perf_results2, file="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/Performance_stats.csv")
