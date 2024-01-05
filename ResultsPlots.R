library(tidyverse)
library(RColorBrewer)
##Code for looking at results
substrRight <- function(x, nrmv){
  substr(x, nrmv, nchar(x))
}

options(scipen = 100)


#Calculate summaries of results and samples
Scenarios<-c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10") 
             #, "S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18", "S19", "S20")
for (i in Scenarios){
  #Unit testing
 # i<-"S6"
  
    results<-read_csv(paste0("/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Output/sim_runs_",i,".csv"))
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
    
    results <- results %>% na.omit 
    
    results_supp<-read_csv(paste0("/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Output/sim_runs_",i,"_supp.csv"))
    colnames(results_supp)<-colnames(results)
    results_supp <- results_supp %>% na.omit 
    
    #Supplement missing rows 
    results<-rbind(results, results_supp[1:(5000-nrow(results)),])
    
    results <- results %>% na.omit %>% mutate(STUD_CIcov = case_when(STUD_LCI <= Truth & STUD_UCI>=Truth ~ 1,
                                                          TRUE ~ 0),
                                  WT_1_CIcov = case_when(WT_1_LCI <= Truth & WT_1_UCI>=Truth ~ 1,
                                                            TRUE ~ 0),
                                  WT_2_CIcov = case_when(WT_2_LCI <= Truth & WT_2_UCI>=Truth ~ 1,
                                                            TRUE ~ 0),
                                  WT_3_CIcov = case_when(WT_3_LCI <= Truth & WT_3_UCI>=Truth ~ 1,
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
                                                         TRUE ~ 0))
    
    #HOw to calculate MSE/RMSE? Use CIs to obtain variance or use SD of estimates directly?
    SDs <- results %>% summarise(STUD_empSE = sd(STUD_PtEst),
                                 WT_1_empSE = sd(WT_1_PtEst),
                                 WT_2_empSE = sd(WT_2_PtEst),
                                 WT_3_empSE = sd(WT_3_PtEst),
                                 OM_1_empSE = sd(OM_1_PtEst),
                                 OM_2_empSE = sd(OM_2_PtEst),
                                 OM_3_empSE = sd(OM_3_PtEst),
                                 OM_4_empSE = sd(OM_4_PtEst),
                                 OM_5_empSE = sd(OM_5_PtEst))
    
    summary<-SDs %>% cbind(data.frame(t(colMeans(results))), .) %>% 
      
                           mutate(STUD_bias = (STUD_PtEst-Truth),
                                  WT_1_bias = (WT_1_PtEst-Truth),
                                  WT_2_bias = (WT_2_PtEst-Truth),
                                  WT_3_bias = (WT_3_PtEst-Truth),
                                  OM_1_bias = (OM_1_PtEst-Truth),
                                  OM_2_bias = (OM_2_PtEst-Truth),
                                  OM_3_bias = (OM_3_PtEst-Truth),
                                  OM_4_bias = (OM_4_PtEst-Truth),
                                  OM_5_bias = (OM_5_PtEst-Truth),
                                  
                                  STUD_pctbias = 100*(STUD_PtEst-Truth)/Truth, 
                                  WT_1_pctbias = 100*(WT_1_PtEst-Truth)/Truth,
                                  WT_2_pctbias = 100*(WT_2_PtEst-Truth)/Truth,
                                  WT_3_pctbias = 100*(WT_3_PtEst-Truth)/Truth,
                                  OM_1_pctbias = 100*(OM_1_PtEst-Truth)/Truth,
                                  OM_2_pctbias = 100*(OM_2_PtEst-Truth)/Truth,
                                  OM_3_pctbias = 100*(OM_3_PtEst-Truth)/Truth,
                                  OM_4_pctbias = 100*(OM_4_PtEst-Truth)/Truth, 
                                  OM_5_pctbias = 100*(OM_5_PtEst-Truth)/Truth, 
                                  
                                  STUD_SEfromCI= (STUD_UCI-STUD_LCI)/3.92,
                                  WT_1_SEfromCI= (WT_1_UCI-WT_1_LCI)/3.92,
                                  WT_2_SEfromCI= (WT_2_UCI-WT_2_LCI)/3.92,
                                  WT_3_SEfromCI= (WT_3_UCI-WT_3_LCI)/3.92,
                                  OM_1_SEfromCI= (OM_1_UCI-OM_1_LCI)/3.92,
                                  OM_2_SEfromCI= (OM_2_UCI-OM_2_LCI)/3.92,
                                  OM_3_SEfromCI= (OM_3_UCI-OM_3_LCI)/3.92,
                                  OM_4_SEfromCI= (OM_4_UCI-OM_4_LCI)/3.92,
                                  OM_5_SEfromCI= (OM_5_UCI-OM_5_LCI)/3.92,
                                  
                                 STUD_RMSE = sqrt((STUD_PtEst - Truth)^2 + STUD_empSE^2),
                                 WT_1_RMSE = sqrt((WT_1_PtEst - Truth)^2 + WT_1_empSE^2),
                                 WT_2_RMSE = sqrt((WT_2_PtEst - Truth)^2 + WT_2_empSE^2),
                                 WT_3_RMSE = sqrt((WT_3_PtEst - Truth)^2 + WT_3_empSE^2),
                                 OM_1_RMSE = sqrt((OM_1_PtEst - Truth)^2 + OM_1_empSE^2),
                                 OM_2_RMSE = sqrt((OM_2_PtEst - Truth)^2 + OM_2_empSE^2),
                                 OM_3_RMSE = sqrt((OM_3_PtEst - Truth)^2 + OM_3_empSE^2),
                                 OM_4_RMSE = sqrt((OM_4_PtEst - Truth)^2 + OM_4_empSE^2),
                                 OM_5_RMSE = sqrt((OM_5_PtEst - Truth)^2 + OM_5_empSE^2)
                           )

    summary2<-summary %>% select(grep("STUD", names(.)),
                                 grep("WT_", names(.)),
                                 grep("OM_", names(.))) %>% t() %>%  data.frame() 
    
    summary2$Model <-substr(rownames(summary2),1,4)
    summary2$Metric<-substrRight(rownames(summary2), 6) 
    
    summary3<-summary2 %>% pivot_wider(.,id_cols=c("Model"), 
                                       names_from="Metric", values_from=".") %>% 
      bind_rows(data.frame(Model="Truth", PtEst=mean(results$Truth))) %>%
      mutate(Scenario = i) %>% 
      select(Scenario, Model, PtEst, LCI, UCI, empSE, bootSE, 
             SEfromCI, bias, pctbias, CIcov, RMSE)
    
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

#RELABELING OM estimators so they make sense in the Paper! Need to be very careful about this!

Final_res$Model2<-factor(Final_res$Model, levels=c("Truth", "STUD", "WT_1", "WT_2","WT_3", "OM_1", "OM_4", "OM_5", "OM_2","OM_3"), 
                         labels=c("Truth", "SATE", "IOSW1", "IOSW2", "IOSW3", "OM1", "OM2", "OM3", "OM4", "OM5"))


Final_res$Scenario<-factor(Final_res$Scenario, levels=c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"))


colors<-brewer.pal(11,"RdYlBu")[c(2:4,8:11)]

Final_res_nomed<-Final_res %>% filter(Scenario %in% c("S1", "S2", "S3", "S4", "S5", "S6"), Model!="Truth", Model!="OM_2", Model!="OM_3", Model!="OM_5") 
Truth_nomed<-Final_res$PtEst[Final_res$Scenario=="S1" & Final_res$Model=="Truth"]

plot_nomed<- ggplot(data=Final_res_nomed)+
  geom_pointrange(aes(x=Scenario, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
                      color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
  geom_hline(yintercept=Truth_nomed)+ylab("Estimated effect of A on Y (95% CI)")+
  scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy")) + ylim(0,0.4)+
  theme_bw()

ggsave(plot=plot_nomed, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_nomed.jpg", width = 9.5, height=5.75, units="in")


Final_res_med<-Final_res %>% filter(Scenario %in% c("S7", "S8", "S9"), Model!="Truth") 
Truth_med<-Final_res$PtEst[Final_res$Scenario=="S7" & Final_res$Model=="Truth"]

plot_med<- ggplot(data=Final_res_med)+
  geom_pointrange(aes(x=Scenario, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
                      color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
  geom_hline(yintercept=Truth_med)+
  scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy")) + ylim(0,0.6)+
  ylab("Estimated total effect of A on Y (95% CI)")+theme_bw()

ggsave(plot=plot_med, filename="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/results_med.jpg", width = 8, height=5.75, units="in")



Perf_results<-Final_res %>% select(Scenario, Model, bias, pctbias, CIcov, empSE) %>% filter(Model!="Truth")

Perf_results2<-Perf_results %>% mutate(Model2 = case_when(Model == "STUD" ~ "STUD",
                                                          Model == "WT_1" ~ "IOSW1",
                                                          Model == "WT_2" ~ "IOSW2",
                                                          Model == "WT_3" ~ "IOSW3",
                                                          Model == "OM_1" ~ "OM1",
                                                          Model == "OM_2" ~ "OM4",
                                                          Model == "OM_3" ~ "OM5",
                                                          Model == "OM_4" ~ "OM2",
                                                          Model == "OM_5" ~ "OM3")) %>%
  select(Scenario, Model2, bias, pctbias, CIcov, empSE)

Perf_results2$Model2<-factor(Perf_results2$Model2, levels=c("Truth", "STUD", "IOSW1", "IOSW2", "IOSW3", "OM1", "OM2", "OM3", "OM4", "OM5"))
                                  
Perf_results2<-Perf_results2 %>% arrange(.,Scenario, Model2)
                        
                #  (Final_res$Model, levels=c("Truth", "STUD", "WT_1", "WT_2","WT_3", "OM_1", "OM_4", "OM_5", "OM_2","OM_3"), 
                 #        labels=c("Truth", "Selected study", "IOSW1", "IOSW2", "IOSW3", "OM1", "OM2", "OM3", "OM4", "OM5"))

write.csv(Perf_results2, file="/Users/eleanorhayes-larson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Manuscript/Figures/Performance_stats.csv")




# #Specific performance stat tables:
# Ptest_tab<-Final_res %>% select(Scenario, Model, PtEst) %>% 
#   pivot_wider(,names_from=Model, values_from=PtEst) %>% select(-Truth)
# 
# CIcov_tab<-Final_res %>% select(Scenario, Model, CIcov) %>% 
#     pivot_wider(,names_from=Model, values_from=CIcov) %>% select(-Truth)
# 
# empSE_tab<-Final_res %>% select(Scenario, Model, empSE) %>% 
#   pivot_wider(,names_from=Model, values_from=empSE) %>% select(-Truth)
# 
# bias_tab<-Final_res %>% select(Scenario, Model, bias) %>% 
#   pivot_wider(,names_from=Model, values_from=bias) %>% select(-Truth)
# 
# pctbias_tab<-Final_res %>% select(Scenario, Model, pctbias) %>% 
#   pivot_wider(,names_from=Model, values_from=pctbias) %>% select(-Truth)


# 
# plot_S1<- ggplot(data=Final_res_nomed %>% filter(Scenario=="S1"))+
#   geom_pointrange(aes(x=Model2, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
#                       color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
#   geom_hline(yintercept=Truth_nomed)+ylab("Est. effect of A on Y (95% CI)")+
#   scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy")) + ylim(0,0.4)+xlab("")+
#   theme_bw()+theme(legend.position = "none",
#                    text = element_text(size = 20))
# 
# 
# ggsave(plot_S1, filename = "C:/Users/ehlarson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Presentations/Figures/S1_rev.jpg", 
#        width=12.6, height=4.37, units="in")
# 
# 
# 
# plot_S4<- ggplot(data=Final_res_nomed %>% filter(Scenario=="S4"))+
#   geom_pointrange(aes(x=Model2, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
#                       color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
#   geom_hline(yintercept=Truth_nomed)+ylab("Est. effect of A on Y (95% CI)")+
#   scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy")) + ylim(0,0.4)+xlab("")+
#   theme_bw()+theme(legend.position = "none",
#                    text = element_text(size = 20))
# 
# 
# ggsave(plot_S4, filename = "C:/Users/ehlarson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Presentations/Figures/S4_rev.jpg", 
#        width=12.6, height=4.37, units="in")
# 
# 
# 
# 
# plot_S9<- ggplot(data=Final_res_med %>% filter(Scenario=="S9"))+
#   geom_pointrange(aes(x=Model2, y=PtEst, ymin=LCI, ymax=UCI, group=Model2, 
#                       color=Model2), position=position_dodge(width=0.75), size=1, shape=15)+
#   geom_hline(yintercept=Truth_med)+ylab("Est. effect of A on Y (95% CI)")+
#   scale_color_manual(name="Estimator", values=c("darkgray", colors, "navy")) + ylim(0,0.6)+xlab("")+
#   theme_bw()+theme(legend.position = "none",
#                    text = element_text(size = 20))
# 
# 
# ggsave(plot_S9, filename = "C:/Users/ehlarson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Presentations/Figures/S9_rev.jpg", 
#        width=12.6, height=4.37, units="in")
# 
# 
# 
# 
# 
# #PLots for melodem presentations
# 
# Final_res$Model2<-factor(Final_res$Model, levels=c("Truth", "STUD", "WT_1", "WT_2","WT_3", "OM_1","OM_2","OM_3","OM_4", "OM_5"), 
#                          labels=c("Truth", "Selected study", "IOSW1", "IOSW2", "IOSW3", "OM1", "OM2", "OM3", "OM4", "OM5"))
# 
# Final_res$Scenario<-factor(Final_res$Scenario, levels=c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"))
# 
# colors<-brewer.pal(11,"RdYlBu")[c(2:4,8:11)]
# 
# 
# 

# 
# 
# Scenarios<-c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")
# for (i in Scenarios){
#   S<-Final_res %>% filter(Scenario==i, Model!="Truth")
#   Truth<-Final_res %>% filter(Scenario==i, Model=="Truth") %>% select(PtEst) %>% as.numeric()
# 
# if (i %in% c("S1", "S2", "S3", "S4", "S5", "S6")) {
#     plot<-ggplot(data=S)+
#       geom_pointrange(aes(x=Model2, y=PtEst, ymin=LCI, ymax=UCI, color=Model2), size=1, shape=15)+
#       geom_hline(yintercept=Truth)+
#       scale_color_manual(values=c("darkgray", colors, "navy")) + ylim(0,0.4)+
#       theme_bw() + xlab(NULL) +theme(legend.position = "none",
#                                      text = element_text(size = 20))
# }  else {plot<-ggplot(data=S)+
#     geom_pointrange(aes(x=Model2, y=PtEst, ymin=LCI, ymax=UCI, color=Model2), size=1, shape=15)+
#     geom_hline(yintercept=Truth)+
#     scale_color_manual(values=c("darkgray", colors, "navy")) + ylim(0,0.6)+
#     theme_bw() + xlab(NULL) +theme(legend.position = "none",
#                                    text = element_text(size = 20))
#   }
# ggsave(plot, filename = paste0("C:/Users/ehlarson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Presentations/Figures/",i,".jpg"), 
#        width=12.6, height=4.37, units="in")
# }
# 
# i<-"S1"
# res_tab<-list()
# for (i in Scenarios){
#   S<-Final_res %>% filter(Scenario==i, Model!="Truth")
#   S2<-data.frame(t(S %>% select(Model2, bias, pctbias, CIcov, empSE)))
# res_tab[[i]]<-S2  
# }
# 
# 
# test<-S %>% select(Model2, bias, pctbias, CIcov, empSE) %>% t() %>% data.frame() %>% mutate(metric=rownames(.))
# 
# colnames(test)<-test %>% filter(metric=="Model2")
# 
# test %>% filter(Model2!="Model2") %>% rename(Metric=Model2) %>% mutate(Scenario=i)
# 
# S1tab<-data.frame(res_tab$S1)
# S4tab<-data.frame(res_tab$S4)
# S9tab<-data.frame(res_tab$S9)
# 
# test<-data.frame(do.call(rbind, res_tab))

# # # #Check creation
# path_to_data<-"C:/Users/ehlarson/MHL Dropbox/Eleanor Hayes-Larson/UCLA/Eleanor_ERM/Transport sims/Code/Data"
# paramcheck<-data.frame(matrix(ncol=43))
# for (i in 1:10){
#   load(paste0(path_to_data,"/params_beta0_S",i,".Rdata"))
#   p<-p %>% select(selectcols)
#   paramcheck[i,]<-p
#   colnames(paramcheck)<-colnames(p)
# }
# selectcols<-colnames(p)
