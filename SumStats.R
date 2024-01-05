SumStats<-function(data){
  n<-nrow(data)
  C1prev_A0<-mean(data$C1[data$A==0])
  C1prev_A1<-mean(data$C1[data$A==1])
  C2mean_A0<-mean(data$C2[data$A==0])
  C2mean_A1<-mean(data$C2[data$A==1])
  Aprev<-mean(data$A)
  Zmean<-mean(data$Z)
  Zsd<-sd(data$Z)
  Ymean<-mean(data$Y)
  Ysd<-sd(data$Y)
  
  stats<-cbind(n, C1prev_A0, C1prev_A1, C2mean_A0, C2mean_A1, Aprev, Zmean, Zsd, Ymean, Ysd)
  return(stats)
}
