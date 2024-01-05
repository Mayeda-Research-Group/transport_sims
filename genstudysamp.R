genstudysamp<-function(data, ntarget, beta_S_0, beta_S_A, beta_S_C1, beta_S_C2, beta_S_Z, beta_S_AC1, beta_S_AC2, beta_S_AZ, beta_S_C1Z, beta_S_C2Z){
    
    targetpop<-data
    targetpop$S<-rbinom(ntarget, 1, exp(beta_S_0+beta_S_A*targetpop$A+beta_S_C1*targetpop$C1+beta_S_C2*targetpop$C2+
                                        beta_S_Z*targetpop$Z+ beta_S_AC1*targetpop$A*targetpop$C1+ beta_S_AC2*targetpop$A*targetpop$C2+
                                        beta_S_AZ*targetpop$A*targetpop$Z+
                                        beta_S_C1Z*targetpop$C1*targetpop$Z+beta_S_C2Z*targetpop$C2*targetpop$Z)/(1+exp(beta_S_0+beta_S_A*targetpop$A+beta_S_C1*targetpop$C1+beta_S_C2*targetpop$C2+
                                                                                                                          beta_S_Z*targetpop$Z+ beta_S_AC1*targetpop$A*targetpop$C1+ beta_S_AC2*targetpop$A*targetpop$C2+
                                                                                                                          beta_S_AZ*targetpop$A*targetpop$Z+
                                                                                                                          beta_S_C1Z*targetpop$C1*targetpop$Z+beta_S_C2Z*targetpop$C2*targetpop$Z)))
  samp<-targetpop %>% filter(S==1)
  
  
  return(samp)
}



#unit testing


