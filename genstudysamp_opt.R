genstudysamp_opt<-function(BETA_S_0, data, ntarget, nstudysamp, beta_S_A, beta_S_C1, beta_S_C2, beta_S_Z, beta_S_AC1,beta_S_AC2, beta_S_AZ, beta_S_C1Z, beta_S_C2Z){
  
  #Need to calculate beta_S_0 as a balancing intercept to get p(S)~1500/6M, conditional p's so annoying for complex model
  #also, do we need to re-calculate this each time?
  
  targetcopy<-data
  
  targetcopy$S<-rbinom(ntarget, 1, exp(BETA_S_0+beta_S_A*targetcopy$A+beta_S_C1*targetcopy$C1+beta_S_C2*targetcopy$C2+
                                         beta_S_Z*targetcopy$Z+ beta_S_AC1*targetcopy$A*targetcopy$C1+ beta_S_AC2*targetcopy$A*targetcopy$C2+
                                         beta_S_AZ*targetcopy$A*targetcopy$Z+
                                         beta_S_C1Z*targetcopy$C1*targetcopy$Z + beta_S_C2Z*targetcopy$C2*targetcopy$Z)/(1+exp(BETA_S_0+beta_S_A*targetcopy$A+beta_S_C1*targetcopy$C1+beta_S_C2*targetcopy$C2+
                                                                                                                                  beta_S_Z*targetcopy$Z+ beta_S_AC1*targetcopy$A*targetcopy$C1+ beta_S_AC2*targetcopy$A*targetcopy$C2+
                                                                                                                                  beta_S_AZ*targetcopy$A*targetcopy$Z+
                                                                                                                                  beta_S_C1Z*targetcopy$C1*targetcopy$Z + beta_S_C2Z*targetcopy$C2*targetcopy$Z)))
  samp<-targetcopy %>% filter(S==1)
  optSreturn<-abs(nrow(samp) - nstudysamp)
  return(optSreturn)
}