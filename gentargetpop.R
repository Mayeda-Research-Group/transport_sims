
gen_target_optA<-function(BETA_A_0, ntarget, beta_A_C1, beta_A_C2, Aprop, C1prev, C2form){

  set.seed(12345)
  targetpop<-data.frame( ID=seq(1:ntarget),
                         C1=rbinom(ntarget,1,C1prev),
                         C2=eval(parse(text=C2form))
                        )
  targetpop$A=rbinom(ntarget, 1, exp(BETA_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)/(1+exp(BETA_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)))
  
  optA_return<-abs(mean(targetpop$A) - Aprop)
  return(optA_return)
}


gen_target_optZ<-function(BETA_Z_0, ntarget, beta_A_0, beta_A_C1, beta_A_C2, beta_Z_A, Zmean, C1prev, C2form){
  
  set.seed(12345)
  targetpop<-data.frame( ID=seq(1:ntarget),
                         C1=rbinom(ntarget,1,C1prev),
                         C2=eval(parse(text=C2form))
                        )
  targetpop$A=rbinom(ntarget, 1, exp(beta_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)/(1+exp(beta_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)))
  targetpop$Z=rnorm(ntarget,BETA_Z_0+beta_Z_A*targetpop$A,10)
  
  optZ_return<-abs(mean(targetpop$Z) - Zmean)
  return(optZ_return)
}



gen_target_optY<-function(BETA_Y_0, ntarget, beta_A_0, beta_A_C1, beta_A_C2, 
                          beta_Z_0, beta_Z_A, 
                          beta_Y_A, beta_Y_Z, beta_Y_C1, beta_Y_C2, 
                            beta_Y_AC1, beta_Y_AC2, beta_Y_AZ, beta_Y_C1Z, beta_Y_C2Z, 
                          Ymean, C1prev, C2form){
  
  set.seed(12345)
  targetpop<-data.frame( ID=seq(1:ntarget),
                         C1=rbinom(ntarget,1,C1prev),
                         C2=eval(parse(text=C2form))
                        )
  targetpop$A=rbinom(ntarget, 1, exp(beta_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)/(1+exp(beta_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)))
  targetpop$Z=rnorm(ntarget,beta_Z_0+beta_Z_A*targetpop$A,10)
  targetpop$Y=rnorm(ntarget,BETA_Y_0+beta_Y_C1*targetpop$C1++beta_Y_C2*targetpop$C2+beta_Y_A*targetpop$A+beta_Y_Z*targetpop$Z+
                      beta_Y_AC1*targetpop$A*targetpop$C1+beta_Y_AC2*targetpop$A*targetpop$C2+
                      beta_Y_AZ*targetpop$A*targetpop$Z+beta_Y_C1Z*targetpop$C1*targetpop$Z+beta_Y_C2Z*targetpop$C2*targetpop$Z,1)
 
  opty_return<-abs(mean(targetpop$Y) - Ymean)
   return(opty_return)
}








# #test
# beta_A0_opt<-optimize(gen_target_optA, lower = -4, upper = 0,
#                       maximum = FALSE, ntarget=6000000, beta_A_C1=0.5, Aprop=0.45)
# 
# 
# #test
# beta_Z0_opt<-optimize(gen_target_optZ, lower = 44, upper = 46,
#                       maximum = FALSE, ntarget=6000000, beta_A_0=beta_A0_opt$minimum, beta_A_C1=0.5, beta_Z_A=0, Zmean=45)
# 
# 
# #test
# beta_Y0_opt<-optimize(gen_target_optY, lower = -10, upper = 10,
#                       maximum = FALSE, 
#                       
#                       ntarget=6000000, 
#                       
#                       beta_A_0=beta_A0_opt$minimum, beta_A_C1=0.5, 
#                       
#                       beta_Z_0=beta_Z0_opt$minimum, beta_Z_A=0, 
#                       
#                       beta_Y_A=0.13, beta_Y_Z=0.005, beta_Y_C1=.5, beta_Y_AC1=0.2, beta_Y_AZ=0, beta_Y_C1Z=0, Ymean=0)
# 










gen_target<-function(ntarget, 
                     beta_A_0, beta_A_C1, beta_A_C2,
                     beta_Z_0, beta_Z_A, 
                     beta_Y_0, beta_Y_A, beta_Y_Z, beta_Y_C1, beta_Y_C2, 
                     beta_Y_AC1, beta_Y_AC2, beta_Y_AZ, beta_Y_C1Z, beta_Y_C2Z, 
                     C1prev, C2form){
  targetpop<-data.frame( ID=seq(1:ntarget),
                         C1=rbinom(ntarget,1,C1prev),
                         C2=eval(parse(text=C2form))
  )
  targetpop$A=rbinom(ntarget, 1, exp(beta_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)/(1+exp(beta_A_0+beta_A_C1*targetpop$C1+beta_A_C2*targetpop$C2)))
  targetpop$Z=rnorm(ntarget,beta_Z_0+beta_Z_A*targetpop$A,10)
  targetpop$Y=rnorm(ntarget,beta_Y_0+beta_Y_C1*targetpop$C1+beta_Y_C2*targetpop$C2+beta_Y_A*targetpop$A+beta_Y_Z*targetpop$Z+
                      beta_Y_AC1*targetpop$A*targetpop$C1+beta_Y_AC2*targetpop$A*targetpop$C2+
                      beta_Y_AZ*targetpop$A*targetpop$Z+beta_Y_C1Z*targetpop$C1*targetpop$Z+beta_Y_C2Z*targetpop$C2*targetpop$Z,1)
  
  targetpop$C2_cat<-ifelse(targetpop$C2<(-5), 1,
                           ifelse(targetpop$C2>=(-5) & targetpop$C2<5, 2, 
                                  ifelse(targetpop$C2>=5, 3,NA)))
  
  return(targetpop)
}


# set.seed(12345)
# targetpop<-gen_target(
#   #Parameters:
#   #sample sizes
#   ntarget=6000000,
#   # A model
#   beta_A_0=beta_A0_opt$minimum, beta_A_C1=0.5, 
#   # Z model
#   beta_Z_0=beta_Z0_opt$minimum, beta_Z_A=0, 
#   # Y model
#   beta_Y_0=beta_Y0_opt$minimum, beta_Y_A=0.13, beta_Y_Z=0.005, beta_Y_C1=.5, beta_Y_AC1=0.2, beta_Y_AZ=0, beta_Y_C1Z=0
# )
# 
# mean(targetpop$A)
# mean(targetpop$Z)
# mean(targetpop$Y)
# sd(targetpop$Y)
#A is college education so approx 35% have it, per 2020 census https://www.census.gov/data/tables/2020/demo/educational-attainment/cps-detailed-tables.html. 
#Want Z to be "wealth" in thousands (but accept it being unrealistically normally distributed)
  #per census.gov 2019 data, median wealth/assets is is 41,200 excluding home equity https://www.census.gov/data/tables/2019/demo/wealth/wealth-asset-ownership.html 
#Want Y to have mean ~0, SD ~1 for being cognitive function

#unit testing
# targetpop<-gen_target(100000, beta_A_0=-0.3, beta_A_C1=0.5, beta_Z_0=34, beta_Z_A=15, beta_Y_0=0, beta_Y_A=.13, beta_Y_Z=0.006, beta_Y_C1=1, beta_Y_AC1=0, beta_Y_AZ=0, beta_Y_C1Z=0)
# mean(targetpop$C1)
# mean(targetpop$A)
# mean(targetpop$Z)
# mean(targetpop$Y)
# hist(targetpop$Y)
# hist(targetpop$Z)
# 
# 
# 
# lm(Y~A+C1+Z, targetpop)
