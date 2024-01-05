GetStdPop<-function(data){
                    targetsamp_total <- nrow(data)
                    C1C2_totals <- data %>% group_by(C1, C2_cat) %>% 
                      summarise(n = n())
                    C1C2_totals$prop_C1C2<-C1C2_totals$n/targetsamp_total
                    
                    C1C2_totals <- C1C2_totals %>% select(C1, C2_cat, prop_C1C2)
return(C1C2_totals)
}

#test<-GetStdPop(targetsamp)
