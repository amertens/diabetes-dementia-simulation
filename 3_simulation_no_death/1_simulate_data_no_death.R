

rm(list=ls())
library(lava)
library(data.table)
source(paste0(here::here(),"/functions/0_synthesizeDD.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
cc_no_death <- cc %>% filter(!grepl("event_death", var))

u <- synthesizeDD(cc_no_death)


clean_sim_data_no_death <- function(d, N_time){

  #d <- as.data.frame(sapply(d, as.numeric))
  #d[is.na(d)] <- 0 #Missingness due to censoring should be coded 0 as long as censoring variable is equal to 1.
  d<- data.table(d)

  for(i in 1:(N_time+1)){
    j=i+1
    d[get(paste0("event_dementia_",i))==1, (paste0("event_dementia_",j)):=1]
  }

  dementia.nodes<- grep("event_dementia_",names(d))
  d[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]
  return(d)
}



set.seed(12345)
d_wide_list <- NULL
n<-200
N_time=10


for(i in 1:n){
  cat("\ni: ",i,"\n")

  d <- sim(u,115698)

  d$dem_prev <-    mean(1*(d$event_dementia_1 +
                        d$event_dementia_2 +
                        d$event_dementia_3 +
                        d$event_dementia_4 +
                        d$event_dementia_5 +
                        d$event_dementia_6 +
                        d$event_dementia_7 +
                        d$event_dementia_8 +
                        d$event_dementia_9 +
                        d$event_dementia_10 >0))

  d<- clean_sim_data_no_death(d, N_time = 10)

  d_wide_list[[i]] <- d
  gc()
}


saveRDS(d_wide_list, paste0(here::here(),"/data/simulated_data_no_death_list.RDS"))



