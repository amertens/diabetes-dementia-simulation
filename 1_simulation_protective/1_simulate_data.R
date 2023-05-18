

rm(list=ls())
library(lava)
library(data.table)
source(paste0(here::here(),"/functions/0_synthesizeDD.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
u <- synthesizeDD(cc)

set.seed(12345)
sim_list <- NULL
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

  d<- clean_sim_data(d, N_time = 10)

  sim_list[[i]] <- d
  gc()
}


saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list.RDS"))



