
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/functions/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))


library(parallel)
library(doParallel)
if(detectCores()>64){
  registerDoParallel(cores=64)
}else{
  registerDoParallel(cores=detectCores())

}

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_no_death_list"))
d_wide_list <- d_wide_list[200:400]
gc()




i<-j<-1
resdf_boot = NULL

for(i in 1:200){

  cat(i,"\n")
  d <- d_wide_list[[i]]
  d$id <- 1:nrow(d)
  d<-d %>% select(id, everything())

  res_df <- NULL
  res_df <- foreach(j = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {

    source(here::here("0_config.R"))
    source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
    source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

    set.seed(j)
    dboot <- d[sample(.N, nrow(d),replace=TRUE)]


    res <- NULL
    try(res <- run_ltmle_glmnet_no_death(dboot, N_time = 11, resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic", id=dboot$id), silent=TRUE)
    return(res)
  }
  res_df

  gc()
  res_df$iteration <- i
  resdf_boot <- bind_rows(resdf_boot, res_df)
  saveRDS(res_df, paste0(here::here(),"/sim_res/no_death/bootstrap/sim_res_boot_",i,".RDS"))

}
