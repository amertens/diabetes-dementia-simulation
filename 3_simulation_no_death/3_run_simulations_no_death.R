
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/functions/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))


library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_no_death_list.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()



# d=d_wide_list[[1]]
# resdf=NULL
# Qint=FALSE
# det.Q =TRUE
# varmethod = "ic"
#
# try(res <- run_ltmle_glmnet_no_death(d_wide_list[[1]], resdf=NULL, Qint=FALSE, det.Q =FALSE, varmethod = "ic"))


#lasso
int.start.time <- Sys.time()
resdf_DetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_death(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =FALSE, varmethod = "ic"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
saveRDS(resdf_DetQ_ic, paste0(here::here(),"/sim_res/no_death/sim_res_ic.RDS"))

int.start.time <- Sys.time()
resdf_DetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_death(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =FALSE, varmethod = "tmle"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
saveRDS(resdf_DetQ_ic, paste0(here::here(),"/sim_res/no_death/sim_res_tmle.RDS"))




#lasso
int.start.time <- Sys.time()
resdf_DetQ_ic_t2 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_death(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =FALSE, varmethod = "ic", N_time = 2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
saveRDS(resdf_DetQ_ic_t2, paste0(here::here(),"/sim_res/no_death/sim_res_ic_t2.RDS"))

int.start.time <- Sys.time()
resdf_DetQ_ic_t2 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_death(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =FALSE, varmethod = "tmle", N_time = 2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
saveRDS(resdf_DetQ_ic_t2, paste0(here::here(),"/sim_res/no_death/sim_res_tmle_t2.RDS"))
