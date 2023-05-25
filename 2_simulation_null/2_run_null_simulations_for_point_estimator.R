
rm(list=ls())
library(here)
source(paste0(here::here(),"/0_config.R"))
source(paste0(here::here(),"/functions/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=64)

d_wide_list <- readRDS(file=paste0(here::here(),"/data/simulated_data_list_null.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()




#--------------------------------------------------------------------------
# GLM
#--------------------------------------------------------------------------


#TEMP! No parallelization:
# resdf_noDetQ_ic_glm <- NULL
# for(i in 1:length(d_wide_list)){
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11, SL.library="glm"))
#   resdf_noDetQ_ic_glm <- bind_rows(resdf_noDetQ_ic_glm, res)
# }
# resdf_noDetQ_ic_glm


resdf_noDetQ_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_noDetQ_ic_glm, paste0(here::here(),"/sim_res/null/sim_res_noDetQ_ic_glm.RDS"))


resdf_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_ic_glm, paste0(here::here(),"/sim_res/null/sim_res_ic_glm.RDS"))


resdf_Qint_noDetQ_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_Qint_noDetQ_ic_glm, paste0(here::here(),"/sim_res/null/sim_res_Qint_noDetQ_ic_glm.RDS"))


resdf_Qint_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_Qint_ic_glm, paste0(here::here(),"/sim_res/null/sim_res_Qint_ic_glm.RDS"))


#--------------------------------------------------------------------------
# LASSO
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# RIDGE
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# EN
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# GLM, LASSO prescreen
#--------------------------------------------------------------------------

#lasso prescreen
resdf_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))
  return(res)
}
saveRDS(resdf_noDetQ_lasso_prescreen, paste0(here::here(),"/sim_res/null/sim_res_noDetQ_lasso_prescreen.RDS"))

resdf_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))
  return(res)
}
saveRDS(resdf_lasso_prescreen, paste0(here::here(),"/sim_res/null/sim_res_lasso_prescreen.RDS"))

resdf_Qint_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))
  return(res)
}
saveRDS(resdf_Qint_noDetQ_lasso_prescreen, paste0(here::here(),"/sim_res/null/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))

resdf_Qint_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))
  return(res)
}
saveRDS(resdf_Qint_lasso_prescreen, paste0(here::here(),"/sim_res/null/sim_res_Qint_lasso_prescreen.RDS"))



#--------------------------------------------------------------------------
#Random forest
#--------------------------------------------------------------------------
int.start.time <- Sys.time()
resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.randomForest", det.Q=F, N_time=11, override_function=SuperLearner_override_RF)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_RF

gc()
saveRDS(resdf_RF, paste0(here::here(),"/sim_res/null/sim_res_rf_ic.RDS"))

int.start.time <- Sys.time()
resdf_RF_detQ <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.randomForest", det.Q=T, N_time=11, override_function=SuperLearner_override_RF)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
gc()
saveRDS(resdf_RF_detQ, paste0(here::here(),"/sim_res/null/sim_res_rf_ic.RDS"))

#ADD Qint models



###OTHER

#Ridge
int.start.time <- Sys.time()
resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.glmnet", N_time=11, override_function=SuperLearner_override_ridge)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_RF

gc()
saveRDS(resdf_RF, paste0(here::here(),"/sim_res/null/sim_res_ridge_ic.RDS"))

#Elastic Net
int.start.time <- Sys.time()
resdf_EN <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.glmnet", N_time=11, override_function=SuperLearner_override_EN)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_EN

gc()
saveRDS(resdf_EN, paste0(here::here(),"/sim_res/null/sim_res_EN_ic.RDS"))


#lasso prescreen
resdf_Qint_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))

  return(res)
}
saveRDS(resdf_Qint_noDetQ_lasso_prescreen, paste0(here::here(),"/sim_res/null/sim_res_ic_Qint_noDetQ_lasso_prescreen.RDS"))

#This ones running forever
# resdf_Qint_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))
#
#   return(res)
# }
# saveRDS(resdf_Qint_noDetQ_lasso_prescreen, paste0(here::here(),"/sim_res/null/sim_res_ic_Qint_DetQ_lasso_prescreen.RDS"))
#

resdf_AUC <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, varmethod = "ic",override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_AUC, paste0(here::here(),"/sim_res/null/sim_res_AUC.RDS"))


resdf_Qint_AUC <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, varmethod = "ic",override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_Qint_AUC, paste0(here::here(),"/sim_res/null/sim_res_Qint_AUC.RDS"))


#primary
int.start.time <- Sys.time()
resdf_noDetQ_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/sim_res/null/sim_res_noDetQ_tmle.RDS"))

int.start.time <- Sys.time()
resdf_noDetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_ic, paste0(here::here(),"/sim_res/null/sim_res_noDetQ_ic.RDS"))

summary(resdf_noDetQ_ic$ate)
summary(resdf_noDetQ_ic$iptw.ate)

int.start.time <- Sys.time()
resdf_noDetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_ic, paste0(here::here(),"/sim_res/null/sim_res_ic.RDS"))



#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=T, det.Q=FALSE, varmethod = "tmle",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/sim_res/null/sim_res_noDetQ_Qint_tmle.RDS"))

int.start.time <- Sys.time()
resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=T, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/sim_res/null/sim_res_noDetQ_Qint_ic.RDS"))


