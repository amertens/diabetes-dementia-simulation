


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/functions/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))
source(paste0(here::here(),"/functions/0_simulation_cleaning_functions.R"))





#---------------------------------------------------------
# manuscript sim results: Null sim
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/null/"), pattern = "*.RDS")



#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/sim_res/null//bootstrap/"), pattern = "*.RDS")
length(boot_iter_files)


trueRR=1
trueRD=0
iptw=T
sim_res_null <- calc_sim_performance(files=files, filepath="/sim_res/null/", boot_iter_files=boot_iter_files, trueRD=0, iptw=T)
sim_res_null$filenames



saveRDS(sim_res_null,  file=paste0(here::here(),"/results/sim_performance_results_null.RDS"))




#---------------------------------------------------------
# manuscript sim results: protective sim
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/protective/"), pattern = "*.RDS")



boot_iter_files <- dir(path=paste0(here::here(),"/sim_res/bootstrap/protective/"), pattern = "*.RDS")

truth <- readRDS(paste0(here::here(),"/data/sim_res_truth.RDS"))
trueRD <- truth[10,3]






sim_res_protective <- calc_sim_performance(files=files, filepath="/sim_res/protective/", boot_iter_files=boot_iter_files,  trueRD= trueRD, iptw=T )

saveRDS(sim_res_protective,  file=paste0(here::here(),"/results/sim_performance_results_protective.RDS"))


