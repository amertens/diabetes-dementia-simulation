


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))





#---------------------------------------------------------
# manuscript sim results: Null sim
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/null/"), pattern = "*.RDS")


# #NOTE: check which files are missing from the old pipeline:
# old_files <- dir(path=paste0(here::here(),"/sim_res_old/"), pattern = "*.RDS")
# old_files <- old_files[grepl("old_null_sim_res_",old_files)]

# files_temp[!(files_temp %in% old_files)]
# old_files[!(old_files %in% files_temp)]


#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("sim_res_boot_old_sim_null_T11_",boot_iter_files)]
length(boot_iter_files)


trueRR=1
trueRD=0
iptw=T
original_sim_res_null <- calc_sim_performance(files=files, boot_iter_files=boot_iter_files, trueRR=1, trueRD=0, iptw=T)
original_sim_res_null$perf_tab_RR
original_sim_res_null$perf_tab_diff
original_sim_res_null$perf_tab_diff$filenames


res_null_diff <- original_sim_res_null$perf_tab_diff
res_null_RR <- original_sim_res_null$perf_tab_RR

save(res_null_diff, res_null_RR,  file=paste0(here::here(),"/results/sim_performance_results_original_null.Rdata"))


#explore answer to Maya's comments:
df <- res_null_diff %>% filter(iptw=="IPTW", estimator=="GLM")
df


df <- res_null_diff %>% filter(iptw=="IPTW", estimator=="LASSO", variance_estimator=="ic")
df

df <- res_null_diff %>% filter(o.coverage > 95)
df


#---------------------------------------------------------
# manuscript sim results: protective sim
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
#files <- files[grepl("_v3",files)]
files <- files[!grepl("_null_",files)]

#NOTE: check which files are missing from the old pipeline:
old_files <- dir(path=paste0(here::here(),"/sim_res_old/"), pattern = "*.RDS")
old_files <- old_files[grepl("_v3",old_files)]
old_files <- gsub("_v3","",old_files)


files_temp <- files[!(files %in% c( "sim_res_1se.RDS","sim_res_AUC.RDS" ))]
old_files <- old_files[!(old_files %in% c( "sim_res_1se_ic.RDS","sim_res_AUC_ic.RDS" ))]


files_temp <- gsub(".RDS","", files_temp)
old_files <- gsub("_T11","", old_files)

files_temp[!(files_temp %in% old_files)]
old_files[!(old_files %in% files_temp)]


boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
unique(gsub("\\d","",boot_iter_files))

boot_iter_files_500iter <- boot_iter_files[grepl("sim_res_boot_old_sim_cens_competing_risks_500_iter_T11",boot_iter_files)]
boot_iter_files <- boot_iter_files[grepl("_v3",boot_iter_files)|grepl("sim_res_boot_original_sim_T11_",boot_iter_files)|grepl("sim_res_boot_old_sim_detQ_T11",boot_iter_files)]
#boot_iter_files <- boot_iter_files[!grepl("ridge",boot_iter_files)]
boot_iter_files <- c(boot_iter_files, boot_iter_files_500iter)
boot_iter_files <- data.frame(boot_file = boot_iter_files, analysis=NA)
boot_iter_files$analysis[grepl("500_iter",boot_iter_files$boot_file)] <- "DetQ, 500 iter"
boot_iter_files$analysis[grepl("sim_res_boot_old_sim_detQ_T11",boot_iter_files$boot_file)] <- "DetQ"
boot_iter_files$analysis[grepl("ridge",boot_iter_files$boot_file)] <- "Ridge"
boot_iter_files$analysis[grepl("cens_competing_risks_v3",boot_iter_files$boot_file) | grepl("sim_res_boot_original_sim_T11_",boot_iter_files$boot_file)] <- "no DetQ"
table(boot_iter_files$analysis)






trueRR=0.5148661
trueRD= -0.009683665


files=files
boot_iter_files=boot_iter_files
trueRR=trueRR
trueRD= trueRD
iptw=T


res_v3 <- calc_sim_performance(files=files, boot_iter_files=boot_iter_files, trueRR=trueRR, trueRD= trueRD, iptw=T )


res_v3_diff <- res_v3$perf_tab_diff
res_v3_RR <- res_v3$perf_tab_RR

#remove duplicates /iptw repeats
res_v3_diff <- res_v3_diff %>% filter(!filenames %in% c("sim_res_ridge_ic_v3_iptw","sim_res_ridge","sim_res_Qint_ic","sim_res_Qint_ic_v3_iptw",
                                                        "sim_res_DetQ__ridge_ic_v3_iptw","sim_res_ridge_ic_v3","sim_res_DetQ_ic_v3_iptw","sim_res_ic"))

save(res_v3_diff, res_v3_RR,  file=paste0(here::here(),"/results/sim_performance_results_original.Rdata"))

