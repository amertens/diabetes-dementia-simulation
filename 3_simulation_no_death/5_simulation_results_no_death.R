



rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))
source(paste0(here::here(),"/functions/0_simulation_cleaning_functions.R"))
library(knitr)
library(xtable)
library(kableExtra)


files <- dir(path=paste0(here::here(),"/sim_res/no_death/"), pattern = "*.RDS")

truth <- readRDS(paste0(here::here(),"/data/sim_res_truth_no_death.RDS"))
truth$RD[10]

res_protective <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/no_death/"), files=files, boot_iter_files=NULL, trueRR=truth$RR[10], trueRD= truth$RD[10], iptw=T )
res_diff <- res_protective$perf_tab_diff
res_diff

# res_protective <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/no_death/"), files=files, boot_iter_files=NULL, trueRR=truth$RR[10], trueRD= -0.007917, iptw=T )
# res_diff <- res_protective$perf_tab_diff
# res_diff
#
#
# #after death/dementia ordered cleaning
# res_protective <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/no_death/"), files=files, boot_iter_files=NULL, trueRR=0.6964379, trueRD= -0.005829, iptw=T )
# res_diff <- res_protective$perf_tab_diff
# res_diff
#
#
# res_protective <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/no_death/"), files=files, boot_iter_files=NULL, trueRR=0.6964379, trueRD= -0.010047, iptw=T )
# res_diff <- res_protective$perf_tab_diff
# res_diff







setwd(paste0(here::here(),"/sim_res/no_death/"))
d <- files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="analysis")
head(d)
table(d$analysis)

d <- d %>% group_by(analysis) %>% mutate(rep=row_number())
ggplot(d, aes(x=rep, y=ate)) + facet_wrap(~analysis) +
  geom_point() +
  geom_linerange(aes(ymin=ate.ci.lb, ymax=ate.ci.ub)) +
  geom_hline(yintercept = -0.0354950000) + coord_flip()


ggplot(d, aes(x=rep, y=estimate)) + facet_wrap(~analysis) +
  geom_point() +
  geom_linerange(aes(ymin=CI.2.5., ymax=CI.97.5.)) +
  geom_hline(yintercept = 0.3940834) + coord_flip() +
  scale_y_continuous(trans = "log10")



