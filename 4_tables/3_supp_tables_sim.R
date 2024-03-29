
rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
try(source(paste0(here::here(),"/simulation study/0_simulation_functions.R")))

# load(paste0(here::here(),"/simulation study/sim_performance_results/T4_null_sim_res.Rdata"))
# load(paste0(here::here(),"/simulation study/sim_performance_results/T4_outcome_blind_sim_res.Rdata"))
# load(paste0(here::here(),"/simulation study/sim_performance_results/T10_sim_res.Rdata"))

# load(paste0(here::here(),"/results/sim_performance_results.Rdata"))
# load(paste0(here::here(),"/results/simulation_results_old.Rdata"))
load(paste0(here::here(),"/results/sim_performance_results_simple.Rdata"))
load(paste0(here::here(),"/results/sim_performance_results_original.Rdata"))
load(paste0(here::here(),"/results/sim_performance_results_original_null.Rdata"))
load(paste0(here::here(),"/results/performance_over_time.Rdata"))

library(knitr)


# Scenario 2: Realistic simulation, null outcome

#
# True RR: 1
#
# True RD: 0
#
# **Notes:**
#
#   * Outcome, death and censoring all jointly scrambled
# * Oracle coverage is pretty good but a little too high for all estimators, but GLM estimators have more variance

### RD oracle coverage of different estimators


#colnames(old_sim_res_null_diff)
#unique(old_sim_res_null_diff$filenames)
old_sim_res_null_diff_coverage <- old_sim_res_null_diff %>% #filter(variance_estimator=="ic") %>%
  filter(filenames %in% c("old_null_sim_res_noDetQ_ic_T11","old_null_sim_res_noDetQ_Qint_ic_T11","old_null_sim_res_ic_glm_T11","old_null_sim_res_noDetQ_ic_glm_T11","old_null_sim_res_Qint_noDetQ_glm_tmle_T11")) %>%
  filter(DetQ=="No") %>%
  select(estimator, Qint,bias,variance,mse,bias_se_ratio,o.coverage) %>%
  rename(`Oracle coverage`=o.coverage,
         Estimator=estimator,
         `Intercept-only Q`=Qint,
         Bias=bias,
         Variance=variance,
         MSE=mse,
         `Bias-SE ratio`=bias_se_ratio)

knitr::kable(old_sim_res_null_diff_coverage, digits =5)
knitr::kable(old_sim_res_null_diff_coverage, digits =5, format = "latex")



### RR oracle coverage of different estimators


old_sim_res_null_RR_coverage <- old_sim_res_null_RR %>% #filter(variance_estimator=="ic") %>%
  filter(filenames %in% c("old_null_sim_res_noDetQ_ic_T11","old_null_sim_res_noDetQ_Qint_ic_T11","old_null_sim_res_ic_glm_T11","old_null_sim_res_noDetQ_ic_glm_T11","old_null_sim_res_Qint_noDetQ_glm_tmle_T11",
                          "old_null_sim_res_noDetQ_ic_T11_iptw","old_null_sim_res_noDetQ_Qint_ic_T11_iptw","old_null_sim_res_ic_glm_T11_iptw","old_null_sim_res_noDetQ_ic_glm_T11_iptw","old_null_sim_res_Qint_noDetQ_glm_tmle_T11_iptw")) %>%
  mutate(iptw=ifelse(grepl("_iptw",filenames),"yes","no")) %>%
  filter(DetQ=="No") %>%
  select(estimator, Qint, iptw, bias,variance,mse,bias_se_ratio,o.coverage) %>%
  rename(`Oracle coverage`=o.coverage,
         Estimator=estimator,
         `Intercept-only Q`=Qint,
         Bias=bias,
         Variance=variance,
         IPTW=iptw,
         MSE=mse,
         `Bias-SE ratio`=bias_se_ratio)

knitr::kable(old_sim_res_null_RR_coverage, digits = 3)
knitr::kable(old_sim_res_null_diff_coverage, digits =3, format = "latex")


## Performance of difference variance estimators on null data

# **Notes:**
#
#   * Only showing LASSO estimator results-all estimator performances assessed in the realistic simulated data below.
# * Sanity-check on estimation performance on data with a known null association between GLP1 and dementia.
# * The IC variance estimator is anti-conservative and the TMLE variance estimator is conservative.
# * The bootstrap is anti-conservative but less so than the IC variance estimator.
# * The TMLE estimator is very conservative, with CI widths 8-10X that of the bootstrap.
# * The IPTW estimator is uniformly biased with overly-wide confidence intervals in all simulations (not shown).

### Risk difference performance


#colnames(old_sim_res_null_diff)
#unique(old_sim_res_null_diff$filenames)
old_sim_res_null_diff$variance_estimator[is.na(old_sim_res_null_diff$variance_estimator)] <- "bootstrap"
old_sim_res_null_diff_coverage <- old_sim_res_null_diff %>%
  filter(estimator=="LASSO"& Qint=="No"& DetQ=="No" |is.na(bias)) %>%
  select(variance_estimator,coverage, mean_ci_width)

knitr::kable(old_sim_res_null_diff_coverage, digits =5)
knitr::kable(old_sim_res_null_diff_coverage, digits =5, format = "latex")



### Relative risk performance
old_sim_res_null_RR$variance_estimator[is.na(old_sim_res_null_RR$variance_estimator)] <- "bootstrap"
old_sim_res_null_RR_coverage <- old_sim_res_null_RR %>% filter(estimator=="LASSO"& Qint=="No"& DetQ=="No" |is.na(bias)) %>%
  filter(!grepl("iptw",filenames)) %>%
  select(variance_estimator,coverage, mean_ci_width)

knitr::kable(old_sim_res_null_RR_coverage, digits =5)




# Scenario 3: Realistic simulation, protective effect of GLP1 on dementia


# True RD: -0.009683665
#
# True RR: 0.5148661
#
# ## Comparison of different estimators' performance
#
# **Notes:**
#
#   * Based on these results, we chose the LASSO estimator with Q-prediction and no deterministic Q function
# * Several of the estimators have comparable performance, but the chosen estimator performs best in both RR and RD estimation
# * Ridge regressions have lower MSE but not perfect 95% oracle coverage
# * Including the deterministic Q function marginally decreases bias/variance, so we should use in the bootstrap estimator


### Risk difference

#colnames(old_sim_res_v3_diff)
#unique(old_sim_res_v3_diff$filenames)
old_sim_res_v3_diff_coverage <- old_sim_res_v3_diff %>% subset(., select = -c(bias_se_ratio_emp, power, censoring_in_data, mean_ci_width, simulated_data))
old_sim_res_v3_diff_coverage <- old_sim_res_v3_diff_coverage %>% filter(variance_estimator=="ic" | is.na(variance_estimator), !is.na(bias))

#unique(old_sim_res_v3_diff_coverage$filenames)
old_sim_res_v3_diff_coverage$estimator <- "LASSO"
old_sim_res_v3_diff_coverage$estimator[grepl("rf",old_sim_res_v3_diff_coverage$filenames)] <- "Random Forest"
old_sim_res_v3_diff_coverage$estimator[grepl("glm",old_sim_res_v3_diff_coverage$filenames)] <- "GLM"
old_sim_res_v3_diff_coverage$estimator[grepl("ridge",old_sim_res_v3_diff_coverage$filenames)] <- "Ridge"
old_sim_res_v3_diff_coverage$estimator[grepl("EN",old_sim_res_v3_diff_coverage$filenames)] <- "Elastic Net"
old_sim_res_v3_diff_coverage$estimator[grepl("lasso_prescreen",old_sim_res_v3_diff_coverage$filenames)] <- "GLM, LASSO prescreen"

# old_sim_res_v3_diff_coverage$estimator[grepl("_DetQ",old_sim_res_v3_diff_coverage$filenames)] <- paste0(old_sim_res_v3_diff_coverage$estimator[grepl("_DetQ",old_sim_res_v3_diff_coverage$filenames)],", Det-Q")
# old_sim_res_v3_diff_coverage$estimator[grepl("_detQ",old_sim_res_v3_diff_coverage$filenames)] <- paste0(old_sim_res_v3_diff_coverage$estimator[grepl("_detQ",old_sim_res_v3_diff_coverage$filenames)],", Det-Q")

old_sim_res_v3_diff_coverage$estimator[old_sim_res_v3_diff_coverage$Qint=="Yes"] <- paste0(old_sim_res_v3_diff_coverage$estimator[old_sim_res_v3_diff_coverage$Qint=="Yes"],", Q-intercept")
old_sim_res_v3_diff_coverage$estimator[grepl("1se",old_sim_res_v3_diff_coverage$filenames)] <- paste0(old_sim_res_v3_diff_coverage$estimator[grepl("1se",old_sim_res_v3_diff_coverage$filenames)],", Lambda: 1se")
old_sim_res_v3_diff_coverage$estimator[grepl("AUC",old_sim_res_v3_diff_coverage$filenames)] <- paste0(old_sim_res_v3_diff_coverage$estimator[grepl("AUC",old_sim_res_v3_diff_coverage$filenames)],", AUC fit")

old_sim_res_v3_diff_coverage <- old_sim_res_v3_diff_coverage %>%
  select(estimator, Qint, DetQ, bias, variance, mse, bias_se_ratio, o.coverage) %>% arrange(o.coverage)  %>%
  rename(`Oracle coverage`=o.coverage,
         Estimator=estimator,
         `Intercept-only Q`=Qint,
         Bias=bias,
         Variance=variance,
         MSE=mse,
         `Bias-SE ratio`=bias_se_ratio) %>%
  distinct()

knitr::kable(old_sim_res_v3_diff_coverage, digits =6)
knitr::kable(old_sim_res_v3_diff_coverage, digits =6, format = "latex")




### Relative Risk

old_sim_res_v3_RR_coverage <- old_sim_res_v3_RR %>% subset(., select = -c(bias_se_ratio_emp, coverage, power, censoring_in_data, mean_ci_width, simulated_data))
old_sim_res_v3_RR_coverage <- old_sim_res_v3_RR_coverage %>% filter(variance_estimator=="ic" | is.na(variance_estimator), !is.na(bias), !grepl("iptw",filenames))

#unique(old_sim_res_v3_RR_coverage$filenames)
old_sim_res_v3_RR_coverage$estimator <- "LASSO"
old_sim_res_v3_RR_coverage$estimator[grepl("rf",old_sim_res_v3_RR_coverage$filenames)] <- "Random Forest"
old_sim_res_v3_RR_coverage$estimator[grepl("glm",old_sim_res_v3_RR_coverage$filenames)] <- "GLM"
old_sim_res_v3_RR_coverage$estimator[grepl("ridge",old_sim_res_v3_RR_coverage$filenames)] <- "Ridge"
old_sim_res_v3_RR_coverage$estimator[grepl("EN",old_sim_res_v3_RR_coverage$filenames)] <- "Elastic Net"
old_sim_res_v3_RR_coverage$estimator[grepl("lasso_prescreen",old_sim_res_v3_RR_coverage$filenames)] <- "GLM, LASSO prescreen"
old_sim_res_v3_RR_coverage$estimator[grepl("_DetQ",old_sim_res_v3_RR_coverage$filenames)] <- paste0(old_sim_res_v3_RR_coverage$estimator[grepl("_DetQ",old_sim_res_v3_RR_coverage$filenames)],", Det-Q")
old_sim_res_v3_RR_coverage$estimator[grepl("_detQ",old_sim_res_v3_RR_coverage$filenames)] <- paste0(old_sim_res_v3_RR_coverage$estimator[grepl("_detQ",old_sim_res_v3_RR_coverage$filenames)],", Det-Q")

old_sim_res_v3_RR_coverage$estimator[old_sim_res_v3_RR_coverage$Qint=="Yes"] <- paste0(old_sim_res_v3_RR_coverage$estimator[old_sim_res_v3_RR_coverage$Qint=="Yes"],", Q-intercept")
old_sim_res_v3_RR_coverage$estimator[grepl("1se",old_sim_res_v3_RR_coverage$filenames)] <- paste0(old_sim_res_v3_RR_coverage$estimator[grepl("1se",old_sim_res_v3_RR_coverage$filenames)],", Lambda: 1se")
old_sim_res_v3_RR_coverage$estimator[grepl("AUC",old_sim_res_v3_RR_coverage$filenames)] <- paste0(old_sim_res_v3_RR_coverage$estimator[grepl("AUC",old_sim_res_v3_RR_coverage$filenames)],", AUC fit")

old_sim_res_v3_RR_coverage <- old_sim_res_v3_RR_coverage %>%
  select(estimator, bias, variance, mse, o.coverage) %>% arrange(o.coverage) %>% rename(oracle.coverage=o.coverage)

knitr::kable(old_sim_res_v3_RR_coverage, digits =3)




## Comparison of different variance estimators



# **Notes:**
#
#   * Showing LASSO estimator results with modeled Q (rather than intercept-only)
# * The IC variance estimator is anti-conservative and the TMLE variance estimator is conservative
# * The bootstrap is anti-conservative but less so than the IC variance estimator
# * The IPTW estimator is uniformly biased with overly-wide confidence intervals in all simulations (not shown)

### Risk difference coverage


old_sim_res_v3_diff_var <- old_sim_res_v3_diff %>% subset(., select = -c(bias,variance,mse,bias_se_ratio, censoring_in_data, o.coverage, simulated_data))
old_sim_res_v3_diff_var <- old_sim_res_v3_diff_var %>% filter(Qint  =="No" , !grepl("iptw",filenames)|is.na(variance_estimator), !grepl("ridge",filenames), !grepl("AUC",filenames),
                                                              !grepl("EN",filenames), !grepl("1se",filenames), !grepl("glm",filenames), !grepl("_lasso_prescreen",filenames),
                                                              filenames!="im_res_DetQ_ic_v3",
                                                              filenames=="no DetQ_iptw-IPTW"|!grepl("iptw",filenames))

old_sim_res_v3_diff_var$variance_estimator[grepl("_DetQ",old_sim_res_v3_diff_var$filenames)] <- paste0(old_sim_res_v3_diff_var$variance_estimator[grepl("_DetQ",old_sim_res_v3_diff_var$filenames)],", Det-Q")
old_sim_res_v3_diff_var <- old_sim_res_v3_diff_var %>% mutate(variance_estimator=case_when(
  filenames=="DetQ" ~ "Bootstrap, Det Q function",
  filenames=="DetQ, 500 iter" ~ "Bootstrap, Det Q function, 500 iterations",
  filenames=="no DetQ" ~ "Bootstrap",
  filenames=="Ridge" ~ "Bootstrap-Ridge",
  filenames=="no DetQ_iptw-IPTW" ~ "Bootstrap- IPTW",
  variance_estimator==variance_estimator ~variance_estimator
)) #%>% filter(filenames!="DetQ, 500 iter")

old_sim_res_v3_diff_var <- old_sim_res_v3_diff_var %>% select(variance_estimator, coverage, mean_ci_width, power, bias_se_ratio_emp)



knitr::kable(old_sim_res_v3_diff_var, digits =5)





### Relative risk coverage
old_sim_res_v3_RR_var <- old_sim_res_v3_RR %>% subset(., select = -c(bias,variance,mse,bias_se_ratio, censoring_in_data, o.coverage, simulated_data))
old_sim_res_v3_RR_var <- old_sim_res_v3_RR_var %>% filter(Qint  =="No" , !grepl("iptw",filenames)|is.na(variance_estimator), !grepl("ridge",filenames), !grepl("AUC",filenames),
                                                          !grepl("EN",filenames), !grepl("1se",filenames), !grepl("glm",filenames), !grepl("_lasso_prescreen",filenames),
                                                          filenames!="im_res_DetQ_ic_v3",
                                                          filenames=="no DetQ_iptw-IPTW"|!grepl("iptw",filenames))

old_sim_res_v3_RR_var$variance_estimator[grepl("_DetQ",old_sim_res_v3_RR_var$filenames)] <- paste0(old_sim_res_v3_RR_var$variance_estimator[grepl("_DetQ",old_sim_res_v3_RR_var$filenames)],", Det-Q")
old_sim_res_v3_RR_var <- old_sim_res_v3_RR_var %>% mutate(variance_estimator=case_when(
  filenames=="DetQ" ~ "Bootstrap, Det Q function",
  filenames=="DetQ, 500 iter" ~ "Bootstrap, Det Q function, 500 iterations",
  filenames=="Ridge" ~ "Bootstrap-Ridge",
  filenames=="no DetQ" ~ "Bootstrap",
  filenames=="no DetQ_iptw-IPTW" ~ "Bootstrap- IPTW",
  variance_estimator==variance_estimator ~variance_estimator
)) #%>% filter(filenames!="DetQ, 500 iter")

old_sim_res_v3_RR_var <- old_sim_res_v3_RR_var %>% select(variance_estimator, coverage, mean_ci_width, power, bias_se_ratio_emp)



knitr::kable(old_sim_res_v3_RR_var, digits =3)




# ## Comparison of variance estimator performance over time
# #The primary analysis examined the effect of continuous GLP1 usage on dementia risk after 5 years, with longitudinal data discretized into 6 month time nodes. The imperfect performance of estimators in simulations may arise from the rare outcome (~2% prevalence after 5 years), positivity issues in the long-term followup (with increasingly small number of individuals continuously on GLP1), or high degrees of administrative censoring (~50% after 5 years). We ran simulations for all length of followup time from 6 months (time=1) to 5 years (time=10). Oracle coverage is good at all times, while IC coverage is increasingly anti-conservative and TMLE coverage is increasingly conservative over time. Interestingly, variance in RD estimates increases more over time while bias increases more in RR estimates.
#
#
# ### Risk difference
# perf_tab_over_time_diff <- perf_tab_over_time_diff %>% rename(oracle.coverage=o.coverage)
# knitr::kable(perf_tab_over_time_diff, digits =5)
#
#
#
# ### Relative risk
# perf_tab_over_time_RR <- perf_tab_over_time_RR %>% rename(oracle.coverage=o.coverage)
# knitr::kable(perf_tab_over_time_RR, digits =3)
#
#
