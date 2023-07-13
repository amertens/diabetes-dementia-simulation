# JICI project
#
# Collaborators: Nerissa, Andrew, Bochra, Kathrine, Zeyi, Maya, Mark, Thomas, Christian
#
# Register data until 2018 are located at x:/Data/Rawdata_Hurtig/706582
# Corona update data are in the latest delivery (look for updates!)
# Currently we use
#  V:/Data/Workdata/706582/Corona_update/Data/11. levering.
#
# setwd("z:/Workdata/706582/Andrew Mertens/targets_diabetes_dementia/")
library(targets)

#load packages
tar_option_set(packages=c("heaven",
                          "ltmle",
                          "data.table",
                          "tidyverse",
                          "SuperLearner",
                          "tictoc",
                          "glmnet",
                          "Matrix",
                          "Publish",
                          "matrixStats",
                          "speedglm"
                          ,"doParallel"
                          ,"parallel"
                          ,"caret"
                          ,"snow"
                          ,"doSNOW"
                          ,"foreach"))

# -------------------------------------------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------------------------------------------

nix=lapply(list.files("./functions/", full.names = TRUE, recursive=TRUE), source)

  # #set up parallelization
  #  #use about half of space
  # ncores <- floor(detectCores()/2)
  # mycluster <- parallel::makeCluster(ncores)
  # doSNOW::registerDoSNOW(cl=mycluster)

# -------------------------------------------------------------------------------------------------------------
# Simulation parameters
# -------------------------------------------------------------------------------------------------------------

set.seed(4534)

#define some important global variables
dataset_N = 1000
sim_reps = 10

#######
#set up config for the boostrap if running it:
# boot==TRUE
# if(make_boot==TRUE){
# #   #set up parallelization
# #   #use about half of space
#   ncores <- 4#floor(detectCores()/2)
#   mycluster <- parallel::makeCluster(ncores)
#   doSNOW::registerDoSNOW(cl=mycluster)
# }
##############

list(
  # set some simple targets that will be used for cutoff points for continuous variables
  # for exposure definition, etc.
  tar_target(varmethod, "ic"),
  tar_target(breaks_hba1c, c(0,48,53,58,70,150)),
  tar_target(druglist,list(
    insulin="A10A",
    metformin=c("A10BA02","A10BD02","A10BD03","A10BD05","A10BD07","A10BD08","A10BD10",
                "A10BD11","A10BD13","A10BD14","A10BD15","A10BD16","A10BD17",
                "A10BD18","A10BD20","A10BD22","A10BD23","A10BD25","A10BD26"),
    biguanidk_o=c("A10BA01","A10BA03","A10BD01"),
    sulfonylurea=c("A10BB","A10BD01","A10BD02","A10BD04","A10BD06"),
    sulfomid="A10BC",
    alfa_glusidase_inhib=c("A10BF","A10BD17"),
    thiazolodinidion=c("A10BG","A10BD03","A10BD04","A10BD05","A10BD06",
                       "A10BD09","A10BD12","A10BD26"),
    dpp4_inhib=c("A10BH","A10BD07","A10BD08","A10BD09","A10BD10","A10BD11",
                 "A10BD12","A10BD13","A10BD18","A10BD19","A10BD21","A10BD22",
                 "A10BD24","A10BD25"),
    glp1="A10BJ",
    sglt2_inhib=c("A10BK","A10BD15","A10BD16","A10BD19","A10BD20",
                  "A10BD21","A10BD23","A10BD24","A10BD25"),
    repaglinid=c("A10BX","A10BD14")
  )),
  tar_target(baseline_vars, c("age_base","sex", "code5txt", "quartile_income")),#,metformin_dur",
  # "year_2nd_line_start","code5txt_miss","quartile_income_miss" )),
  tar_target(long_covariates, c("insulin_", "chronic.pulmonary.disease_",
                                "hypertension_", "myocardial.infarction_", "ischemic.heart.disease_",
                                "heart.failure_", "renal.disease_","event_death_"))#, "stroke_"))
  #export these
  ,tar_target(export_globalvars,{

    drugs<- lapply(druglist,FUN=function(x)paste(x,collapse=","))
    drugs <-data.frame(unlist(druglist))
    fwrite(drugs,file=paste0("./export/druglist_",todaydate,".txt"),row.names=T)

    vars <- data.frame(c(baseline_vars,long_covariates,final_date))
    fwrite(vars,file=paste0("./export/covars_",todaydate,".txt"),row.names=F)


  }),


  # -------------------------------------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------------------------------------
  # ## run main analyses ----------------------------------------------------------------
  # -------------------------------------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------------------------------------
  #
  ###for the simulated data at Berkeley, need to make the dtaaset within targets
  tar_target(wide_df, {
    simdata_100k <- readRDS("../../data/simdata_100k.rds")
    #add in variable for prior dementia
    simdata_100k$prior_dementia <- 0
    #make seqdate between 2009 and 2011
    simdata_100k$secdate <- sample(seq(as.Date('2009/01/01'),as.Date(final_date),by="day"),replace=T,size=nrow(simdata_100k))
    #make BL hba1c variable (random for now)
    simdata_100k$hba1c365_0 <- sample(x=c("[0,48)","[48,53)","[53,70)","[70,150]"),nrow(simdata_100k),replace=T,prob=c(0.5,0.2,0.2,0.1))
    # need to rename/change censoring nodes
    for(i in 1:10){
      data.table::setnames(simdata_100k,(paste0("censor_",i)),paste0("censor_dem_",i))
      simdata_100k[,paste0("any_second_line_",i):=ifelse(get())]
    }
    return(simdata_100k)
  })

  ## Target Causal parameter 1: static comparison, glp1 only always on vs. never on (adjusted)
  ###########################################################
  #This preps the object to be read by analysis
  ,tar_target(tmle_spec_TCP1, subset_and_specify_analysis(data=wide_df,
                                                          Avars="glp1_",
                                                          Yvars="event_dementia_",
                                                          Cvars="censor_dem_",
                                                          yr=2009,
                                                          baseline_vars=baseline_vars,
                                                          long_covariates=c(long_covariates),#dont have this in sim data!:,"any_second_line_"),
                                                          N_time=11))

)
