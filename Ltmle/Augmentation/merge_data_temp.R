merge_data <- function(time_horizon,
                       regimen_data,
                       outcome_data, 
                       baseline_data,
                       timevar_data, 
                       name_outcome,                    
                       name_regimen,
                       name_censoring = NULL,
                       censored_label = "censored",
                       name_comp.event = NULL,
                       test=FALSE){
  
  #browser()
  
  time_grid = 0:time_horizon
  K = length(time_grid)
  # the regimen may have two components (A and B) both are
  # identified based on the first (A)
  # here we select the specified element of a list of regimens
  setkey(regimen_data,pnr)
  #name_outcome=c("pnr",name_outcome)
  name_outcomes_df <- c("pnr",grep(paste0(name_outcome,"_"),names(timevar_data)))
  outcome_data <- outcome_data[,..name_outcomes_df]
  setkey(outcome_data,pnr)
  wide_data=outcome_data[regimen_data, on = c("pnr")]
  #
  # deal with outcome/death/censored at index
  #
  # D_0 = match(paste0(name_comp.event,"_",0),names(wide_data))
  # C_0 = match(paste0(name_censoring,"_",0),names(wide_data))
  # wide_data = wide_data[!(wide_data[[D_0]]%in%1)&!(wide_data[[C_0]]%in%censored_label)]
  #
  # adding the baseline covariates
  #
  #browser()
  wide_data=baseline_data[wide_data, on = c("pnr")]
  # subset and sort data
  if (test)
    work_data <- wide_data[agegroups%in%c("60-65","65-70")]
  else
    work_data <- wide_data
  # add time covariates
  # first remove outcome if overlap
  if (length((outcome_overlap <- grep(paste0(name_outcome,"_"),names(timevar_data)))>0))
    timevar_data <- timevar_data[,-outcome_overlap, with=FALSE]
  setkey(timevar_data,pnr)
  work_data=timevar_data[work_data, on = c("pnr")]
  
  name_time_covariates = unlist(lapply(grep("_0",names(timevar_data),value=TRUE),function(x){substring(x,0,nchar(x)-2)}))
  name_baseline_covariates = setdiff(names(baseline_data),"pnr")

  # sorting the variables for LTMLE
  work_data = work_data[,c("pnr", name_baseline_covariates,unlist(sapply(time_grid, function(timepoint){
      if(timepoint == 0){
          paste0(c(name_time_covariates, name_regimen),"_",timepoint)
      } else{
          if(timepoint != time_grid[K]){
              paste0(c(name_censoring, name_outcome, name_time_covariates, name_comp.event, name_regimen),"_",timepoint)
          } else {
              paste0(c(name_censoring, name_outcome),"_",timepoint)
          }
      }
  }))), with = FALSE]
  
  list(data = work_data[],
       name_baseline_covariates = name_baseline_covariates,
       name_time_covariates = name_time_covariates,
       name_regimen = name_regimen)
}

