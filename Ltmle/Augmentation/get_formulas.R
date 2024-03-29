## Get g-formulas and Q-formulas

get_formulas <- function(time_horizon,
                         work_data,
                         name_outcome,
                         name_baseline_covariates,
                         name_time_covariates,
                         name_regimen,
                         name_censoring = NULL,
                         name_comp.event = NULL,
                         Markov = NULL, ## Names of time varying covariates with Markov property. Note that regimen is assumed NOT to be Markov
                         constant_variables = NULL){

    if (length(Markov)>0 && Markov[[1]]!="")
        if (any(not_found <- !(Markov%in%name_time_covariates)))
            stop(paste0("The following variables in argument Markov do not match time_covariates:\n",
                        paste(Markov[not_found],collapse=", ")))
  
    time_grid = 0:time_horizon
    K = length(time_grid)
    gform = if(length(name_regimen)==2){
                c(paste0(name_regimen[[1]],"_0"," ~ ", get_rhs(timepoint = 0, name_baseline_covariates = name_baseline_covariates,
                                                               name_time_covariates = name_time_covariates, name_regimen = name_regimen,
                                                               regimen = FALSE, Markov = Markov, constant_variables = constant_variables)),
                  paste0(name_regimen[[2]],"_0"," ~ ", get_rhs(timepoint = 0, name_baseline_covariates = name_baseline_covariates,
                                                               name_time_covariates = name_time_covariates, name_regimen = name_regimen,
                                                               regimen = FALSE, Markov = Markov, constant_variables = constant_variables)),
                  if(length(name_censoring)>0){paste0(name_censoring,"_1"," ~ ", get_rhs(timepoint = 0, name_baseline_covariates = name_baseline_covariates,
                                                                                         name_time_covariates = name_time_covariates, name_regimen = name_regimen,
                                                                                         regimen = TRUE, Markov = Markov,
                                                                                         constant_variables = constant_variables))} else{})
            } else{
                c(paste0(name_regimen,"_0"," ~ ", get_rhs(timepoint = 0, name_baseline_covariates = name_baseline_covariates,
                                                          name_time_covariates = name_time_covariates, name_regimen = name_regimen,
                                                          regimen = FALSE, Markov = Markov, constant_variables = constant_variables)),
                  if(length(name_censoring)>0){paste0(name_censoring,"_1"," ~ ", get_rhs(timepoint = 0, name_baseline_covariates = name_baseline_covariates,
                                                                                         name_time_covariates = name_time_covariates, name_regimen = name_regimen,
                                                                                         regimen = TRUE, Markov = Markov, constant_variables = constant_variables))} else{})
            }
    if(time_horizon>1){
        gform = c(gform, unlist(lapply(1:(time_horizon-1),function(tk){
            if(length(name_regimen)==2){
                c(paste0(name_regimen[[1]],"_",tk," ~ ", get_rhs(timepoint = tk, name_baseline_covariates = name_baseline_covariates,
                                                                 name_time_covariates  = name_time_covariates, name_regimen = name_regimen, regimen = TRUE,
                                                                 Markov = Markov, constant_variables = constant_variables)),
                  paste0(name_regimen[[2]],"_",tk," ~ ", get_rhs(timepoint = tk, name_baseline_covariates = name_baseline_covariates,
                                                                 name_time_covariates  = name_time_covariates, name_regimen = name_regimen, regimen = TRUE,
                                                                 Markov = Markov, constant_variables = constant_variables)),
                  if(length(name_censoring)>0) {paste0(name_censoring,"_", tk+1, " ~ ",
                                                       get_rhs(timepoint = tk, name_baseline_covariates = name_baseline_covariates,
                                                               name_time_covariates = name_time_covariates,
                                                               name_regimen = name_regimen, regimen = TRUE,
                                                               Markov = Markov, constant_variables = constant_variables))}
                  else {})
            } else{
                c(paste0(name_regimen,"_",tk," ~ ", get_rhs(timepoint = tk, name_baseline_covariates = name_baseline_covariates,
                                                            name_time_covariates  = name_time_covariates, name_regimen = name_regimen, regimen = TRUE,
                                                            Markov = Markov, constant_variables = constant_variables)),
                  if(length(name_censoring)>0) {paste0(name_censoring,"_", tk+1, " ~ ",
                                                       get_rhs(timepoint = tk+1, name_baseline_covariates = name_baseline_covariates,
                                                               name_time_covariates = name_time_covariates,
                                                               name_regimen = name_regimen, regimen = TRUE,
                                                               Markov = Markov, constant_variables = constant_variables))}
                  else {})
            }
        })))
    }
  
  
  ## Note that A_k ~ V + L_0 + ... + L_(k-1) + A_(k-1) for k = 1,..., time_horizon, but A_0 ~ V + L_0
  ## i.e., regimen at baseline depends on additional baseline covariates, whereas in general, regimen depends
  ## on the previously observed covariates and regimen.
  ## The reason for this is we do not want to mistakenly assume that L_1 -> A_1 when in reality A_1 happens before L_1
  
  Qform <- unlist(lapply(1:time_horizon,function(tk){
    paste0("Q.kplus1 ~ ", get_rhs(timepoint = tk, name_baseline_covariates = name_baseline_covariates,
                                  name_time_covariates  = name_time_covariates, name_regimen = name_regimen, regimen = TRUE,
                                  Markov = Markov, constant_variables = constant_variables))
  }))
  
  names(Qform)=paste0(name_outcome,"_",1:time_horizon)
  list(gform = gform, Qform = Qform)
}  
