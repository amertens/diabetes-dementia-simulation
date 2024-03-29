



rm(list=ls())
source(paste0(here::here(),"/0_config.R"))
source(paste0(here::here(),"/functions/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/functions/0_simulation_functions.R"))
source(paste0(here::here(),"/functions/0_simulation_cleaning_functions.R"))
cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

coefficients = cc
A_name = "glp1"


synthesizeDD <- function(coefficients, A_name = "glp1", A=NULL){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)

  if(!is.null(A)){
    if(A==1){ coefficients$`(Intercept)`[grepl(A_name, coefficients$var)] <- 99999999999}
    if(A==0){ coefficients$`(Intercept)`[grepl(A_name, coefficients$var)] <- -99999999999}
  }


  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  distribution(m,"sex") <- binomial.lvm(p=0.4)
  m <- addvar(m,"ie_type")
  m <- addvar(m,"code5txt")
  m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- INTERCEPT[j]
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}





 seed <- 3457347
 nsamp=100000


   set.seed(seed)
   u <- synthesizeDD(cc)
   d <- sim(u, nsamp)

  set.seed(seed)
  u.always <- synthesizeDD(cc, A=1)
  d.always  <- sim(u.always, nsamp)

  set.seed(seed)
  u.never <- synthesizeDD(cc, A=0)
  d.never <- sim(u.never, nsamp)

  #This simulated data allows for dementia to develop after death
  table(d$event_dementia_10)
  table(d.always$event_dementia_10)
  table(d.never$event_dementia_10)

  #Set dementia after death to NA
  clean_sim_data <- function(d, N_time=10){

    d<- data.table(d)

    for(i in 1:(N_time+1)){
      j=i+1
      d[is.na(get(paste0("event_dementia_",i))), (paste0("event_dementia_",j)):=NA]
      d[get(paste0("event_dementia_",i))==1, (paste0("event_dementia_",j)):=1]
      d[get(paste0("event_death_",i))==1, (paste0("event_death_",j)):=1]
      d[get(paste0("event_death_",i))==1, (paste0("event_dementia_",j)):=NA]
    }
    return(d)
  }

  d <- clean_sim_data(d)
  d.always <- clean_sim_data(d.always)
  d.never <- clean_sim_data(d.never)

  table(d$event_dementia_10)
  table(d.always$event_dementia_10)
  table(d.never$event_dementia_10)


  tRR10 <- mean(d.always$event_dementia_10,na.rm=T)/mean(d.never$event_dementia_10,na.rm=T)
  tRD10 <- mean(d.always$event_dementia_10,na.rm=T) - mean(d.never$event_dementia_10,na.rm=T)
  tRR10
  tRD10


  #observed RR and RD


  d.always <- clean_sim_data(d.always.full, 10)
  d.never <- clean_sim_data(d.never.full, 10)


  tRR10_2 <- mean(d.always$event_dementia_10,na.rm=T)/mean(d.never$event_dementia_10,na.rm=T)
  tRD10_2 <- mean(d.always$event_dementia_10,na.rm=T) - mean(d.never$event_dementia_10,na.rm=T)
  tRR10_2
  tRD10_2
