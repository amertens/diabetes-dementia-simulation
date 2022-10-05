## setwd("~/research/SoftWare/grfCausalSearch/")
try(setwd(here::here()),silent=TRUE)
# ---------------------------------------------------------------------
# packages
# ---------------------------------------------------------------------
thepackages <- c("targets",
                 "tidyverse",
                 "data.table",
                 "ltmle",
                 "future.batchtools",
                 "tarchetypes",
                 "future.callr",
                 "future",
                 "scales",
                 "foreach",
                 "parallel",
                 "ggplotify",
                 "cowplot")
library(targets)
targets::tar_option_set(packages = thepackages)
# ---------------------------------------------------------------------
# R functions
# ---------------------------------------------------------------------
for(f in list.files("R",".R$",full.names=TRUE)){source(f)}
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}
# ---------------------------------------------------------------------
# Simulation settings
# ---------------------------------------------------------------------
source("./setting/simulation_targets.R")

# ---------------------------------------------------------------------
# The target flow
# ---------------------------------------------------------------------

## MCCORES <- 5
## MCCORES <- 50
## MCCORES are set in setting/simulation-targets.R
list(tar_target(REPETITIONS, 1:1000),
     varying_target,
     fixed_target,
     fixed,
     truth_varying,
     truth,
     estimates,
     ate,
     results,
     ranking,
     plotframe)




