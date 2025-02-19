library(data.table)
library(dplyr)
library(mgcv)
library(parallel)
library(rjson)
library(stringr)
library(tidyr)
source("/cbica/projects/luo_wm_dev/two_axes_manuscript/code/fit_GAMs/gam_functions/GAM_functions_tractprofiles.R")

# This script fits developmental nodewise GAMs on tract profiles data using functions from GAM_functions_tractprofiles.R.
# fyi: GAMs are fit for 1 specified scalar (i.e. dti_md)
# Specifically this script does the following:

# 1) run gam.fit.smooth to compute developmental measures + model checks:
# - Age effect: partial Rsq and delta adjusted Rsq
# - Age of maximal developmental change
# - Age of decrease offset: age of maturation if DWI measure decreases with age (e.g. MD)
# - Age of increase offset: age of maturation if DWI measure increases with age (e.g. FA)
# - Age of last change
# - Age of first developmental slowing
# - Model AIC, k-index for checking the model

# 2) run gam.derivatives to compute derivatives for my smooth (age) at each age  

# 3) run gam.estimate.smooth to estimate zero-centered age smooths for each node 
# which will be used to create my twizzler plots :) aka developmental trajectories lol

# 4) run gam.estimate.smooth to estimate age smooths (non-centered) for each node 

################## 
# Set Variables 
################## 
args <- commandArgs(trailingOnly = TRUE) 
dataset = args[1]
scalar = args[2]
print(paste("Fitting developmental GAMs for", dataset))
print(paste("Scalar:", scalar))

################## 
# Set Directories 
################## 
config_data <- fromJSON(file=sprintf("/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_%1$s.json", dataset))
demographics <- read.csv(config_data$demo_qc)
data_root <- config_data$tract_profiles_root
outputs_root <- config_data$outputs_root
GAM_outputs_dir <- paste0(outputs_root, "/GAM/", scalar)

if (dir.exists(GAM_outputs_dir)) {
  print(paste(GAM_outputs_dir, "already exists"))
} else {
  dir.create(GAM_outputs_dir, recursive = TRUE)
  print(paste(GAM_outputs_dir, "created"))
}

################### 
# Define functions 
###################
# 1) run gam.fit.smooth to compute developmental measures + model checks
run_gam.fit.smooth <- function(gam_df, smooth.var, covs, k, set.fx) {
  GAM_dev_measures <- mclapply(tract_node_labels, 
                                    function(x){as.data.frame(gam.fit.smooth(gam.data = gam_df, 
                                                                             tract_node = as.character(x), 
                                                                             smooth_var = smooth.var, 
                                                                             covariates = covs, 
                                                                             knots = k, 
                                                                             set_fx = set.fx))}, mc.cores = 4) 
  GAM_dev_measures <- do.call(rbind, GAM_dev_measures)
  write.csv(GAM_dev_measures, sprintf("%1$s/%2$s_GAM_dev_measures.csv", GAM_outputs_dir, dataset), quote = F, row.names =F)
}

# 2) run gam.derivatives to compute derivatives for my smooth (age) at each age  
run_gam.derivatives <- function(gam_df, smooth.var, covs, k, set.fx, num.draws, num.pred, credible.interval) {
    smooth.derivatives <- mclapply(tract_node_labels, 
                                  function(x){as.data.frame(gam.derivatives(gam.data = gam_df, 
                                                                            tract_node = as.character(x), 
                                                                            smooth_var = smooth.var, 
                                                                            covariates = covs, 
                                                                            knots = k, 
                                                                            set_fx = set.fx, 
                                                                            draws = num.draws, 
                                                                            increments = num.pred, 
                                                                            return_posterior_derivatives = credible.interval)) %>% 
                                      mutate(tract_node = x)}, mc.cores = 4)
    smooth.derivatives <- do.call(rbind, smooth.derivatives)
    write.csv(smooth.derivatives, sprintf("%1$s/%2$s_GAM_derivatives.csv", GAM_outputs_dir, dataset), quote = F, row.names =F)
}

# 3) run gam.estimate.smooth to estimate zero-centered age smooths for each node 
run_gam.estimate.smooth <- function(gam_df, smooth.var, covs, k, set.fx, num.draws, num.pred, start_age, end_age){
  smooth.centered <- mclapply(tract_node_labels, 
                             function(x){as.data.frame(gam.estimate.smooth(gam.data = gam_df, 
                                                                           tract_node = as.character(x), 
                                                                           smooth_var = smooth.var, 
                                                                           covariates = covs, 
                                                                           knots = k, 
                                                                           set_fx = set.fx, 
                                                                           increments = num.pred, 
                                                                           age1 = start_age,
                                                                           age2 = end_age)) %>% mutate(tract_node = x)}, mc.cores = 4)
  smooth.centered <- do.call(rbind, smooth.centered)
  write.csv(smooth.centered, sprintf("%1$s/%2$s_GAM_smoothcentered.csv", GAM_outputs_dir, dataset), quote = F, row.names =F)
}


# 4) run_gam.fitted_values to estimate age smooths (non-centered) for each node
run_gam.smooth.predict <- function(gam_df, smooth.var, covs, k, set.fx, num.pred, start_age, end_age){
  smooth.fittedvals <- mclapply(tract_node_labels, 
                               function(x){as.data.frame(gam.smooth.predict(gam.data = gam_df, 
                                                                            tract_node = as.character(x), 
                                                                            smooth_var = smooth.var, 
                                                                            covariates = covs, 
                                                                            knots = k, 
                                                                            set_fx = set.fx, 
                                                                            increments = num.pred,
                                                                            age1 = start_age,
                                                                            age2 = end_age)) %>% mutate(tract = x)}, mc.cores = 4)
  smooth.fittedvals <- do.call(rbind, smooth.fittedvals)
  write.csv(smooth.fittedvals, sprintf("%1$s/%2$s_GAM_smooth_fittedvalues.csv", GAM_outputs_dir, dataset), quote = F, row.names =F)
}


################### 
# Load files  
###################
if (dataset == "PNC" & !file.exists(sprintf("%1$s/all_subjects/collated_tract_profiles_final.RData", data_root))) {
  print("Formatting PNC collated profiles tsv")
  all_subjects <- fread(sprintf("%1$s/all_subjects/collated_tract_profiles_nocovbat.tsv", data_root))
  all_subjects$tractID <- gsub("Fronto-occipital", "Fronto.occipital", all_subjects$tractID)
  all_subjects <- all_subjects %>% mutate(hemi = ifelse(grepl("Left", tractID), "Left", "Right")) %>% 
    mutate(tract_node = gsub(" ", "_", paste0(tractID, "_", nodeID)))
  all_subjects$sub <- as.factor(all_subjects$sub)
  all_subjects <- all_subjects %>% 
    mutate(nodeID = str_extract(tract_node, "[0-9]+")) %>%
    mutate(tractID = gsub("_[0-9]+", "", tract_node)) %>%
    mutate(hemi = str_extract(tractID, "Left|Right"))
  all_subjects$nodeID <- as.numeric(all_subjects$nodeID)
  all_subjects$sub <- as.factor(all_subjects$sub)
  all_subjects <- all_subjects %>% select(sub, tract_node, nodeID, tractID, hemi, dti_fa, dti_md) %>% arrange(sub, tractID, nodeID, hemi)
  saveRDS(all_subjects, sprintf("%1$s/all_subjects/collated_tract_profiles_final.RData", data_root))
  
  tract_profiles_long <- all_subjects
  
} else {
  print("Loading RData file")
  tract_profiles_long <- readRDS(sprintf("%1$s/all_subjects/collated_tract_profiles_final.RData", data_root))
}

######################################################### 
# Merge demographics and qc with tract profiles data
#########################################################
# first format and make tract profiles data wide
# such that columns = tract_node, rows = subjects
tract_profiles_wide <- tract_profiles_long %>% select(sub, tract_node, all_of(scalar)) %>% pivot_wider(names_from = "tract_node", values_from = !!sym(scalar))

# merge
gam_df <- merge(tract_profiles_wide, demographics, by = "sub")

################### 
# Fit da GAMz 
###################
# set variables for all the gams
tract_node_labels = names(tract_profiles_wide)[-1]
#sanity checks
#tract_node_labels_PNC = names(tract_profiles_wide)[-1]
#tract_node_labels_HCPD = names(tract_profiles_wide)[-1]
#tract_node_labels_HBN = names(tract_profiles_wide)[-1]
#identical(tract_node_labels_PNC, tract_node_labels_HCPD)
#identical(tract_node_labels_PNC, tract_node_labels_HBN)
#identical(tract_node_labels_HCPD, tract_node_labels_HBN)
 
smooth.var = "age"
covs = "sex + mean_fd"
k = 3 
set.fx = TRUE
num.draws = 1
num.pred = 200
credible.interval = FALSE

if(dataset =="PNC") {
  start_age = 8
  end_age = 23
} else if (dataset == "HCPD") {
  start_age = 8
  end_age = 22
} else if (dataset == "HBN") {
  start_age = 5
  end_age = 22
}

# 1) run gam.fit.smooth to compute temporal measures + model checks
print("Running gam.fit.smooth")
x <- run_gam.fit.smooth(gam_df, smooth.var, covs, k, set.fx)

# 2) run gam.derivatives to compute derivatives for my smooth (age) at each age  
print("Running gam.derivatives")
run_gam.derivatives(gam_df, smooth.var, covs, k, set.fx, num.draws, num.pred, credible.interval) 

# 3) run gam.estimate.smooth to estimate zero-centered age smooths for each node 
print("Running gam.estimate.smooth")
run_gam.estimate.smooth(gam_df, smooth.var, covs, k, set.fx, num.draws, num.pred, start_age, end_age)

# 4) run gam.estimate.smooth to estimate age smooths (non-centered) for each node 
print("Running gam.smooth.predict")
run_gam.smooth.predict(gam_df, smooth.var, covs, k, set.fx, num.pred, start_age, end_age)

print("Script finished!")
