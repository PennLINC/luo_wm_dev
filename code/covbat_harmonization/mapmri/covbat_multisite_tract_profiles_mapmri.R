library(ComBatFamily)
library(data.table)
library(dplyr)
library(mgcv)
library(rjson)
library(stringr)
library(tidyr)

################## 
# Set Variables 
################## 
args <- commandArgs(trailingOnly = TRUE) 
dataset = args[1]
print(paste("Processing", dataset))

################## 
# Set Directories 
################## 
config_data <- fromJSON(file=sprintf("/cbica/projects/luo_wm_dev/two_axes/code/config/config_%1$s.json", dataset))
demographics <- read.csv(sprintf("%1$s/sample_selection_files/final_sample/%2$s_WMDev_FinalSampleDemoQC_mapmri.csv", config_data$manuscript_input_root, config_data$dataset))
data_root <- config_data$tract_profiles_root
output_dir <- paste0(config_data$data_root, "/derivatives/mapmri/all_subjects")


################## 
# Define function 
##################
# @param df, dataframe of covbat harmonized data
# @param scalar, string of name of scalar, e.g. "rtop"
# this function makes the covbat df long and the output should have columns "sub", "tract_node", "nodeID", "tractID", "hemi", and the scalar
format_covbat <- function(df, scalar) {
  df_long <- df %>% pivot_longer(cols = -sub, names_to = "tract_node")
  df_long <- df_long %>% 
    mutate(nodeID = str_extract(tract_node, "[0-9]+")) %>%
    mutate(tractID = gsub("_[0-9]+", "", tract_node)) %>%
    mutate(hemi = str_extract(tractID, "Left|Right"))
  df_long$nodeID <- as.numeric(df_long$nodeID)
  df_long$sub <- as.factor(df_long$sub)
  names(df_long)[which(names(df_long) == "value")] <- paste0(scalar)
  return(df_long)
}

################## 
# Load files 
################## 
all_subjects <- fread(sprintf("%1$s/collated_tract_profiles_nocovbat.tsv", output_dir))

all_subjects$tractID <- gsub("Fronto-occipital", "Fronto.occipital", all_subjects$tractID)
all_subjects <- all_subjects %>% mutate(hemi = ifelse(grepl("Left", tractID), "Left", "Right")) %>% 
  mutate(tract_node = gsub(" ", "_", paste0(tractID, "_", nodeID)))
all_subjects$sub <- as.factor(all_subjects$sub)

# df needs to have sub as rows and tract_node as columns. 
rtap <- all_subjects %>% select(sub, RTAP, tract_node)
rtap_wide <- rtap %>% pivot_wider(names_from = "tract_node", values_from = "RTAP")
rtap_wide <- data.frame(rtap_wide)

rtop <- all_subjects %>% select(sub, RTOP, tract_node)
rtop_wide <- rtop %>% pivot_wider(names_from = "tract_node", values_from = "RTOP")
rtop_wide <- data.frame(rtop_wide)

rtpp <- all_subjects %>% select(sub, RTPP, tract_node)
rtpp_wide <- rtpp %>% pivot_wider(names_from = "tract_node", values_from = "RTPP")
rtpp_wide <- data.frame(rtpp_wide)

# set rownames = sub; remove sub column for proper covbat formatting
row.names(rtap_wide) <- rtap_wide$sub
rtap_to_harmonize <- rtap_wide %>% select(-sub)

row.names(rtop_wide) <- rtop_wide$sub
rtop_to_harmonize <- rtop_wide %>% select(-sub)

row.names(rtpp_wide) <- rtpp_wide$sub
rtpp_to_harmonize <- rtpp_wide %>% select(-sub)

# reorder demographics row to match the wide diffusion metric df's 
demographics <- left_join(rtpp_wide[,c(1,2)], demographics, by="sub")
demographics <- demographics %>% select(sub, age, sex, race, site, mean_fd)

# set covariate vectors
age_vec <- demographics$age 
sex_vec <- as.factor(demographics$sex) 
mean_fd_vec <- demographics$mean_fd 
covar_df <- bind_cols(demographics$sub, as.numeric(age_vec), as.factor(sex_vec), as.numeric(mean_fd_vec))
covar_df <- dplyr::rename(covar_df, sub=...1,
                          age = ...2,
                          sex = ...3,
                          mean_fd = ...4)

# Assuming your dataframe is named df
na_indices <- which(is.na(rtpp_to_harmonize), arr.ind = TRUE)

# Convert to a data frame for easier viewing (optional)
na_indices_df <- as.data.frame(na_indices)

################## 
# Harmonize data 
################## 
data.harmonized_rtap <- covfam(rtap_to_harmonize, bat = as.factor(demographics$site), covar = covar_df, model = gam, formula = y ~ s(age, k=3, fx=T) + as.factor(sex) + as.numeric(mean_fd))
print("RTAP harmonized")

data.harmonized_rtop <- covfam(rtop_to_harmonize, bat = as.factor(demographics$site), covar = covar_df, model = gam, formula = y ~ s(age, k=3, fx=T) + as.factor(sex) + as.numeric(mean_fd))
print("RTOP harmonized")

data.harmonized_rtpp <- covfam(rtpp_to_harmonize, bat = as.factor(demographics$site), covar = covar_df, model = gam, formula = y ~ s(age, k=3, fx=T) + as.factor(sex) + as.numeric(mean_fd))
print("RTPP harmonized")

# clean up covbat output for saving to RData
rtap_covbat <- data.frame(data.harmonized_rtap$dat.covbat)
rtap_covbat$sub <- rownames(rtap_covbat)
rtap_covbat <- rtap_covbat %>% relocate(sub)
rownames(rtap_covbat) <- NULL

rtop_covbat <- data.frame(data.harmonized_rtop$dat.covbat)
rtop_covbat$sub <- rownames(rtop_covbat)
rtop_covbat <- rtop_covbat %>% relocate(sub)
rownames(rtop_covbat) <- NULL

rtpp_covbat <- data.frame(data.harmonized_rtpp$dat.covbat)
rtpp_covbat$sub <- rownames(rtpp_covbat)
rtpp_covbat <- rtpp_covbat %>% relocate(sub)
rownames(rtpp_covbat) <- NULL

# final formatting...
final_rtap_covbat <- format_covbat(rtap_covbat, "RTAP")
final_rtop_covbat <- format_covbat(rtop_covbat, "RTOP")
final_rtpp_covbat <- format_covbat(rtpp_covbat, "RTPP")
 
# merge all the covbat harmonized data together
merged_covbat_all <- merge(final_rtap_covbat, final_rtop_covbat)
merged_covbat_all <- merge(merged_covbat_all, final_rtpp_covbat)
merged_covbat_all <- merged_covbat_all %>% arrange(sub, tractID, nodeID, hemi) 

# save out!
saveRDS(merged_covbat_all, sprintf("%1$s/collated_tract_profiles_final.RData", output_dir))
