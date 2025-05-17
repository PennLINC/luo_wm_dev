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
demographics <- read.csv(sprintf("%1$s/sample_selection_files/final_sample/%2$s_WMDev_FinalSampleDemoQC_noddi.csv", config_data$manuscript_input_root, config_data$dataset))
data_root <- config_data$tract_profiles_root
output_dir <- paste0(config_data$data_root, "/derivatives/noddi/all_subjects")


################## 
# Define function 
##################
# @param df, dataframe of covbat harmonized data
# @param scalar, string of name of scalar, e.g. "isovf"
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
icvf <- all_subjects %>% select(sub, icvf, tract_node)
icvf_wide <- icvf %>% pivot_wider(names_from = "tract_node", values_from = "icvf")
icvf_wide <- data.frame(icvf_wide)

isovf <- all_subjects %>% select(sub, isovf, tract_node)
isovf_wide <- isovf %>% pivot_wider(names_from = "tract_node", values_from = "isovf")
isovf_wide <- data.frame(isovf_wide)

od <- all_subjects %>% select(sub, od, tract_node)
od_wide <- od %>% pivot_wider(names_from = "tract_node", values_from = "od")
od_wide <- data.frame(od_wide)

# set rownames = sub; remove sub column for proper covbat formatting
row.names(icvf_wide) <- icvf_wide$sub
icvf_to_harmonize <- icvf_wide %>% select(-sub)

row.names(isovf_wide) <- isovf_wide$sub
isovf_to_harmonize <- isovf_wide %>% select(-sub)

row.names(od_wide) <- od_wide$sub
od_to_harmonize <- od_wide %>% select(-sub)

# reorder demographics row to match the wide diffusion metric df's 
demographics <- left_join(od_wide[,c(1,2)], demographics, by="sub")
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
na_indices <- which(is.na(od_to_harmonize), arr.ind = TRUE)

# Convert to a data frame for easier viewing (optional)
na_indices_df <- as.data.frame(na_indices)

################## 
# Harmonize data 
################## 
data.harmonized_icvf <- covfam(icvf_to_harmonize, bat = as.factor(demographics$site), covar = covar_df, model = gam, formula = y ~ s(age, k=3, fx=T) + as.factor(sex) + as.numeric(mean_fd))
print("ICVF harmonized")

data.harmonized_isovf <- covfam(isovf_to_harmonize, bat = as.factor(demographics$site), covar = covar_df, model = gam, formula = y ~ s(age, k=3, fx=T) + as.factor(sex) + as.numeric(mean_fd))
print("ISOVF harmonized")

data.harmonized_od <- covfam(od_to_harmonize, bat = as.factor(demographics$site), covar = covar_df, model = gam, formula = y ~ s(age, k=3, fx=T) + as.factor(sex) + as.numeric(mean_fd))
print("OD harmonized")

# clean up covbat output for saving to RData
icvf_covbat <- data.frame(data.harmonized_icvf$dat.covbat)
icvf_covbat$sub <- rownames(icvf_covbat)
icvf_covbat <- icvf_covbat %>% relocate(sub)
rownames(icvf_covbat) <- NULL

isovf_covbat <- data.frame(data.harmonized_isovf$dat.covbat)
isovf_covbat$sub <- rownames(isovf_covbat)
isovf_covbat <- isovf_covbat %>% relocate(sub)
rownames(isovf_covbat) <- NULL

od_covbat <- data.frame(data.harmonized_od$dat.covbat)
od_covbat$sub <- rownames(od_covbat)
od_covbat <- od_covbat %>% relocate(sub)
rownames(od_covbat) <- NULL

# final formatting...
final_icvf_covbat <- format_covbat(icvf_covbat, "icvf")
final_isovf_covbat <- format_covbat(isovf_covbat, "isovf")
final_od_covbat <- format_covbat(od_covbat, "od")
 
# merge all the covbat harmonized data together
merged_covbat_all <- merge(final_icvf_covbat, final_isovf_covbat)
merged_covbat_all <- merge(merged_covbat_all, final_od_covbat)
merged_covbat_all <- merged_covbat_all %>% arrange(sub, tractID, nodeID, hemi) 

# save out!
saveRDS(merged_covbat_all, sprintf("%1$s/collated_tract_profiles_final.RData", output_dir))
