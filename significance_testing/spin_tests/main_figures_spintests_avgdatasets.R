library(dplyr)
library(parallel)
library(tidyr)
source("/cbica/projects/luo_wm_dev/two_axes_manuscript/code/results/main_figures_functions.R")

# Spin tests for Figures 5 and 6
## saves out the spun p-values, empirical test statistic, and dof where appropriate in a csv for each dataset

################## 
# Set Variables 
################## 
print(paste("Running spin tests for average across datasets"))

################# 
# set directories
################# 
proj_root <- "/cbica/projects/luo_wm_dev/"
input_root <- paste0(proj_root, "input")
output_root <- paste0(proj_root, "output")

output_dir <- paste0(output_root, "/avgdatasets/tract_profiles/fig5_fig6_spintests")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = T)
  message("Folder created: ", output_dir)
} else {
  message("Folder already exists: ", output_dir)
}

scalar = "dti_md"
PNC_ageeffects <- read.csv(paste0(output_root, "/", "PNC", "/tract_profiles/GAM/", scalar, "/PNC_GAM_dev_measures.csv"))
HCPD_ageeffects <- read.csv(paste0(output_root, "/", "HCPD", "/tract_profiles/GAM/", scalar, "/HCPD_GAM_dev_measures.csv"))
HBN_ageeffects <- read.csv(paste0(output_root, "/", "HBN", "/tract_profiles/GAM/", scalar, "/HBN_GAM_dev_measures.csv"))

# format age effect df's (the functions were originally formatted for all 3 datasets, hence the lapply's)
# make a list of the different df names: [dataset]_GAM_ageeffects_[scalar]
datasets <- c("PNC", "HCPD", "HBN")
ageeffect_df_names <- c()
for (dataset in datasets) {
  ageeffect_df_names <- append(ageeffect_df_names, paste0(dataset, "_ageeffects"))
}
# format age effect df's
ageeffect_dfs <- lapply(ageeffect_df_names, format_ageeffect)
ageeffect.fdr_dfs <- lapply(ageeffect_dfs, sig_nodes)
names(ageeffect.fdr_dfs) <- ageeffect_df_names 
tracts <- setdiff(unique(ageeffect.fdr_dfs$HCPD_ageeffects$tract_label), "Corticospinal") # CST for supplement

# load glasser labels
glasser_labels <- read.csv("/cbica/projects/luo_wm_dev/atlases/glasser/HCP-MMP1_UniqueRegionList.csv")
glasser_labels$regionID <- c(1:360)
glasser_labels$region <- gsub("7Pl", "7PL", glasser_labels$region)   

# load maps for depth = 1.5mm. (variables a, b, and c don't matter - they're just to prevent annoying lapply outputs from getting printed)
a <- lapply("1.5", load_maps, "HCPD") # makes lh_maps_HCPD, rh_maps_HCPD 
b <- lapply("1.5", load_maps, "HBN") # makes lh_maps_HBN, rh_maps_HBN
c <- lapply("1.5", load_maps, "PNC") # makes lh_maps_PNC, rh_maps_PNC

# load S-A axis
glasser_SAaxis <- read.csv("/cbica/projects/luo_wm_dev/SAaxis/glasser_SAaxis.csv")
glasser_SAaxis <- glasser_SAaxis %>% select(SA.axis_rank, label)
glasser_SAaxis$regionName <- gsub("_ROI", "", glasser_SAaxis$label)
glasser_SAaxis$regionName <- gsub("^(.)_(.*)$", "\\2_\\1", glasser_SAaxis$region)

# make cortical endpoint maps for age of maturation
threshold=0.3
PNC_deveffects_5_agemat <- ageeffect.fdr_dfs$PNC_ageeffects %>% filter((nodeID < 10 & nodeID > 4) | (nodeID < 95 & nodeID > 89)) %>% mutate(node_position = case_when((nodeID < 10 & nodeID > 4) ~ "end1", (nodeID < 95 & nodeID > 89) ~ "end2")) %>%
  select(tract_label, tractID, nodeID, node_position, hemi, smooth.peak.change, smooth.decrease.offset, smooth.last.change, smooth.slowing.onset) %>%
  group_by(tractID, node_position, hemi) %>% summarise(mean_peak_change = mean(smooth.peak.change, na.rm = T),
                                                       mean_ageeffect = mean(smooth.decrease.offset, na.rm = T),
                                                       mean_last_change = mean(smooth.last.change, na.rm = T),
                                                       mean_dev_slowing = mean(smooth.slowing.onset, na.rm = T))

HCPD_deveffects_5_agemat <- ageeffect.fdr_dfs$HCPD_ageeffects %>% filter((nodeID < 10 & nodeID > 4) | (nodeID < 95 & nodeID > 89)) %>% mutate(node_position = case_when((nodeID < 11 & nodeID > 4) ~ "end1", (nodeID < 95 & nodeID > 89) ~ "end2")) %>%
  select(tract_label, tractID, nodeID, node_position, hemi, smooth.peak.change, smooth.decrease.offset, smooth.last.change, smooth.slowing.onset) %>%
  group_by(tractID, node_position, hemi) %>% summarise(mean_peak_change = mean(smooth.peak.change, na.rm = T),
                                                       mean_ageeffect = mean(smooth.decrease.offset, na.rm = T), # mean_ageeffect = mean_age_mat
                                                       mean_last_change = mean(smooth.last.change, na.rm = T),
                                                       mean_dev_slowing = mean(smooth.slowing.onset, na.rm = T))

HBN_deveffects_5_agemat <- ageeffect.fdr_dfs$HBN_ageeffects %>% filter((nodeID < 10 & nodeID > 4) | (nodeID < 95 & nodeID > 89)) %>% mutate(node_position = case_when((nodeID < 10 & nodeID > 4) ~ "end1", (nodeID < 95 & nodeID > 89) ~ "end2")) %>%
  select(tract_label, tractID, nodeID, node_position, hemi, smooth.peak.change, smooth.decrease.offset, smooth.last.change, smooth.slowing.onset) %>%
  group_by(tractID, node_position, hemi) %>% summarise(mean_peak_change = mean(smooth.peak.change, na.rm = T),
                                                       mean_ageeffect = mean(smooth.decrease.offset, na.rm = T),
                                                       mean_last_change = mean(smooth.last.change, na.rm = T),
                                                       mean_dev_slowing = mean(smooth.slowing.onset, na.rm = T))

make_maps("PNC", "5_agemat") # makes [bundle_name]_deveffect_[dataset] for each dataset and bundle. e.g. IFOL_deveffect_PNC
make_maps("HCPD", "5_agemat") 
make_maps("HBN", "5_agemat")

# aggregate age maps
region_all_PNC <- aggregate_age_maps("PNC")  
lh_by_region_PNC <- region_all_PNC[[1]]
rh_by_region_PNC <- region_all_PNC[[2]]

region_all_HCPD <- aggregate_age_maps("HCPD")  
lh_by_region_HCPD <- region_all_HCPD[[1]]
rh_by_region_HCPD <- region_all_HCPD[[2]]

region_all_HBN <- aggregate_age_maps("HBN")  
lh_by_region_HBN <- region_all_HBN[[1]]
rh_by_region_HBN <- region_all_HBN[[2]]

# compute SA rank for endpoints
all_endpoints_PNC <- compute_mean_SA("PNC")  
lh_all_endpoints_PNC <- all_endpoints_PNC[[1]]
rh_all_endpoints_PNC <- all_endpoints_PNC[[2]]
all_endpoints_PNC <- all_endpoints_PNC[[3]]

all_endpoints_HCPD <- compute_mean_SA("HCPD")  
lh_all_endpoints_HCPD <- all_endpoints_HCPD[[1]]
rh_all_endpoints_HCPD <- all_endpoints_HCPD[[2]]
all_endpoints_HCPD <- all_endpoints_HCPD[[3]]

all_endpoints_HBN <- compute_mean_SA("HBN")  
lh_all_endpoints_HBN <- all_endpoints_HBN[[1]]
rh_all_endpoints_HBN <- all_endpoints_HBN[[2]]
all_endpoints_HBN <- all_endpoints_HBN[[3]]

###############################################################
# Fig. 5: 
# For individual datasets: compute mean difference in S-A axis 
# rank between endpoints vs. Difference in age of maturation 
# of endpoints (delta-delta plot)
###############################################################
# compute mean difference in S-A rank between the 2 cortical endpoints by the difference in age effect
lh_names <- c("ARCL", "ILFL", "IFOL", "SLFL", "pARCL", "UNCL", "VOFL", "COrbL", "CAntFrL" ,"CSupFrL", "CMotL", "CSupParL", "CPostParL", "CTempL", "COccL")
rh_names <- c("ARCR", "ILFR", "IFOR", "SLFR", "pARCR", "UNCR", "VOFR", "COrbR", "CAntFrR" ,"CSupFrR", "CMotR", "CSupParR", "CPostParR", "CTempR", "COccR")

diffs_PNC <- compute_diffs_wrapper(lh_all_endpoints_PNC, rh_all_endpoints_PNC)  
diffs_PNC <- diffs_PNC %>% mutate(group = ifelse(str_detect(bundle_name, "IFO") | str_detect(bundle_name, "ILF"), 
                                                 "Large Difference\n in S-A Rank", 
                                                 "Small Difference\n in S-A Rank"))
diffs_PNC$group <- factor(diffs_PNC$group, levels = c("Small Difference\n in S-A Rank", "Large Difference\n in S-A Rank"))

diffs_HCPD <- compute_diffs_wrapper(lh_all_endpoints_HCPD, rh_all_endpoints_HCPD)  
diffs_HCPD <- diffs_HCPD %>% mutate(group = ifelse(str_detect(bundle_name, "IFO") | str_detect(bundle_name, "ILF"), 
                                                   "Large Difference\n in S-A Rank", 
                                                   "Small Difference\n in S-A Rank"))
diffs_HCPD$group <- factor(diffs_HCPD$group, levels = c("Small Difference\n in S-A Rank", "Large Difference\n in S-A Rank"))

diffs_HBN <- compute_diffs_wrapper(lh_all_endpoints_HBN, rh_all_endpoints_HBN)   
diffs_HBN <- diffs_HBN %>% mutate(group = ifelse(str_detect(bundle_name, "IFO") | str_detect(bundle_name, "ILF"), 
                                                 "Large Difference\n in S-A Rank", 
                                                 "Small Difference\n in S-A Rank"))
diffs_HBN$group <- factor(diffs_HBN$group, levels = c("Small Difference\n in S-A Rank", "Large Difference\n in S-A Rank"))

lh_combined_df <- bind_rows(lh_all_endpoints_PNC %>% mutate(dataset = "PNC"),
                            lh_all_endpoints_HCPD %>% mutate(dataset = "HCPD"),
                            lh_all_endpoints_HBN %>% mutate(dataset = "HBN"))

rh_combined_df <- bind_rows(rh_all_endpoints_PNC %>% mutate(dataset = "PNC"),
                            rh_all_endpoints_HCPD %>% mutate(dataset = "HCPD"),
                            rh_all_endpoints_HBN %>% mutate(dataset = "HBN"))

# Calculate the averages of mean_SA and age_effect across all dataframes
lh_averaged_df <- lh_combined_df %>%
  group_by(bundle_name, end)  %>% summarize(mean_SA = mean(mean_SA, na.rm = TRUE),
                                            age_effect = mean(age_effect, na.rm = TRUE),
                                            .groups = "drop")
rh_averaged_df <- rh_combined_df %>%
  group_by(bundle_name, end)  %>% summarize(mean_SA = mean(mean_SA, na.rm = TRUE),
                                            age_effect = mean(age_effect, na.rm = TRUE),
                                            .groups = "drop")

# compute diffs
diffs_avg_datasets <- compute_diffs_wrapper(lh_averaged_df, rh_averaged_df)   
diffs_avg_datasets <- diffs_avg_datasets %>% mutate(group = ifelse(str_detect(bundle_name, "IFO") | str_detect(bundle_name, "ILF"), 
                                                                   "Large Difference\n in S-A Rank", 
                                                                   "Small Difference\n in S-A Rank"))
diffs_avg_datasets$group <- factor(diffs_avg_datasets$group, levels = c("Small Difference\n in S-A Rank", "Large Difference\n in S-A Rank"))

print(paste("Delta-delta spin test running for average across datasets"))
across_datasets_delta_p <- perm.sphere.SAaxis_delta(glasser_SAaxis$SA.axis_rank, perm.id.full, "avg_datasets")  
across_datasets_delta_p <- as.data.frame(across_datasets_delta_p)
write.csv(across_datasets_delta_p, paste0(output_dir, "/fig5_pspin.csv"), row.names=F)

########################################################################  
# Fig 6: Pearson's (previously Spearman's) correlation for fully matured endpoints vs. S-A rank
########################################################################  
# binarize age of maturation df's to get matured and not-yet-matured regions
# make an average binarized df (average across datasets first then binarize)
lh_PNC_merge <- lh_by_region_PNC %>% rename(PNC_mean_ageeffect = regional_mean_ageeffect)
lh_HCPD_merge <- lh_by_region_HCPD %>% rename(HCPD_mean_ageeffect = regional_mean_ageeffect)
lh_HBN_merge <- lh_by_region_HBN %>% rename(HBN_mean_ageeffect = regional_mean_ageeffect)

merge_temp <- merge(lh_HCPD_merge, lh_HBN_merge, by = "region")
lh_all_datasets_ageMat <- merge(merge_temp, lh_PNC_merge, by = "region")
lh_all_datasets_ageMat <- lh_all_datasets_ageMat %>% # left hemi age of maturation maps
  mutate(regional_mean_ageeffect = rowMeans(select(., HCPD_mean_ageeffect, HBN_mean_ageeffect, PNC_mean_ageeffect), na.rm = TRUE))

rh_PNC_merge <- rh_by_region_PNC %>% rename(PNC_mean_ageeffect = regional_mean_ageeffect)
rh_HCPD_merge <- rh_by_region_HCPD %>% rename(HCPD_mean_ageeffect = regional_mean_ageeffect)
rh_HBN_merge <- rh_by_region_HBN %>% rename(HBN_mean_ageeffect = regional_mean_ageeffect)

merge_temp <- merge(rh_HCPD_merge, rh_HBN_merge, by = "region")
rh_all_datasets_ageMat <- merge(merge_temp, rh_PNC_merge, by = "region")
rh_all_datasets_ageMat <- rh_all_datasets_ageMat %>% # right hemi age of maturation maps
  mutate(regional_mean_ageeffect = rowMeans(select(., HCPD_mean_ageeffect, HBN_mean_ageeffect, PNC_mean_ageeffect), na.rm = TRUE))

lh_all_datasets_ageMat$region <- paste0(lh_all_datasets_ageMat$region, "_L")
rh_all_datasets_ageMat$region <- paste0(rh_all_datasets_ageMat$region, "_R")
combined_df <- rbind(lh_all_datasets_ageMat, rh_all_datasets_ageMat) %>% rename(regionName = region)

binarized_combined_df <- right_join(combined_df, glasser_SAaxis, by = "regionName") # binarize
max_value <- max(binarized_combined_df$regional_mean_ageeffect, na.rm = TRUE)
binarized_combined_df$regional_mean_ageeffect[binarized_combined_df$regional_mean_ageeffect == max_value] <- NA

# spin Pearsons (previously Spearman's)
print("Age of maturation vs. SA rank spin test running for across-dataset average") # final regional_mean_ageeffect = mean across datasets
combined_parcelSA_p <- perm.sphere.p(x = binarized_combined_df$SA.axis_rank, y = binarized_combined_df$regional_mean_ageeffect, perm.id = perm.id.full, corr.type = "pearson") 

# save out empirical rho's and spun p-values
combined_rho.emp <- cor.test(binarized_combined_df$SA.axis_rank, binarized_combined_df$regional_mean_ageeffect, method = "pearson", use = "complete.obs")$estimate # r = 0.53, pspin < 0.0001
combined_ageMat_SA_p_df <- data.frame(list(p.perm = combined_parcelSA_p, rho.emp = combined_rho.emp), row.names = NULL)
write.csv(combined_ageMat_SA_p_df, paste0(output_dir, "/fig6_pearsons_pspin_avgdatasets.csv"), row.names=F)

# spin t-test 
aggregated_axis_avgdatasets_binary <- binarized_combined_df

parcelSA_p_ttest <- perm.sphere.p.ttest(SAaxis = glasser_SAaxis$SA.axis_rank, perm.id = perm.id.full, dataset = "avgdatasets", alternative = "greater", var.equal = FALSE)  
parcelSA_p_ttest <- as.data.frame(parcelSA_p_ttest)
write.csv(parcelSA_p_ttest, paste0(output_dir, "/fig6_ttest_pspin_avgdatasets.csv"), row.names=F)

print("Script finished!")
