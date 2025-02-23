library(dplyr)
library(parallel)
library(tidyr)
source("/cbica/projects/luo_wm_dev/two_axes_manuscript/code/results/main_figures_functions.R")
source("/cbica/projects/luo_wm_dev/two_axes_manuscript/code/results/supp_figures_functions.R")

# Spin tests for supplementary figures: tract-level Pearson's (age of maturation vs. S-A rank), and parcel-level Pearson's
## saves out the spun p-values, empirical test statistic, and dof where appropriate in a csv for each dataset

################## 
# Set Variables 
################## 
print(paste("Running spin tests for average across datasets"))

################# 
# set directories
################# 
data_root <- "/cbica/projects/luo_wm_dev/"
proj_root <- "/cbica/projects/luo_wm_dev/two_axes_manuscript/"
input_root <- paste0(proj_root, "input")
output_root <- paste0(proj_root, "output")

output_dir <- paste0(output_root, "/avgdatasets/suppfigs_spintests/")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Folder created: ", output_dir)
} else {
  message("Folder already exists: ", output_dir)
}

scalar = "dti_md"
PNC_ageeffects <- read.csv(paste0(output_root, "/", "PNC", "/GAM/", scalar, "/PNC_GAM_dev_measures.csv"))
HCPD_ageeffects <- read.csv(paste0(output_root, "/", "HCPD", "/GAM/", scalar, "/HCPD_GAM_dev_measures.csv"))
HBN_ageeffects <- read.csv(paste0(output_root, "/", "HBN", "/GAM/", scalar, "/HBN_GAM_dev_measures.csv"))

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

 
# not doing tract-level: age of maturation vs. mean S-A rank for all datasets
# would involve doing across-dataset average for each cortical endpoint map (make_maps)

###############################################################
# Supplementary Fig. 
# tract-level: age of maturation vs. mean S-A rank
###############################################################

combined_df <- bind_rows(all_endpoints_PNC %>% mutate(dataset = "PNC"),
                            all_endpoints_HCPD %>% mutate(dataset = "HCPD"),
                            all_endpoints_HBN %>% mutate(dataset = "HBN"))

# Calculate the averages of mean_SA and age_effect across all dataframes
all_endpoints_avg_datasets <- combined_df %>%
  group_by(bundle_name, end)  %>% summarize(mean_SA = mean(mean_SA, na.rm = TRUE),
                                            age_effect = mean(age_effect, na.rm = TRUE),
                                            .groups = "drop")
 
# tract-level: Pearson's with ALL endpoints (immature and matured)
# for using PNC's tract to region mapping for the across-dataset average
tractlevel_SA_p <- perm.sphere.SAaxis(glasser_SAaxis$SA.axis_rank, perm.id.full, "avg_datasets")  
write.csv(tractlevel_SA_p, paste0(output_dir, "/supp_tractlevel_pearson_pspin_allendpoints.csv"), row.names=F)

# tract-level: Pearson's (previously Spearman's) with matured only (plus a t-test)
## binarize age of maturation df's to get matured and not-yet-matured regions
## spin
print(paste("Age of maturation vs. SA rank spin test running for", dataset))
tractlevel_SA_ttest_p <- perm.sphere.SAaxis(glasser_SAaxis$SA.axis_rank, spun_ttest = TRUE, perm.id.full, "avg_datasets")
write.csv(tractlevel_SA_ttest_p, paste0(output_dir, "/supp_tractlevel_pearson_ttest_pspin_maturedonly.csv"), row.names=F)

###############################################################
# Supplementary Fig. 
# parcel-level: age of maturation vs. mean S-A rank
###############################################################
# make an average aggregated df  
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
aggregated_axis <- right_join(combined_df, glasser_SAaxis, by = "regionName")  

pspin <- perm.sphere.p(x = aggregated_axis$SA.axis_rank, y = aggregated_axis$regional_mean_ageeffect, perm.id = perm.id.full, corr.type = "pearson") 
cor <- cor.test(aggregated_axis$SA.axis_rank, aggregated_axis$regional_mean_ageeffect, method = "pearson", use = "complete.obs")$estimate
ageMat_SA_p_df <- data.frame(list(p.perm = pspin, rho.emp = cor), row.names = NULL)
ageMat_SA_p_df <- as.data.frame(ageMat_SA_p_df)
write.csv(ageMat_SA_p_df, paste0(output_dir, "/supp_parcellevel_pearson_pspin_avgdatasets.csv"), row.names=F)

print("Script finished!")
