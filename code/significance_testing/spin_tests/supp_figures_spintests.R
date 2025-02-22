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
args <- commandArgs(trailingOnly = TRUE) 
dataset = args[1]
print(paste("Running spin tests for", dataset))

################# 
# set directories
################# 
data_root <- "/cbica/projects/luo_wm_dev/"
proj_root <- "/cbica/projects/luo_wm_dev/two_axes_manuscript/"
input_root <- paste0(proj_root, "input")
output_root <- paste0(proj_root, "output")

output_dir <- paste0(output_root, "/", dataset, "/suppfigs_spintests")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message("Folder created: ", output_dir)
} else {
  message("Folder already exists: ", output_dir)
}

scalar = "dti_md"
ageeffects <- read.csv(sprintf("%1$s/%2$s/GAM/%3$s/%2$s_GAM_dev_measures.csv", output_root, dataset, scalar))
invisible(assign(paste0(dataset, "_ageeffects"), ageeffects, envir = .GlobalEnv))

# format age effect df's (the functions were originally formatted for all 3 datasets, hence the lapply's)
ageeffect_df_names <- c()
ageeffect_df_names <- append(ageeffect_df_names, paste0(dataset, "_ageeffects"))
ageeffect_dfs <- lapply(ageeffect_df_names, format_ageeffect)
ageeffect.fdr_dfs <- lapply(ageeffect_dfs, sig_nodes)
names(ageeffect.fdr_dfs) <- ageeffect_df_names
tracts = unique(ageeffect.fdr_dfs[[paste0(dataset, "_ageeffects")]]$tract_label)

# load glasser labels
glasser_labels <- read.csv("/cbica/projects/luo_wm_dev/atlases/glasser/HCP-MMP1_UniqueRegionList.csv")
glasser_labels$regionID <- c(1:360)
glasser_labels$region <- gsub("7Pl", "7PL", glasser_labels$region)   

# load maps for depth = 1.5mm
lapply("1.5", load_maps, paste0(dataset)) # makes lh_maps_[dataset], rh_maps_[dataset]

 
# load S-A axis
glasser_SAaxis <- read.csv("/cbica/projects/luo_wm_dev/SAaxis/glasser_SAaxis.csv")
glasser_SAaxis <- glasser_SAaxis %>% select(SA.axis_rank, label)
glasser_SAaxis$regionName <- gsub("_ROI", "", glasser_SAaxis$label)
glasser_SAaxis$regionName <- gsub("^(.)_(.*)$", "\\2_\\1", glasser_SAaxis$region)

# make cortical endpoint maps for age of maturation
threshold=0.3
deveffects_5_agemat <- ageeffect.fdr_dfs[[paste0(dataset, "_ageeffects")]] %>% filter((nodeID < 10 & nodeID > 4) | (nodeID < 95 & nodeID > 89)) %>% mutate(node_position = case_when((nodeID < 10 & nodeID > 4) ~ "end1", (nodeID < 95 & nodeID > 89) ~ "end2")) %>%
  select(tract_label, tractID, nodeID, node_position, hemi, smooth.peak.change, smooth.decrease.offset, smooth.last.change, smooth.slowing.onset) %>%
  group_by(tractID, node_position, hemi) %>% summarise(mean_peak_change = mean(smooth.peak.change, na.rm = T),
                                                       mean_ageeffect = mean(smooth.decrease.offset, na.rm = T),
                                                       mean_last_change = mean(smooth.last.change, na.rm = T),
                                                       mean_dev_slowing = mean(smooth.slowing.onset, na.rm = T))
invisible(assign(paste0(dataset, "_deveffects_5_agemat"), deveffects_5_agemat, envir = .GlobalEnv))
make_maps(paste0(dataset), "5_agemat") # makes [bundle_name]_deveffect_[dataset] for each dataset and bundle. e.g. IFOL_deveffect_PNC

# aggregate age maps
region_all <- aggregate_age_maps(paste0(dataset))  
lh_by_region <- region_all[[1]]
rh_by_region <- region_all[[2]]
invisible(assign(paste0("region_all_", dataset), region_all, envir = .GlobalEnv))
invisible(assign(paste0("lh_by_region_", dataset), lh_by_region, envir = .GlobalEnv))
invisible(assign(paste0("rh_by_region_", dataset), rh_by_region, envir = .GlobalEnv))

# compute SA rank for endpoints
all_endpoints <- compute_mean_SA(paste0(dataset))  
lh_all_endpoints <- all_endpoints[[1]]
rh_all_endpoints <- all_endpoints[[2]]
all_endpoints <- all_endpoints[[3]]
invisible(assign(paste0("all_endpoints_", dataset), all_endpoints, envir = .GlobalEnv))
invisible(assign(paste0("lh_all_endpoints_", dataset), lh_all_endpoints, envir = .GlobalEnv))
invisible(assign(paste0("rh_all_endpoints_", dataset), rh_all_endpoints, envir = .GlobalEnv))

###############################################################
# Supplementary Fig. 
# tract-level: age of maturation vs. mean S-A rank
###############################################################
# tract-level: Pearson's with ALL endpoints (immature and matured)
tractlevel_SA_p <- perm.sphere.SAaxis(glasser_SAaxis$SA.axis_rank, perm.id.full, paste0(dataset))  
write.csv(tractlevel_SA_p, paste0(output_dir, "/supp_tractlevel_pearson_pspin_allendpoints.csv"), row.names=F)

# tract-level: Pearson's with matured only (plus a t-test)
## binarize age of maturation df's to get matured and not-yet-matured regions
all_endpoints_binary <- all_endpoints
max_value <- max(all_endpoints_binary$age_effect, na.rm = TRUE)
all_endpoints_binary$age_effect[all_endpoints_binary$age_effect == max_value] <- NA
all_endpoints_binary <- all_endpoints_binary %>% mutate(maturation_status = ifelse(is.na(age_effect), 0, 1)) 

## spin
print(paste("Age of maturation vs. SA rank spin test running for", dataset))
tractlevel_SA_ttest_p <- perm.sphere.SAaxis(glasser_SAaxis$SA.axis_rank, spun_ttest = TRUE, perm.id.full, paste0(dataset))
write.csv(tractlevel_SA_ttest_p, paste0(output_dir, "/supp_tractlevel_pearson_ttest_pspin_maturedonly.csv"), row.names=F)

###############################################################
# Supplementary Fig. 
# parcel-level: age of maturation vs. mean S-A rank
###############################################################
aggregated_axis <- merge_SA_parcel(paste0(dataset))  
invisible(assign(paste0("aggregated_axis_", dataset), aggregated_axis, envir = .GlobalEnv))

pspin <- perm.sphere.p(x = aggregated_axis$SA.axis_rank, y = aggregated_axis$regional_mean_ageeffect, perm.id = perm.id.full, corr.type = "pearson") 
cor <- cor.test(aggregated_axis$SA.axis_rank, aggregated_axis$regional_mean_ageeffect, method = "pearson", use = "complete.obs")$estimate
ageMat_SA_p_df <- data.frame(list(p.perm = pspin, rho.emp = cor), row.names = NULL)
ageMat_SA_p_df <- as.data.frame(ageMat_SA_p_df)
write.csv(ageMat_SA_p_df, paste0(output_dir, "/supp_parcellevel_pearson_pspin.csv"), row.names=F)

print("Script finished!")
