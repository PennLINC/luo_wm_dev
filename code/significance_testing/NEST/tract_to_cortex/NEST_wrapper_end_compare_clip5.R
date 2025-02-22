library(dplyr)
library(gratia) 
library(mgcv)
library(parallel)
library(rjson)
library(stringr)
library(tidyr)
library(NEST)

################## 
# Set Variables 
################## 
args <- commandArgs(trailingOnly = TRUE) 
dataset = args[1]
tract = args[2]
print(paste("Running NEST for", dataset, tract))

################## 
# Set Directories 
################## 
config_data <- fromJSON(file=sprintf("/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_%1$s.json", dataset))
demographics <- read.csv(config_data$demo_qc)
tract_prof_output_root <- config_data$outputs_root
NEST_outputs_dir <- paste0(tract_prof_output_root, "/NEST/dti_md/")

if (dir.exists(NEST_outputs_dir)) {
  print(paste(NEST_outputs_dir, "already exists"))
} else {
  dir.create(NEST_outputs_dir, recursive = TRUE)
  print(paste(NEST_outputs_dir, "created"))
}

################### 
# Define functions 
###################
# function for clipping ends
clip_ends <- function(nodes_to_clip, nodes_per_tract) {
  c(0:(nodes_to_clip - 1), (nodes_per_tract - nodes_to_clip):(nodes_per_tract - 1))
}

# a wrapper function for running NEST on each tract
NEST_wrapper <- function(tract, df, bin_size, nodes_to_clip) {
  df_tract <- df %>% filter(tract_label == tract)
  df_wide <- df_tract %>% select(sub, tract_node, all_of("dti_md")) %>% pivot_wider(names_from = "tract_node", values_from = "dti_md") 
  
  if(!identical(demographics$sub, as.character(df_wide$sub))) {
    stop("Error: The 'sub' columns in 'demographics' and 'df_wide' do not match.")
  }
  
  df_wide <- df_wide %>% select(-sub)
  
  # set phenotype of interest (age) and covariates (sex + mean_fd)
  demographics$sex <- as.numeric(as.factor(demographics$sex))
  # 1 = F, 2 = M
  covs <- as.matrix(cbind( demographics$age, demographics$sex, as.numeric(demographics$mean_fd))) # matrix dimensions = N x 3
  colnames(covs) <- c("age", "sex", "mean_fd")
  
  # get column names
  colnames_df <- names(df_wide)
  
  # get node IDs from column names
  node_ids <- as.numeric(sub(".*_(\\d+)$", "\\1", colnames_df))
  
  # identify left and right tracts for appropriate tracts
  if (str_detect(tract, "Callosum")) {
    total_node_ids <- node_ids
  } else { # all other tracts including CST
    is_left <- grepl("^Left_", colnames_df)
    is_right <- grepl("^Right_", colnames_df)
    # create total node IDs: 0-99 for left, 100-199 for right
    total_node_ids <- ifelse(is_left, node_ids, node_ids + nodes_per_tract)
  }
  
  
  # mapping from total_node_ids to column names
  node_id_to_colname <- setNames(colnames_df, total_node_ids)
  
  # clips the ends of tracts and label end1 vs end2
  if (str_detect(tract, "Callosum")) {
    # Total nodes (0 to 99)
    total_nodes <- nodes_per_tract
    
    # clip ends
    clipped_nodes <- clip_ends(nodes_to_clip, nodes_per_tract)
    
    # identify nodes for each end
    end1_nodes <- c((nodes_to_clip):(nodes_to_clip + bin_size - 1))
    end2_nodes <- c((nodes_per_tract - nodes_to_clip - bin_size):(nodes_per_tract - nodes_to_clip - 1))
  } else {
    #Total nodes (0 to 199 for both hemispheres)
    total_nodes <- nodes_per_tract * 2  
    
    # clip ends
    clipped_nodes_left <- clip_ends(nodes_to_clip, nodes_per_tract)
    clipped_nodes_right <- clip_ends(nodes_to_clip, nodes_per_tract) + nodes_per_tract
    clipped_nodes <- c(clipped_nodes_left, clipped_nodes_right)
    
    # identify nodes for each end
    end1_nodes_left <- c((nodes_to_clip):(nodes_to_clip + bin_size - 1))
    end2_nodes_left <- c((nodes_per_tract - nodes_to_clip - bin_size):(nodes_per_tract - nodes_to_clip - 1))
    
    end1_nodes_right <- c((nodes_to_clip):(nodes_to_clip + bin_size - 1)) + nodes_per_tract
    end2_nodes_right <- c((nodes_per_tract - nodes_to_clip - bin_size):(nodes_per_tract - nodes_to_clip - 1)) + nodes_per_tract
    
    end1_nodes <- c(end1_nodes_left, end1_nodes_right)
    end2_nodes <- c(end2_nodes_left, end2_nodes_right)
    
  }
  
  # node type identifier (peripheral = 1, deep = 2, clip or exclude = 0)
  node_type <- rep(0, total_nodes)
  node_type[end1_nodes + 1] <- 1  # end1 nodes
  node_type[end2_nodes + 1] <- 2  # end2 nodes
  
  # keep only end1 and end2 nodes
  nodes_to_keep <- setdiff(0:(total_nodes - 1), clipped_nodes)
  final_nodes <- nodes_to_keep[node_type[nodes_to_keep + 1] > 0]
  
  # map final_nodes to column names using node_id_to_colname
  final_colnames <- node_id_to_colname[as.character(final_nodes)]
  
  # make final df
  df_final <- df_wide[, final_colnames]
  
  # final variables for NEST. See https://github.com/smweinst/NEST/blob/main/R/readme.md 
  X <- as.matrix(df_final)
  colnames(X) <- NULL
  dat <- covs
  end1_idx <- rep(1, length(final_nodes))
  end1_idx[final_nodes %in% end2_nodes] <- 0 # set indices corresponding to end2 nodes to 0
  net <- list(end2_nodes <- end1_idx) # list of nodes of interest (peripheral nodes)
  cbind(names(df_final), unlist(net))
  
  print(paste("Running NEST for", tract))
  print(paste("Number of nodes included as peripheral nodes:", bin_size))
  
  # one-sided: is delta adj rsq more extreme end1 vs. end2?
  print("Running one-sided NEST")
  result_onesided <- NEST(
  statFun = "gam.deltaRsq",
  args = list(X = X, 
              dat = dat, 
              gam.full.formula = as.formula(X.v ~ s(age, k = 3, fx = TRUE) + sex + mean_fd), 
              gam.null.formula = as.formula(X.v ~ sex + mean_fd), 
              lm.formula = as.formula(X.v ~ age + sex + mean_fd),  
              y.in.gam = "s(age)", 
              y.in.lm = "age", 
              y.permute = "age",  
              n.perm = 10000),
   net.maps = net,
   one.sided = TRUE,
   n.cores = 4, 
   seed = 123, 
  what.to.return = "everything"
  )
  saveRDS(result_onesided, paste0(NEST_outputs_dir, gsub(" ", "_", gsub("-", "_",tract)), "_bin", bin_size, "_clip", nodes_to_clip, "_1sided_end_compare.RData"))
  
}

df <- readRDS(sprintf("%1$s/tract_profiles/tract_profiles_for_NEST.RData", tract_prof_output_root))

print(paste0(dataset, " df loaded"))
df$tract_label <- gsub(" ", "_", df$tract_label)

# set var
nodes_per_tract <- 100 # nodes per tract (nodeIDs from 0 to 99)

################### 
# Run NEST
################### 
NEST_wrapper(tract, df = df, bin_size = 5, nodes_to_clip = 5)
