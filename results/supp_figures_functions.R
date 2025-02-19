library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(gratia) 
library(knitr)
library(mgcv)
library(RColorBrewer)
library(scales)
library(stringr)
library(rjson)
library(tidyr)

########################
# Supplementary Figs
########################

######################################
# Supplementary Figure 1. Hex plot of MD age effects 
######################################

# function for permutation-based correlation test
perm_cor_test <- function(vector1, vector2, n_perm = 10000, seed = 123) {
  # empirical correlation
  rho.emp <- cor(vector1, vector2, use = "complete.obs")
  
  # null correlation vector
  rho.perm <- numeric(n_perm)
  
  # set seed
  set.seed(seed)
  
  # permute
  for (i in 1:n_perm) {
    permuted_vector2 <- sample(vector2)  # shuffle one vector
    rho.perm[i] <- cor(vector1, permuted_vector2, use = "complete.obs")
  }
  
  # compute p-value (two-tailed test)
  exceed_count <- sum(abs(rho.perm) >= abs(rho.emp))
  p_value <- (1 + exceed_count) / (1 + n_perm)
  
  return(list(rho.emp = rho.emp, p_value = p_value))
}


# hex plot function for correlations between datasets
hex_plot <- function(df, x, y, text, ylim1, ylim2, xlim1, xlim2, x_text, y_text) {
  plot <- ggplot(df, aes_string(x = x, y = y)) +
    geom_hex(bins = 7) +
    paletteer::scale_fill_paletteer_c("ggthemes::Blue-Green Sequential", direction = -1) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +   
    annotate("text", x = x_text, y = y_text, 
             label = text, 
             hjust = 0, vjust = 1, size = 7, color = "black") + 
    theme(legend.position = "bottom", 
          legend.key.width = unit(2, 'cm'), legend.key.height = unit(1, 'cm'),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.margin = unit(c(0.2, 0.5, 0.2, 1), "cm")) + 
    labs(x = x, y = y, fill = "Count") + ylim(ylim1, ylim2) + xlim(xlim1, xlim2)
  return(plot)
}

############################################################################
# Supplementary Table 1. Coefficient of Variation for tract ends and deep tract regions
############################################################################
# make summary df's: this creates summary_[dataset]_[scalar] dataframes that include mean, sd, se for each scalar/dataset
make_summary_dfs <- function(scalar, dataset_name, df) {
  # set variables
  mean_scalar <- paste0("mean_", scalar)
  # Process the given dataset df
  scalar_data <- df %>% select(sub, tractID, tract_label, nodeID, hemi, all_of(scalar))
  sqrt_n <- sqrt(length(unique(scalar_data$sub)))
  setDT(scalar_data)
  summary_data <- scalar_data[, .(
    mean_scalar = mean(.SD[[1]], na.rm = TRUE),
    sd = sd(.SD[[1]], na.rm = TRUE),
    se = sd(.SD[[1]], na.rm = TRUE) / sqrt_n,
    ymin_sd = mean(.SD[[1]], na.rm = TRUE) - sd(.SD[[1]], na.rm = TRUE),
    ymax_sd = mean(.SD[[1]], na.rm = TRUE) + sd(.SD[[1]], na.rm = TRUE),
    ymin_se = mean(.SD[[1]], na.rm = TRUE) - (sd(.SD[[1]], na.rm = TRUE) / sqrt_n),
    ymax_se = mean(.SD[[1]], na.rm = TRUE) + (sd(.SD[[1]], na.rm = TRUE) / sqrt_n),
    cv = (sd(.SD[[1]], na.rm = TRUE) / (mean(.SD[[1]], na.rm = TRUE))) * 100
  ), by = .(tract_label, tractID, nodeID, hemi), .SDcols = scalar]
  
  setnames(summary_data, "mean_scalar", paste0("mean_", scalar))
  summary_data[, Dataset := dataset_name]
  summary_dataset_scalar <- paste0("summary_", dataset_name, "_", scalar)
  assign(summary_dataset_scalar, summary_data, envir = .GlobalEnv)
}

############################################################################
# Supplementary Figure 3. Age effect plots for fractional anisotropy (FA).
############################################################################
# functions for arranging different tracts into 1 big plot (exluding lollipop plots)
## callosum
arrange_callosum_plots_nolollipop <- function(list_figures, title) {
  callosum1_plots <- ggarrange(list_figures$`Callosum Orbital` + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Orbital", size = 20, y = 0.2, hjust = 1), 
                               list_figures$`Callosum Anterior Frontal` + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Anterior Frontal", size = 20, y = 0.2, hjust = 1),  
                               list_figures$`Callosum Superior Frontal`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Superior Frontal", size = 20, y = 0.2, hjust = 1), 
                               list_figures$`Callosum Motor`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Motor", size = 20, y = 0.2, hjust = 1), ncol = 4, nrow = 1)  
  callosum1_plots <- plot_grid(ggdraw() + draw_label(expression("Age Effect (|" * Delta * " Adjusted " * R^2 * "|)"), size = 20, vjust = 0.5, hjust = 0.4, angle = 90), 
                               callosum1_plots, rel_widths = c(0.02, 1))
  callosum1_plots <- annotate_figure(callosum1_plots, bottom = text_grob("", color = "black", size = 20, hjust = 0.3))
  
  
  
  callosum2_plots <- ggarrange(list_figures$`Callosum Superior Parietal`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Superior Parietal", size = 20, y = 0.2, hjust = 1), 
                               list_figures$`Callosum Posterior Parietal`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Posterior Parietal", size = 20, y = 0.2, hjust = 1), 
                               list_figures$`Callosum Temporal`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Temporal", size = 20, y = 0.2, hjust = 1), 
                               list_figures$`Callosum Occipital`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Callosum Occipital", size = 20, y = 0.2, hjust = 1),  ncol = 4, nrow = 1)  
  callosum2_plots <- plot_grid(ggdraw() + draw_label(expression("Age Effect (|" * Delta * " Adjusted " * R^2 * "|)"), size = 20, vjust = 0.5, hjust = 0.4, angle = 90), 
                               callosum2_plots, rel_widths = c(0.02, 1))
  callosum2_plots <- annotate_figure(callosum2_plots, bottom = text_grob("Position on Tract (Node ID)", color = "black", size = 20, hjust = 0.3))
  
  callosum_plot_final <- ggarrange(callosum1_plots, callosum2_plots, nrow = 2) + bgcolor("white")  
  return(callosum_plot_final)
}

# association FA plots
arrange_association_plots_nolollipop <- function(list_figures) {
  AP_plots <- ggarrange(list_figures$`Arcuate` + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Arcuate", size = 18, y = 0.2, hjust = 0.5),
                        list_figures$`Inferior Fronto-occipital`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Inferior Fronto-occipital", size = 18, y = 0.2, hjust = 0.5), 
                        list_figures$`Inferior Longitudinal`+ theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Inferior Longitudinal", size = 18, y = 0.2, hjust = 0.5), 
                        list_figures$`Superior Longitudinal` + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Superior Longitudinal", size = 18, y = 0.2, hjust = 0.5), ncol = 4, nrow = 1)
  AP_plots <- plot_grid(ggdraw() + draw_label(expression("Age Effect (|" * Delta * " Adjusted " * R^2 * "|)"), size = 18, vjust = 0.5, hjust = 0.4, angle = 90), 
                        AP_plots, rel_widths = c(0.02, 1))
  AP_plots_final <- annotate_figure(AP_plots, top = text_grob("Anterior - Posterior", 
                                                              color = "black", face = "bold", size = 18, hjust = 0.4, vjust = 0.2), 
                                    bottom = text_grob("", color = "black", size = 18, hjust = 0.3))
  
  blank_plot <- ggdraw() + theme_void()
  SI_plots <- ggarrange(list_figures$`Posterior Arcuate` + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Posterior Arcuate", size = 18, y = 0.2, hjust = 0.5), 
                        list_figures$`Uncinate` + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Uncinate", size = 18, y = 0.2, hjust = 0.5),
                        list_figures$`Vertical Occipital` + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0.5, 0.5), "cm")) + labs(title = "Vertical Occipital", size = 18, y = 0.2, hjust = 0.5),
                        blank_plot, 
                        ncol=4, nrow = 1)
  SI_plots <- plot_grid(ggdraw() + draw_label(expression("Age Effect (|" * Delta * " Adjusted " * R^2 * "|)"), size = 18, vjust = 0.5, hjust = 0.4, angle = 90), 
                        SI_plots, rel_widths = c(0.02, 1))
  
  SI_plots_final <- annotate_figure(SI_plots, top = text_grob("Superior - Inferior", 
                                                              color = "black", face = "bold", size = 18, hjust = 0.4, vjust = 0.2),
                                    bottom = text_grob("Position on Tract (Node ID)", color = "black", size = 18, hjust = 0.3))
  tractprofiles_plot_final <- ggarrange(AP_plots_final, SI_plots_final, nrow = 2) + bgcolor("white") 
  return(tractprofiles_plot_final) 
}

##################################################################################
# Supplementary Figure 4. Tract-level: age of maturation association with S-A axis.
##################################################################################

plot_meanSA_by_age_mat <- function(dataset, annot_text, binary=NULL) {
  title = gsub("PD", "P-D", dataset) # for HCPD
  
  if(is.null(binary)) {
    all_endpoints <- get(paste0("all_endpoints_", dataset))
    
    SA_plot <- ggplot(all_endpoints, aes(x = mean_SA, y = age_effect, label = bundle_name)) +
      geom_point(aes(fill = mean_SA, color = mean_SA, shape = end), size = 4) + 
      paletteer::scale_fill_paletteer_c("grDevices::ag_Sunset", direction = -1, 
                                        limits = c(min(all_endpoints$mean_SA), max(all_endpoints$mean_SA)), oob = squish) + 
      paletteer::scale_color_paletteer_c("grDevices::ag_Sunset", direction = -1,
                                         limits = c(min(all_endpoints$mean_SA), max(all_endpoints$mean_SA)), oob = squish) +
      scale_shape_manual(values = c(19,1)) + 
      geom_smooth(data = all_endpoints, method='lm', se=TRUE, fill=alpha(c("gray70"),.9), col="black") + 
      ggrepel::geom_text_repel(
        size = 5, 
        position = position_jitter(seed = 3), 
        box.padding = 0.5,  # Increase space around labels
        point.padding = 0.5,  # Increase space around points
        segment.color = "gray",  # Line color connecting point to label
        segment.size = 0.5,  # Line thickness
        max.overlaps = Inf 
      )  + 
      annotate(geom="text", x=180, y=27, label=annot_text, color="black", size=8) + 
      labs(title = title) + theme_classic() +
      theme(legend.position = "none",
            legend.text = element_text(size = 24),
            legend.title = element_text(size = 24),
            plot.title = element_text(hjust = 0.5, size = 24),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 24),
            axis.text.y = element_text(size = 24)) + xlim(15, 340) + ylim(13, 27) + guides(shape = guide_legend("Endpoint"), color = FALSE, fill = FALSE) 
  } else {
    all_endpoints <- get(paste0("all_endpoints_", dataset))
    all_endpoints_binary <- all_endpoints
    max_value <- max(all_endpoints_binary$age_effect, na.rm = TRUE)
    all_endpoints_binary$age_effect[all_endpoints_binary$age_effect == max_value] <- NA
    all_endpoints_binary <- all_endpoints_binary %>% mutate(maturation_status = ifelse(is.na(age_effect), 0, 1)) # 0 = not matured, 1 = matured
    
    SA_plot <- ggplot(all_endpoints_binary, aes(x = mean_SA, y = age_effect, label = bundle_name)) +
      geom_point(aes(fill = mean_SA, color = mean_SA, shape = end), size = 4) + 
      paletteer::scale_fill_paletteer_c("grDevices::ag_Sunset", direction = -1, 
                                        limits = c(min(all_endpoints$mean_SA), max(all_endpoints$mean_SA)), oob = squish) + 
      paletteer::scale_color_paletteer_c("grDevices::ag_Sunset", direction = -1,
                                         limits = c(min(all_endpoints$mean_SA), max(all_endpoints$mean_SA)), oob = squish) +
      scale_shape_manual(values = c(19,1)) + 
      geom_smooth(data = all_endpoints_binary, method='lm', se=TRUE, fill=alpha(c("gray70"),.9), col="black") + 
      ggrepel::geom_text_repel(
        size = 5, 
        position = position_jitter(seed = 3), 
        box.padding = 0.5,  # space around labels
        point.padding = 0.5,  # space around points
        segment.color = "gray",  # line color connecting point to label
        segment.size = 0.5,  # line thickness
        max.overlaps = Inf 
      )  + 
      annotate(geom="text", x=180, y=27, label=annot_text, color="black", size=8) + 
      labs(title = title) + theme_classic() +
      theme(legend.position = "none",
            legend.text = element_text(size = 24),
            legend.title = element_text(size = 24),
            plot.title = element_text(hjust = 0.5, size = 24),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 24),
            axis.text.y = element_text(size = 24)) + xlim(15, 340) + ylim(13, 27) + guides(shape = guide_legend("Endpoint"), color = FALSE, fill = FALSE) 
    
  }
  
  return(SA_plot)
}

# spin test for tract-level comparison to the S-A axis. This function spins the S-A axis then recomputes average S-A rank for each end. 
# Option to do pearson's (all endpoints) or spearman's (only matured included in correlation, plus a spun t-test)
# @param SAaxis, vector of S-A axis ranks
perm.sphere.SAaxis <- function(SAaxis, perm.id, dataset, spun_ttest = FALSE, alternative = "greater", var.equal = FALSE) {
  
  nroi = dim(perm.id)[1]  # number of regions
  nperm = dim(perm.id)[2] # number of permutations
  
  # spin SA axis: permutation of measures
  SAaxis.perm = array(NA,dim=c(nroi,nperm))
  for (r in 1:nperm) {
    for (i in 1:nroi) {
      SAaxis.perm[i,r] = SAaxis[perm.id[i,r]]
    }
  }
  # if just want to compute spun p-value for age of maturation vs mean S-A rank for all cortical endpoints: 
  if (spun_ttest == FALSE) { 
    # empirical correlation
    all_endpoints <- get(paste0("all_endpoints_", dataset))
    rho.emp = cor(all_endpoints$mean_SA, all_endpoints$age_effect, method = "pearson", use="complete.obs")  
    
    # correlation between permuted S-A axis and age of maturation
    rho.null.SAaxis = vector(length=nperm)
    
    # when averaging across datasets, use PNC's bundle-to-cortex probability map
    if(dataset=="avg_datasets") {
      dataset <- "PNC"
    }
    
    for (r in 1:nperm) {
      print(r)
      # merging of permuted S-A axis to age of maturation df
      perm_mean_SAaxis <- compute_mean_SA(dataset, SAaxis.perm[,r]) # merge permuted S-A rank with age of maturation AND compute mean permuted S-A ranks
      perm_mean_SAaxis <- perm_mean_SAaxis[[3]] # get all_endpoints df
      rho.null.SAaxis[r] = cor(perm_mean_SAaxis$mean_SA, perm_mean_SAaxis$age_effect, method="pearson", use="complete.obs")
    }
    # p-value definition depends on the sign of the empirical correlation
    if (rho.emp>0) {
      p.perm = sum(rho.null.SAaxis>rho.emp)/nperm
    } else { 
      p.perm = sum(rho.null.SAaxis<rho.emp)/nperm
    } 
    return(list(p.perm = p.perm, rho.emp = rho.emp))
    # return p-value for the correlation between age of maturation vs. mean S-A rank for cortical endpoint
    
  } else if(spun_ttest == TRUE) {
    
    all_endpoints <- get(paste0("all_endpoints_", dataset))
    all_endpoints_binary <- all_endpoints
    max_value <- max(all_endpoints_binary$age_effect, na.rm = TRUE)
    all_endpoints_binary$age_effect[all_endpoints_binary$age_effect == max_value] <- NA
    all_endpoints_binary <- all_endpoints_binary %>% mutate(maturation_status = ifelse(is.na(age_effect), 0, 1)) # 0 = not matured, 1 = matured
    
    # empirical correlation
    rho.emp = cor(all_endpoints_binary$mean_SA, all_endpoints_binary$age_effect, method = "pearson", use="complete.obs")  # previously spearman
    
    # empirical t value
    t.emp <- t.test(all_endpoints_binary$mean_SA[which(all_endpoints_binary$maturation_status == 0)], # compare differences in sa rank
                    all_endpoints_binary$mean_SA[which(all_endpoints_binary$maturation_status == 1)], 
                    alternative = alternative, var.equal = var.equal)$statistic # hypothesis is non-matured tract ends have greater S-A rank
    
    df.emp <- t.test(all_endpoints_binary$mean_SA[which(all_endpoints_binary$maturation_status == 0)], # compare differences in sa rank
                    all_endpoints_binary$mean_SA[which(all_endpoints_binary$maturation_status == 1)], 
                    alternative = alternative, var.equal = var.equal)$parameter 
    
    # set vector for correlations between permuted S-A axis and age of maturation
    rho.null.SAaxis = vector(length=nperm)
    # set vector for t-tests between permuted S-A axis differences and age of maturation differences between endpoints
    t.null.SAaxis = vector(length=nperm)
    
    # when averaging across datasets, use PNC's bundle-to-cortex probability map
    if(dataset=="avg_datasets") {
      dataset <- "PNC"
    }
    
    for (r in 1:nperm) {
      print(r)
      # merging of permuted S-A axis to age of maturation df
      perm_mean_SAaxis <- compute_mean_SA(dataset, SAaxis.perm[,r]) # merge permuted S-A rank with age of maturation AND compute mean permuted S-A ranks
      perm_mean_SAaxis <- perm_mean_SAaxis[[3]] # get all_endpoints df
      max_value_perm <- max(perm_mean_SAaxis$age_effect, na.rm = TRUE)
      perm_mean_SAaxis$age_effect[perm_mean_SAaxis$age_effect == max_value_perm] <- NA
      rho.null.SAaxis[r] = cor(perm_mean_SAaxis$mean_SA, perm_mean_SAaxis$age_effect, method="spearman", use="complete.obs")
      
      perm_mean_SAaxis <- perm_mean_SAaxis %>% mutate(maturation_status = ifelse(is.na(age_effect), 0, 1)) # 0 = not matured, 1 = matured
      
      # calculate t-test and permuted t-test: do tract ends that have NOT matured between endpoints actually have greater s-a ranks than tract ends that have matured?? 
      t.null.SAaxis[r] <- t.test(perm_mean_SAaxis$mean_SA[which(perm_mean_SAaxis$maturation_status == 0)], 
                                 perm_mean_SAaxis$mean_SA[which(perm_mean_SAaxis$maturation_status == 1)], 
                                 alternative = alternative, var.equal = var.equal)$statistic
    }
    # p-value definition depends on the sign of the empirical correlation
    if (rho.emp>0) {
      p.perm.cor = sum(rho.null.SAaxis>rho.emp)/nperm
    } else { np
      p.perm.cor = sum(rho.null.SAaxis<rho.emp)/nperm
    } 
    
    # p-value definition for t-test
    if (t.emp>0) {
      p.perm.t = sum(t.null.SAaxis>t.emp)/nperm
    } else { 
      p.perm.t= sum(t.null.SAaxis<t.emp)/nperm
    } 
    
    return(list(rho.emp = rho.emp, p.perm.cor = p.perm.cor, t.emp = t.emp, p.perm.t = p.perm.t, df.emp = df.emp))
  }
}


##########################################################################################
# Supplementary Figure 8. Threshold testing for tract-to-cortex maps (10%, 30%, 50%)
##########################################################################################
plot_threshold_maps <- function(map, hemi, threshold) {
  map <- map %>% mutate(thresh_probability = ifelse(probability < threshold, NA, probability)) 
  
  if(hemi == "left") {
    cortical_pos <- c("left lateral", "left medial")
  } else {
    cortical_pos <- c("right lateral", "right medial")
  } 
  if(all(is.na(map$thresh_probability))) {
    plot <- ggplot() + 
      geom_brain(data = map, atlas = glasser, 
                 mapping = aes(fill = "white"),  
                 show.legend = FALSE, 
                 hemi = hemi,
                 position = position_brain(cortical_pos)) +
      scale_fill_manual(values = "white") +  
      theme_void() +
      theme(legend.position = "bottom",
            legend.key.height = unit(1, 'cm'),
            legend.key.width = unit(2.3, 'cm'),
            legend.margin=margin(0,0,0,0),
            legend.text = element_text(size=20),
            legend.title = element_blank(),
            plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
            plot.title = element_text(size = 20, hjust = 0.5)) 
    
  } else { 
    plot <- ggplot() + 
      geom_brain(data = map, atlas= glasser, 
                 mapping=aes(fill=thresh_probability), 
                 show.legend=TRUE, 
                 hemi = hemi,
                 position = position_brain(cortical_pos)) + 
      scale_fill_gradientn(colors = aquamarine, na.value = "white", limits = c(threshold, 1), labels = label_percent(accuracy = 1)) +  
      theme_void() +
      theme(legend.position = "bottom",
            legend.key.height = unit(1, 'cm'),
            legend.key.width = unit(2.3, 'cm'),
            legend.margin=margin(0,0,0,0),
            legend.text = element_text(size=24),
            legend.title = element_blank(),
            plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
            plot.title = element_text(size=20, hjust = 0.5)) 
  }
  return(plot)
}

################################################################# 
# Endpoints maps for youngest and oldest -- not sure if keeping
################################################################# 

plot_ends <- function(bundle_name, dataset) {
  df <- get(paste0(bundle_name, "_endpoints_", dataset))
  
  if(grepl("L$", bundle_name)) {
    hemi = "left"
    cortical_pos <- c("left lateral", "left medial")
  } else{
    hemi = "right"
    cortical_pos <- c("right lateral", "right medial")
  }
  plot <- ggplot() + 
    geom_brain(data = df, atlas= glasser, 
               mapping=aes(fill=end), 
               show.legend=TRUE, 
               hemi = hemi,
               position = position_brain(cortical_pos))  +   
    scale_fill_manual(values = c("end1" = "#9A1F72FF", "end2" = "#FF9572FF"), na.value = "white")  +
    theme_void() +
    theme(legend.position = "none",
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(2.3, 'cm'),
          legend.margin=margin(0,0,0,0),
          legend.text = element_text(size=24),
          legend.title = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          plot.title = element_text(size=20, hjust = 0.5)) 
  return(plot)
  
}

# make tract to region maps of tract-end developmental effects
## have to assign cortical regions to end1 or end2 manually. There's no consistent way to automate this unfortunately.
make_endpoint_maps <- function(dataset, bin_num_nodes) {
  
  lh_maps <- get(paste0("lh_maps_", dataset))
  rh_maps <- get(paste0("rh_maps_", dataset))
  #deveffects <- get(paste0(dataset, "_deveffects_", bin_num_nodes))
  
  # for tracts with 1 endpoint (CST), extract the age effect for end1 (corresponds to cortical endpoint) 
  CSTL_endpoints <- lh_maps$LeftCorticospinal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  
  CSTR_endpoints <- rh_maps$RightCorticospinal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) 
  
  # for tracts with 2 distinct cortical endpoints: ARC, ILF, IFO, SLF
  ARCL_endpoints <- lh_maps$LeftArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse(
        !is.na(thresh_probability) & (str_detect(cortex, "(?i)frontal") | str_detect(cortex, "motor") | str_detect(cortex, "Posterior_Opercular")), 
        "end1", 
        ifelse(
          !is.na(thresh_probability) & (str_detect(cortex, "Temp") | str_detect(cortex, "Auditory") |
                                          str_detect(cortex, "(?i)Visual")), 
          "end2", 
          NA))
    )  
  
  ARCR_endpoints <- rh_maps$RightArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(end = case_when(
      !is.na(thresh_probability) & 
        (str_detect(cortex, "(?i)frontal") | str_detect(cortex, "(?i)motor") | str_detect(cortex, "(?i)Posterior_Opercular")) ~ "end1",
      !is.na(thresh_probability) & 
        (str_detect(cortex, "(?i)Temp") | str_detect(cortex, "(?i)MT") | str_detect(cortex, "(?i)Auditory")) ~ "end2",
      TRUE ~ NA_character_
    )
    )  
  
  ILFL_endpoints <- lh_maps$LeftInferiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(end = case_when(
      !is.na(thresh_probability) & 
        !str_detect(cortex, "(?i)visual|Inferior_Parietal|Occipital") ~ "end1",
      !is.na(thresh_probability) & 
        str_detect(cortex, "(?i)visual|Inferior_Parietal|Occipital") ~ "end2",
      TRUE ~ NA_character_
    )
    )  
  
  
  ILFR_endpoints <- rh_maps$RightInferiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse(
        !is.na(thresh_probability) & (!str_detect(cortex, "(?i)visual")) & region != "PHA3", 
        "end1", 
        ifelse(
          !is.na(thresh_probability) & (str_detect(cortex, "(?i)visual")) | region == "PHA3", 
          "end2", 
          NA))
    )  
  
  
  IFOL_endpoints <- lh_maps$LeftInferiorFrontooccipital %>%
    mutate(
      thresh_probability = ifelse(probability < 0.1, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal") | 
             str_detect(cortex, "Auditory")), 
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)visual") | 
               str_detect(cortex, "Pariet")), 
          "end2", 
          NA
        )
      )
    ) 
  
  IFOR_endpoints <- rh_maps$RightInferiorFrontooccipital %>%
    mutate(
      thresh_probability = ifelse(probability < 0.1, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal") | 
             str_detect(cortex, "Auditory")), 
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)visual") | 
               str_detect(cortex, "Pariet")), 
          "end2", 
          NA
        )
      )
    )  
  
  
  SLFL_endpoints <- lh_maps$LeftSuperiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(end = ifelse(
      !is.na(thresh_probability) & 
        (str_detect(cortex, "(?i)frontal") |
           str_detect(cortex, "(?i)motor") | 
           str_detect(cortex, "(?i)Opercular")), 
      "end1", 
      ifelse(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)pariet|(?i)auditory")), 
        "end2", 
        NA))) 
  
  SLFR_endpoints <- rh_maps$RightSuperiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal") |
             str_detect(cortex, "(?i)motor") | 
             str_detect(cortex, "(?i)Opercular")), 
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)pariet")), 
          "end2", 
          NA
        )
      )
    )  
  
  # for tracts with 2 cortical endpoints that require some manual work: pARC, VOF 
  pARCL_endpoints <- lh_maps$LeftPosteriorArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)parietal") | 
             regionName =="PSL_L"),   
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)tempor") |
               str_detect(cortex, "(?i)visual") |
               str_detect(cortex, "(?i)auditory")), 
          "end2", 
          NA
        )
      )
    )  
  
  
  
  pARCR_endpoints <- rh_maps$RightPosteriorArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)parietal")),  # might exclude TPOJ1 since its between the two endpoints
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)tempor") |
               str_detect(cortex, "(?i)visual") |
               str_detect(cortex, "(?i)auditory")), 
          "end2", 
          NA
        )
      )
    )  
  
  UNCL_endpoints <- lh_maps$LeftUncinate %>% # connects parts of the limbic system in areas in the temporal lobe with inferior portions of the frontal lobe such as the OFC
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal")),   
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)temporal")), 
          "end2", 
          NA
        )
      )
    ) 
  
  
  UNCR_endpoints <- rh_maps$RightUncinate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal")),   
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)temporal") | 
               str_detect(cortex, "(?i)auditory")), 
          "end2", 
          NA
        )
      )
    )  
  
  VOFL_endpoints <- lh_maps$LeftVerticalOccipital %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)dorsal") | 
             str_detect(cortex, "(?i)parietal") |
             region == "LO1" |
             region == "V3CD" |
             region == "V4" | # kind of arbitrarily placing V3 and V4  
             region == "MST" |
             region == "MT" |
             region == "TPOJ3" |
             region == "LO3"),   
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)ventral") | 
               region == "PH" |
               region == "LO2" |
               region == "V3" ), 
          "end2", 
          NA
        )
      )
    )  
  
  
  VOFR_endpoints <- rh_maps$RightVerticalOccipital %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      end = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)dorsal") |
             str_detect(cortex, "(?i)parietal") |
             region == "LO1" |
             region == "V3CD" |
             region == "V4"| # kind of arbitrarily placing V3 and V4  
             region == "LO3" | 
             region == "TPOJ3"),   
        "end1", 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)ventral") | 
               region == "PH" |
               region == "LO2" |
               region == "V3" | 
               region == "MST" | 
               region == "FST"), 
          "end2", 
          NA
        )
      )
    )  
  
  # callosum bundles
  COrbL_endpoints <- lh_maps$LeftCallosumOrbital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    )  
  
  COrbR_endpoints <- rh_maps$RightCallosumOrbital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  
  CAntFrL_endpoints <- lh_maps$LeftCallosumAnteriorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    )  
  
  CAntFrR_endpoints <- rh_maps$RightCallosumAnteriorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  CSupFrL_endpoints <- lh_maps$LeftCallosumSuperiorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    )  
  
  
  CSupFrR_endpoints <- rh_maps$RightCallosumSuperiorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  
  
  CMotL_endpoints <- lh_maps$LeftCallosumMotor %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    )  
  
  CMotR_endpoints <- rh_maps$RightCallosumMotor %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) 
  
  CSupParL_endpoints <- lh_maps$LeftCallosumSuperiorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    )  
  
  CSupParR_endpoints <- rh_maps$RightCallosumSuperiorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  CPostParL_endpoints <- lh_maps$LeftCallosumPosteriorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    )  
  
  
  CPostParR_endpoints <- rh_maps$RightCallosumPosteriorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  CTempL_endpoints <- lh_maps$LeftCallosumTemporal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) 
  
  CTempR_endpoints <- rh_maps$RightCallosumTemporal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  COccL_endpoints <- lh_maps$LeftCallosumOccipital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) 
  
  COccR_endpoints <- rh_maps$RightCallosumOccipital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    )  
  
  invisible(assign(paste0("CSTL_endpoints_", dataset), CSTL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("CSTR_endpoints_", dataset), CSTR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("ARCL_endpoints_", dataset), ARCL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("ARCR_endpoints_", dataset), ARCR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("IFOL_endpoints_", dataset), IFOL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("IFOR_endpoints_", dataset), IFOR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("ILFL_endpoints_", dataset), ILFL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("ILFR_endpoints_", dataset), ILFR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("pARCL_endpoints_", dataset), pARCL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("pARCR_endpoints_", dataset), pARCR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("SLFL_endpoints_", dataset), SLFL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("SLFR_endpoints_", dataset), SLFR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("UNCL_endpoints_", dataset), UNCL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("UNCR_endpoints_", dataset), UNCR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("VOFL_endpoints_", dataset), VOFL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("VOFR_endpoints_", dataset), VOFR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("COrbL_endpoints_", dataset), COrbL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("COrbR_endpoints_", dataset), COrbR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("CAntFrL_endpoints_", dataset), CAntFrL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("CAntFrR_endpoints_", dataset), CAntFrR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("CSupFrL_endpoints_", dataset), CSupFrL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("CSupFrR_endpoints_", dataset), CSupFrR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("CMotL_endpoints_", dataset), CMotL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("CMotR_endpoints_", dataset), CMotR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("CSupParL_endpoints_", dataset), CSupParL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("CSupParR_endpoints_", dataset), CSupParR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("CPostParL_endpoints_", dataset), CPostParL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("CPostParR_endpoints_", dataset), CPostParR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("CTempL_endpoints_", dataset), CTempL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("CTempR_endpoints_", dataset), CTempR_endpoints, envir = .GlobalEnv))
  
  invisible(assign(paste0("COccL_endpoints_", dataset), COccL_endpoints, envir = .GlobalEnv))
  invisible(assign(paste0("COccR_endpoints_", dataset), COccR_endpoints, envir = .GlobalEnv))
}



arrange_endpoint_plots <- function(lh_plots, rh_plots) {
  # arrange maps together  
  COrb_plots <- ggarrange(lh_plots$COrbL, rh_plots$COrbR, ncol=1, nrow = 2)
  COrb_final <- annotate_figure(COrb_plots, top = text_grob("Callosum Orbital", 
                                                            color = "black", size = 20))
  
  CAntFr_plots <- ggarrange(lh_plots$CAntFrL, rh_plots$CAntFrR, ncol=1, nrow = 2)
  CAntFr_final <- annotate_figure(CAntFr_plots, top = text_grob("Callosum Anterior Frontal", 
                                                                color = "black", size = 20))
  
  CSupFr_plots <- ggarrange(lh_plots$CSupFrL, rh_plots$CSupFrR, ncol=1, nrow = 2)
  CSupFr_final <- annotate_figure(CSupFr_plots, top = text_grob("Callosum Superior Frontal", 
                                                                color = "black", size = 20))
  
  CMot_plots <- ggarrange(lh_plots$CMotL, rh_plots$CMotR, ncol=1, nrow = 2)
  CMot_final <- annotate_figure(CMot_plots, top = text_grob("Callosum Motor", 
                                                            color = "black", size = 20))
  
  CSupPar_plots <- ggarrange(lh_plots$CSupParL, rh_plots$CSupParR, ncol=1, nrow = 2)
  CSupPar_final <- annotate_figure(CSupPar_plots, top = text_grob("Callosum Superior Parietal", 
                                                                  color = "black", size = 20))
  
  CPostPar_plots <- ggarrange(lh_plots$CPostParL, rh_plots$CPostParR, ncol=1, nrow = 2)
  CPostPar_final <- annotate_figure(CPostPar_plots, top = text_grob("Callosum Posterior Parietal", 
                                                                    color = "black", size = 20))
  
  CTemp_plots <- ggarrange(lh_plots$CTempL, rh_plots$CTempR, ncol=1, nrow = 2)
  CTemp_final <- annotate_figure(CTemp_plots, top = text_grob("Callosum Temporal", 
                                                              color = "black", size = 20))
  
  
  COcc_plots <- ggarrange(lh_plots$COccL, rh_plots$COccR, ncol=1, nrow = 2)
  COcc_final <- annotate_figure(COcc_plots, top = text_grob("Callosum Occipital", 
                                                            color = "black", size = 20))
  
  ARC_plots <- ggarrange(lh_plots$ARCL, rh_plots$ARCR, ncol=1, nrow = 2)
  ARC_final <- annotate_figure(ARC_plots, top = text_grob("Arcuate Fasciculus", 
                                                          color = "black", size = 20))
  
  
  CST_plots <- ggarrange(lh_plots$CSTL, rh_plots$CSTR, ncol=1, nrow = 2)
  CST_final <- annotate_figure(CST_plots, top = text_grob("Corticospinal Tract", 
                                                          color = "black", size = 20))
  
  IFO_plots <- ggarrange(lh_plots$IFOL, rh_plots$IFOR, ncol=1, nrow = 2)
  IFO_final <- annotate_figure(IFO_plots, top = text_grob("Inferior Fronto-occipital Fasciculus", 
                                                          color = "black", size = 20))
  
  ILF_plots <- ggarrange(lh_plots$ILFL, rh_plots$ILFR, ncol=1, nrow = 2)
  ILF_final <- annotate_figure(ILF_plots, top = text_grob("Inferior Longitudinal Fasciculus", 
                                                          color = "black", size = 20))
  
  pARC_plots <- ggarrange(lh_plots$pARCL, rh_plots$pARCR, ncol=1, nrow = 2)
  pARC_final <- annotate_figure(pARC_plots, top = text_grob("Posterior Arcuate Fasciculus", 
                                                            color = "black", size = 20))
  
  SLF_plots <- ggarrange(lh_plots$SLFL, rh_plots$SLFR, ncol=1, nrow = 2)
  SLF_final <- annotate_figure(SLF_plots, top = text_grob("Superior Longitudinal Fasciculus", 
                                                          color = "black", size = 20))
  
  UNC_plots <- ggarrange(lh_plots$UNCL, rh_plots$UNCR, ncol=1, nrow = 2)
  UNC_final <- annotate_figure(UNC_plots, top = text_grob("Uncinate Fasciculus", 
                                                          color = "black", size = 20))
  
  VOF_plots <- ggarrange(lh_plots$VOFL, rh_plots$VOFR, ncol=1, nrow = 2)
  VOF_final <- annotate_figure(VOF_plots, top = text_grob("Vertical Occipital Fasciculus", 
                                                          color = "black", size = 20))
  
  all_plots <- ggarrange(COrb_final, CAntFr_final, CSupFr_final, CMot_final, CSupPar_final, CPostPar_final, CTemp_final, COcc_final, ARC_final, CST_final, IFO_final, ILF_final, pARC_final, SLF_final, UNC_final, VOF_final, ncol = 4, nrow = 4)
  
  # stitch it all together
  legend <- get_legend(lh_plots$VOFL + theme(legend.position = "bottom",
                                             legend.key.height = unit(1.5, 'cm'),
                                             legend.key.width = unit(3.5, 'cm'),
                                             legend.margin=margin(-1,0,-1,0),
                                             legend.text = element_text(size=28),
                                             legend.title = element_blank()))
  
  final_plot <- plot_grid(all_plots, ncol = 1)
  return(final_plot)
}

