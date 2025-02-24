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
