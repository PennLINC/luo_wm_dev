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

######################################
# Figures 2 and 3
######################################
format_ageeffect <- function(df) {
  df <- get(df)
  # add tractID, tract_label, and nodeID
  df <- df %>% mutate(hemi = ifelse(grepl("Left", tract_node), "Left", 
                                    ifelse(grepl("Right", tract_node), "Right", NA))) %>%
    mutate(tractID = gsub("_[0-9]+", "", tract_node)) %>%
    mutate(tract_label = gsub("Left |Right ", "", gsub("_", " ", gsub("Fronto.", "Fronto-", tractID)))) %>%
    mutate(nodeID = str_extract(tract_node, "[0-9]+")) 
  df$nodeID <- as.numeric(df$nodeID)
  
  # label main orientation
  df <- df %>% 
    mutate(main_orientation = case_when(
      tract_label %in% c("Inferior Fronto-occipital",
                         "Inferior Longitudinal", "Superior Longitudinal") ~ "AP",
      tract_label %in% c("Arcuate") ~ "AP_frontal_temporal",
      tract_label %in% c("Callosum Anterior Frontal", "Callosum Motor", "Callosum Occipital", "Callosum Orbital", 
                         "Callosum Posterior Parietal", "Callosum Superior Frontal", "Callosum Superior Parietal", "Callosum Temporal") ~ "RL",
      tract_label %in% c("Corticospinal", "Posterior Arcuate","Vertical Occipital") ~ "SI",
      TRUE ~ NA_character_
    ))
  df$main_orientation <- as.factor(df$main_orientation)
  df <- df %>% relocate(tract_label, tractID, nodeID, tract_node, hemi, main_orientation)
  return(df)
}


# function for calculating number of significant nodes (FDR corrected)
# @param df A dataframe with GAM results
sig_nodes <- function(df) {
  
  df$Anova.age.pvalue.fdr <- p.adjust(df$Anova.smooth.pvalue, method=c("fdr"))
  cat(sprintf("There are %s/%s significant nodes\n", sum(df$Anova.age.pvalue.fdr < 0.05, na.rm=TRUE), nrow(df)))
  df$significant.fdr <- df$Anova.age.pvalue.fdr < 0.05
  df$significant.fdr[df$significant.fdr == TRUE] <- 1
  df$significant.fdr[df$significant.fdr == FALSE] <- 0
  return(df)
}


 
# functions for arranging different tracts into 1 big plot
## callosum
arrange_callosum_plots <- function(list_figures) {
  # row 1
  callosum_orbital <- plot_grid(ggdraw() + draw_label("Callosum Orbital", size = 20, y = 0.2, hjust = 0.5),
                                plot_grid(plotlist = list(ageeffects_plots$`Callosum Orbital`, lollipop_plots$`Callosum Orbital`),
                                          align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  callosum_anterior_frontal <- plot_grid(ggdraw() + draw_label("Callosum Anterior Frontal", size = 20, y = 0.2, hjust = 0.5),
                                         plot_grid(plotlist = list(ageeffects_plots$`Callosum Anterior Frontal`, lollipop_plots$`Callosum Anterior Frontal`),
                                                   align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  callosum_superior_frontal <- plot_grid(ggdraw() + draw_label("Callosum Superior Frontal", size = 20, y = 0.2, hjust = 0.5), 
                                         plot_grid(plotlist = list(ageeffects_plots$`Callosum Superior Frontal`, lollipop_plots$`Callosum Superior Frontal`),
                                                   align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  callosum_motor <- plot_grid(ggdraw() + draw_label("Callosum Motor", size = 20, y = 0.2, hjust = 0.5),
                              plot_grid(plotlist = list(ageeffects_plots$`Callosum Motor`, lollipop_plots$`Callosum Motor`), align = "h", ncol = 2, rel_widths = c(1, 0.4)),
                              ncol = 1, rel_heights = c(0.25, 1))
  callosum1_plots <- plot_grid(ggdraw() + draw_label(expression("Magnitude of Age Effect (" * Delta * " Adjusted " * R^2 * ")"), size = 20, vjust = 0.2, hjust = 0.55, angle = 90), 
                               plot_grid(callosum_orbital, callosum_anterior_frontal, callosum_superior_frontal, callosum_motor, ncol = 4), rel_widths = c(0.02, 1))
  
  # row 2
  callosum_superior_parietal <- plot_grid(ggdraw() + draw_label("Callosum Superior Parietal", size = 20, y = 0.2, hjust = 0.5),
                                          plot_grid(plotlist = list(ageeffects_plots$`Callosum Superior Parietal`, lollipop_plots$`Callosum Superior Parietal`),
                                                    align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  callosum_posterior_parietal <- plot_grid(ggdraw() + draw_label("Callosum Posterior Parietal", size = 20, y = 0.2, hjust = 0.5),
                                           plot_grid(plotlist = list(ageeffects_plots$`Callosum Posterior Parietal`, lollipop_plots$`Callosum Posterior Parietal`),
                                                     align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  callosum_temporal <- plot_grid(ggdraw() + draw_label("Callosum Temporal", size = 20, y = 0.2, hjust = 0.5), 
                                 plot_grid(plotlist = list(ageeffects_plots$`Callosum Temporal`, lollipop_plots$`Callosum Temporal`),
                                           align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  callosum_occipital <- plot_grid(ggdraw() + draw_label("Callosum Occipital", size = 20, y = 0.2, hjust = 0.5),
                                  plot_grid(plotlist = list(ageeffects_plots$`Callosum Occipital`, lollipop_plots$`Callosum Occipital`), 
                                            align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  callosum2_plots <- plot_grid(ggdraw() + draw_label(expression("Magnitude of Age Effect (" * Delta * " Adjusted " * R^2 * ")"), size = 20, vjust = 0.5, hjust = 0.55, angle = 90),
                               plot_grid(callosum_superior_parietal, callosum_posterior_parietal, callosum_temporal, callosum_occipital, ncol = 4), rel_widths = c(0.02, 1))
  
  # arrange all temporarily
  callosum_plots <- ggarrange(callosum1_plots, callosum2_plots, nrow = 2)
  
  # get legend
  lollipop_legend_df <- lollipop_data %>% filter(tract_label == "Callosum Occipital")
  legend_plot <- ggplot(lollipop_legend_df) + geom_segment(aes(x = Dataset, xend = Dataset, y = Superficial - 0.02, yend = Deep + 0.02),
                                                           color = "black", size = 1, position = position_dodge(width = 0.1), alpha = 0.8) +
    geom_point(aes(x = Dataset, y = Superficial, fill = Dataset, color = Dataset, shape = "Superficial"),
               size = 7.5, stroke = 2, alpha = 0.9,
               position = position_dodge(width = 0.1)) +
    geom_point(aes(x = Dataset, y = Deep, color = Dataset, shape = "Deep"), 
               size = 7.5, stroke = 2, alpha = 0.9,
               position = position_dodge(width = 0.1)) +
    scale_shape_manual(values = c("Superficial" = 19, "Deep" = 1)) + scale_fill_manual(values = colors) + scale_color_manual(values = colors) +
    geom_text(aes(x = Dataset, y = Superficial + 0.01, label = significance_star),
              color = "black", size = 10, vjust = 0,
              position = position_dodge(width = 0.2)) +
    labs(x = "", fill = "Dataset", shape = "Tract Region") +
    theme_classic() + 
    ylim(0, 0.41) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 20),
          legend.box = "vertical",
          text = element_text(color = "black"),      
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(0, 1, 0, -0.5), "cm"))
  legend <- get_legend(legend_plot)
  
  # final
  callosum_plot_final <- ggarrange(callosum_plots, legend, nrow=2, heights = c(2, 0.5)) + bgcolor("white") 
  return(callosum_plot_final)
}

## association bundles
arrange_association_plots <- function(list_figures) {
  # row 1
  arcuate <- plot_grid(ggdraw() + draw_label("Arcuate", size = 18, y = 0.2, hjust = 0.5),
                       plot_grid(plotlist = list(ageeffects_plots$Arcuate, lollipop_plots$Arcuate),
                                 align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  inferior_frontooccipital <- plot_grid(ggdraw() + draw_label("Inferior Fronto-occipital", size = 18, y = 0.2, hjust = 0.5),
                                        plot_grid(plotlist = list(ageeffects_plots$`Inferior Fronto-occipital`, lollipop_plots$`Inferior Fronto-occipital`),
                                                  align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  inferior_longitudinal <- plot_grid(ggdraw() + draw_label("Inferior Longitudinal", size = 18, y = 0.2, hjust = 0.5), 
                                     plot_grid(plotlist = list(ageeffects_plots$`Inferior Longitudinal`, lollipop_plots$`Inferior Longitudinal`),
                                               align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  superior_longitudinal <- plot_grid(ggdraw() + draw_label("Superior Longitudinal", size = 18, y = 0.2, hjust = 0.5),
                                     plot_grid(plotlist = list(ageeffects_plots$`Superior Longitudinal`, lollipop_plots$`Superior Longitudinal`), align = "h", ncol = 2, rel_widths = c(1, 0.4)),
                                     ncol = 1, rel_heights = c(0.25, 1))
  y_label <- ggdraw() + draw_label(expression("Magnitude of Age Effect (" * Delta * " Adjusted " * R^2 * ")"), 
                                   size = 18, vjust = 0.5, hjust = 0.55, angle = 90) 
  title1_label <- ggdraw() + draw_label("Anterior - Posterior", 
                                        size = 18, vjust = 2.5, hjust = 0.5, fontface = "bold")
  association1_plots <- plot_grid(arcuate, inferior_frontooccipital, inferior_longitudinal, superior_longitudinal, ncol = 4)
  association1_plots <- plot_grid(title1_label, plot_grid(y_label, association1_plots, rel_widths = c(0.02, 1), ncol = 2), ncol = 1, rel_heights = c(0.005, 1))
  
  # row 2
  posterior_arcuate <- plot_grid(ggdraw() + draw_label("Posterior Arcuate", size = 18, y = 0.2, hjust = 0.5),
                                 plot_grid(plotlist = list(ageeffects_plots$`Posterior Arcuate`, lollipop_plots$`Posterior Arcuate`),
                                           align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  uncinate <- plot_grid(ggdraw() + draw_label("Uncinate", size = 18, y = 0.2, hjust = 0.5), 
                        plot_grid(plotlist = list(ageeffects_plots$Uncinate, lollipop_plots$Uncinate),
                                  align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  vertical_occipital <- plot_grid(ggdraw() + draw_label("Vertical Occipital", size = 18, y = 0.2, hjust = 0.5),
                                  plot_grid(plotlist = list(ageeffects_plots$`Vertical Occipital`, lollipop_plots$`Vertical Occipital`),
                                            align = "h", ncol = 2, rel_widths = c(1, 0.4)), ncol = 1, rel_heights = c(0.25, 1))
  title2_label <- ggdraw() + draw_label("Superior - Inferior", 
                                        size = 18, vjust = 2.5, hjust = 0.5, fontface = "bold")
  blank_plot <- ggdraw() + theme_void()
  association2_plots <- plot_grid(uncinate, posterior_arcuate, vertical_occipital, blank_plot, ncol = 4)
  association2_plots <- plot_grid(title2_label, plot_grid(y_label, association2_plots, rel_widths = c(0.02, 1), ncol = 2), ncol = 1, rel_heights = c(0.005, 1))
  
  # get legends
  lollipop_legend_df <- lollipop_data %>% filter(tract_label == "Arcuate")
  legend_plot <- ggplot(lollipop_legend_df) + geom_segment(aes(x = Dataset, xend = Dataset, y = Superficial - 0.02, yend = Deep + 0.02),
                                                           color = "black", size = 1, position = position_dodge(width = 0.1), alpha = 0.8) +
    geom_point(aes(x = Dataset, y = Superficial, fill = Dataset, color = Dataset, shape = "Superficial"),
               size = 7.5, stroke = 2, alpha = 0.9,
               position = position_dodge(width = 0.1)) +
    geom_point(aes(x = Dataset, y = Deep, color = Dataset, shape = "Deep"), 
               size = 7.5, stroke = 2, alpha = 0.9,
               position = position_dodge(width = 0.1)) +
    scale_shape_manual(values = c("Superficial" = 19, "Deep" = 1)) + scale_fill_manual(values = colors) + scale_color_manual(values = colors) +
    geom_text(aes(x = Dataset, y = Superficial + 0.01, label = significance_star),
              color = "black", size = 10, vjust = 0,
              position = position_dodge(width = 0.2)) +
    labs(x = "", fill = "Dataset", shape = "Tract Region") +
    theme_classic() + 
    ylim(0, 0.41) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 18),
          legend.box = "vertical",
          text = element_text(color = "black"),        
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(0, 1, 0, -0.5), "cm"))
  legend_lollipop <- get_legend(legend_plot)
  
  legend_plot2 <- plot_age_effect("Arcuate", scalar = "dti_md", ageeffect_measure = ageeffect_measure, 
                                  color_HCPD = color_HCPD, color_HBN = color_HBN, color_PNC = color_PNC, clipEnds = 5, ylim2 = 0.41, ylim1 = 0, fontsize = 18,
                                  legend_position = "bottom") + theme(legend.position = "bottom",
                                                                      legend.title = element_text(size = 18, face = "bold"),
                                                                      legend.text = element_text(size = 18),
                                                                      legend.box = "vertical",
                                                                      text = element_text(color = "black"),        
                                                                      axis.text.x = element_blank(),
                                                                      axis.text.y = element_blank(),
                                                                      axis.title.x = element_text(size = 18),
                                                                      axis.title.y = element_blank(),
                                                                      axis.line.y = element_blank(),
                                                                      axis.line.x = element_blank(),
                                                                      axis.ticks.x = element_blank(),
                                                                      axis.ticks.y = element_blank(), 
                                                                      plot.margin = unit(c(0, 1, 0, -0.5), "cm"))
  legend_ageeffect <- get_legend(legend_plot2)
  
  # final
  association_plots_final <- ggarrange(association1_plots, association2_plots, nrow=2)
  association_plots_final <- ggarrange(association_plots_final, legend_lollipop, legend_ageeffect, nrow=3, heights = c(2, 0.3, 0.3)) + bgcolor("white") 
  return(association_plots_final) 
}


# functions for formatting data for glass brain
## compute average age effect
compute_avg_ageeffect <- function(tract, scalar, clipEnds) {
  
  HCPD_dev <- ageeffect.fdr_dfs$HCPD_ageeffects %>% filter(tractID == tract) %>% mutate(HCPD_dev = abs(get(ageeffect_measure))) %>% select(HCPD_dev, nodeID) %>%
    group_by(nodeID) %>% summarize(HCPD_dev = mean(HCPD_dev))
  HBN_dev <- ageeffect.fdr_dfs$HBN_ageeffects %>% filter(tractID == tract) %>% mutate(HBN_dev = abs(get(ageeffect_measure))) %>% select(HBN_dev, nodeID) %>%
    group_by(nodeID) %>% summarize(HBN_dev = mean(HBN_dev))
  PNC_dev <- ageeffect.fdr_dfs$PNC_ageeffects %>% filter(tractID == tract) %>% mutate(PNC_dev = abs(get(ageeffect_measure))) %>% select(PNC_dev, nodeID) %>%
    group_by(nodeID) %>% summarize(PNC_dev = mean(PNC_dev))
  
  
  avg_dev <- HCPD_dev %>%
    inner_join(HBN_dev, by = "nodeID") %>%
    inner_join(PNC_dev, by = "nodeID")
  
  mean_dev <- avg_dev %>% group_by(nodeID) %>% summarise(mean_scalar = (HCPD_dev + HBN_dev + PNC_dev) /3)
  mean_dev <- mean_dev %>%
    mutate(mean_scalar = case_when(
      nodeID < clipEnds | nodeID > (99-clipEnds) ~ NA_real_,  # set NA in mean_scalar for clipped nodes 
      TRUE ~ mean_scalar  
    ))
  #mean_dev <- mean_dev %>% filter(nodeID >= clipEnds & nodeID < (100 - clipEnds))
  return(mean_dev)
  
}

# normalize age effects for plotting with fury/python
normalize_dev <- function(avgs_df, min_val, max_val) {
  for(i in 1:length(avgs_df)) {
    avgs_df[[i]] <- avgs_df[[i]] %>% mutate(normalized_values = (mean_scalar - min_val) / (max_val - min_val))
  }
  return(avgs_df)
}


# function for plotting age effect
plot_age_effect <- function(tract, scalar, ageeffect_measure, color_HCPD, color_HBN, color_PNC, clipEnds, ylim1, ylim2, fontsize, legend_position = "none") {
  # ageeffect_measure = GAM.smooth.AdjRsq or GAM.smooth.partialR2
  HCPD <- ageeffect.fdr_dfs$HCPD_ageeffects %>% filter(tract_label == tract) %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1)) %>%
    mutate(Dataset = "HCPD")
  HBN <- ageeffect.fdr_dfs$HBN_ageeffects %>% filter(tract_label == tract) %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1)) %>%
    mutate(Dataset = "HBN")
  PNC <- ageeffect.fdr_dfs$PNC_ageeffects %>% filter(tract_label == tract) %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1)) %>%
    mutate(Dataset = "PNC")
  
  # NA out color/fill aes if adj rsq = 0 or if p-value doesn't survive FDR correction (makes the color gray)
  if (ageeffect_measure == "GAM.smooth.AdjRsq" | ageeffect_measure == "GAM.smooth.partialR2") {
    includes_zero_HCPD <- which(HCPD[[ageeffect_measure]]==0 | HCPD$Anova.age.pvalue.fdr > 0.05)
    includes_zero_HBN <- which(HBN[[ageeffect_measure]]==0 | HBN$Anova.age.pvalue.fdr > 0.05)
    includes_zero_PNC <- which(PNC[[ageeffect_measure]]==0 | PNC$Anova.age.pvalue.fdr > 0.05)
  } else {
    includes_zero_HCPD <- which(is.na(HCPD[[ageeffect_measure]]) | HCPD[[ageeffect_measure]]==0 | HCPD$Anova.age.pvalue.fdr > 0.05)
    includes_zero_HBN <- which(is.na(HBN[[ageeffect_measure]]) | HBN[[ageeffect_measure]]==0 | HBN$Anova.age.pvalue.fdr > 0.05)
    includes_zero_PNC <- which(is.na(PNC[[ageeffect_measure]]) | PNC[[ageeffect_measure]]==0 | PNC$Anova.age.pvalue.fdr > 0.05)
  }
  
  HCPD$Dataset[includes_zero_HCPD] <- NA
  HBN$Dataset[includes_zero_HBN] <- NA
  PNC$Dataset[includes_zero_PNC] <- NA
  
  # make df to plot
  df <- rbind(HCPD, HBN, PNC)
  
  mean_data <- df %>%
    group_by(nodeID) %>%
    summarize(mean_value = mean(abs(get(ageeffect_measure))))
  
  if(str_detect(tract, "Callosum")) {
    plot <- ggplot(data = df, aes(x = nodeID, y = abs(get(ageeffect_measure)), colour = Dataset, fill = Dataset)) +
      geom_point(size=2, alpha = 0.25) + 
      geom_smooth(data = mean_data, aes(x = nodeID, y = mean_value), inherit.aes = FALSE, 
                  method = "loess", span = 0.4, se = FALSE, color = "black", size = 1) + 
      scale_colour_manual(values = c("HCPD" = color_HCPD, "HBN" = color_HBN, "PNC" = color_PNC), na.value = "grey50") +
      scale_fill_manual(values = c("HCPD" = color_HCPD, "HBN" = color_HBN, "PNC" = color_PNC), na.value = "grey50") + 
      theme_classic() + 
      scale_x_continuous(
        breaks = c(10, 50, 90),                 
        labels = c("Left", "Deep", "Right")  
      ) +
      theme(legend.position = legend_position,
            legend.text = element_text(size = fontsize),
            legend.title = element_text(size = fontsize),
            legend.box = "vertical",
            axis.line = element_line(color = "black"),
            axis.text.x = element_text(size = fontsize, color = "black"),
            axis.text.y = element_text(size = fontsize, color = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size=fontsize, hjust=0.5),
            plot.margin = unit(c(0, 1, 0.2, 0.2), "cm")) + ylim(ylim1, ylim2) +
      guides(shape = guide_legend("Hemi", override.aes = list(alpha = 1, size = 6)),  # Ensure alpha = 1 in legend for shape
             colour = guide_legend(override.aes = list(alpha = 1, size = 6)))
  } else {
    
    if(tract %in% c("Posterior Arcuate", "Vertical Occipital")) {
      x_scale <- scale_x_continuous(breaks = c(15, 52, 86), labels = c("Superior", "Deep", "Inferior"))
    } else if(tract %in% c("Corticospinal")) {
      x_scale <- scale_x_continuous(breaks = c(15, 75), labels = c("Superior", "Deep"))
    } else {
      x_scale <- scale_x_continuous(breaks = c(15, 49, 86), labels = c("Anterior", "Deep", "Posterior")) 
    }
    
    if(tract == "Inferior Fronto-occipital") {
      span <- 0.5
    } else {
      span <- 0.6
    }
    plot <- ggplot(data = df, aes(x = nodeID, y = abs(get(ageeffect_measure)), colour = Dataset, 
                                  fill = Dataset, shape = hemi)) + 
      geom_point(data = subset(df, hemi == "Right"), alpha = 0.35, size = 3, stroke = 0) +   
      geom_point(data = subset(df, hemi == "Left"), alpha = 0.25, size = 2) +
      geom_smooth(data = mean_data, aes(x = nodeID, y = mean_value), inherit.aes = FALSE, 
                  method = "loess", span = span, se = FALSE, color = "black", size = 1) + 
      scale_colour_manual(values = c("HCPD" = color_HCPD, "HBN" = color_HBN, "PNC" = color_PNC), na.value = "grey50") +
      scale_fill_manual(values = c("HCPD" = color_HCPD, "HBN" = color_HBN, "PNC" = color_PNC), na.value = "grey50") +
      scale_shape_manual(values = c("Right" = 23, "Left" = 5)) + 
      theme_classic() + 
      x_scale + 
      theme(legend.position = legend_position,
            legend.text = element_text(size = fontsize),
            legend.title = element_text(size = fontsize),
            legend.box = "vertical",
            axis.line = element_line(color = "black"),
            axis.text.x = element_text(size = fontsize, color = "black"),
            axis.text.y = element_text(size = fontsize, color = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size=fontsize, hjust=0.5),
            plot.margin = unit(c(0, 1, 0.2, 0.2), "cm")) + ylim(ylim1, ylim2) +
      guides(shape = guide_legend("Hemi", override.aes = list(alpha = 1, size = 6, stroke = 2, fill = "black")), color = "none", fill = "none")
  }
  return(plot)
}

 


# functions for lollipop plots
## identify deep and Superficial nodes
format_deep_superficial <- function(df, ageeffect_measure, bin_num_nodes, clipEnds) {
  sqrt_n <- sqrt(bin_num_nodes) # n = number of nodes included in each bin (or level of node_position)
  df <- df %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1))
  df_binned <- df %>%
    filter(
      nodeID < (bin_num_nodes + clipEnds) |  # Start of the tract
        nodeID > (99 - bin_num_nodes - clipEnds) |  # End of the tract
        (nodeID > (50-bin_num_nodes) & nodeID <= (50+bin_num_nodes))  # Deep WM nodes: the  nodes in the center
    ) %>%
    mutate(
      node_position = case_when(
        tract_label == "Corticospinal" & nodeID < (bin_num_nodes + clipEnds) ~ "Superficial",
        tract_label == "Corticospinal" & nodeID > (99 - bin_num_nodes - clipEnds) ~ "Deep",
        tract_label != "Corticospinal" & nodeID < (bin_num_nodes + clipEnds) ~ "Superficial",
        tract_label != "Corticospinal" & nodeID > (99 - bin_num_nodes - clipEnds) ~ "Superficial",
        tract_label != "Corticospinal" & (nodeID > (50-bin_num_nodes) & nodeID <= (50+bin_num_nodes)) ~ "Deep",
        TRUE ~ NA_character_
      )
    ) %>%
    #group_by(tract_label, node_position) %>%
    group_by(tract_label, node_position, Dataset) %>% # removed hemi
    summarise(
      mean_ageeffect = abs(mean(get(ageeffect_measure), na.rm = TRUE)),
      sd = abs(sd(get(ageeffect_measure), na.rm = TRUE)),
      ymin_sd = abs(mean(get(ageeffect_measure), na.rm = TRUE)) - sd,
      ymax_sd = abs(mean(get(ageeffect_measure), na.rm = TRUE)) + sd, 
      se = sd(get(ageeffect_measure), na.rm = TRUE) / sqrt_n,
      ymin_se = abs(mean(get(ageeffect_measure), na.rm = TRUE)) - se,
      ymax_se = abs(mean(get(ageeffect_measure), na.rm = TRUE)) + se
    )
  df_binned$node_position <- factor(df_binned$node_position, levels = c("Superficial", "Deep"))
  
  return(df_binned)
}


## prepare and format data for lollipop plots
prepare_lollipop_data <- function(df_formatted) {
  df_formatted$tract_label <- as.factor(df_formatted$tract_label)
  df_formatted <- df_formatted %>%
    pivot_wider(id_cols = c(tract_label, Dataset), names_from = node_position, values_from = mean_ageeffect) %>%
    
    mutate(
      tract_numeric = as.numeric(tract_label),
      tract_node_dodged = as.numeric(Dataset) * 0.001 - 0.001
    ) %>%
    arrange(tract_label)
  
  lollipop_data <- left_join(df_formatted, NEST, by = c("tract_label", "Dataset")) 
  lollipop_data$pval_fdr_all <- as.numeric(lollipop_data$pval_fdr_all)
  lollipop_data <- lollipop_data %>% 
    mutate(significance_star = case_when(
      round(pval_fdr_all, 4) <= 0.0001 ~ "**",
      round(pval_fdr_all, 4) <= 0.05 ~ "*",
      TRUE ~ "",
    ))
  return(lollipop_data)
}

## plot lollipops!
make_lollipop_plot <- function(tract, data) {
  df <- data %>% filter(tract_label == tract)
  plot <- ggplot(df) + geom_segment(aes(x = Dataset, xend = Dataset, y = Superficial - 0.018, yend = Deep + 0.018),
                                    color = "black", size = 1, position = position_dodge(width = 0.1), alpha = 0.8) +
    geom_point(aes(x = Dataset, y = Superficial, fill = Dataset, color = Dataset, shape = "Superficial"),
               size = 7.5, stroke = 2, alpha = 0.9,
               position = position_dodge(width = 0.1)) +
    geom_point(aes(x = Dataset, y = Deep, color = Dataset, shape = "Deep"), 
               size = 7.5, stroke = 2, alpha = 0.9,
               position = position_dodge(width = 0.1)) +
    scale_shape_manual(values = c("Superficial" = 19, "Deep" = 1)) + scale_fill_manual(values = colors) + scale_color_manual(values = colors) +
    geom_text(aes(x = Dataset, y = Superficial + 0.02, label = significance_star),
              color = "black", size = 10, vjust = 0,
              position = position_dodge(width = 0.2)) +
    labs(x = "", fill = "Dataset", shape = "Tract Region") +
    theme_classic() + 
    ylim(0, 0.41) +
    theme(legend.position = "none",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.box = "vertical",
          text = element_text(color = "black"),      
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(0, 1, 0, -0.5), "cm"))
  return(plot)
}



######################################
# Figures 4 and 5
######################################

# this function loads each dataset-averaged tract-to-region map
## @param depth, A string
## @param dataset, A string
load_maps <- function(depth, dataset, age_group = NULL) {
  
  config_file <- sprintf("%1$s/code/config/config_%2$s.json", proj_root, dataset)
  config <- fromJSON(paste(readLines(config_file), collapse=""))
  
  vol_to_surf_dir <- paste0(config$data_root, "/derivatives/vol_to_surf")
  group_dir <- paste0(vol_to_surf_dir, "/group")
  
  # load glasser maps for each tract... load for a specific depth
  if (is.null(age_group)) {
    pattern <- paste0("^(Right|Left).*", depth, ".*\\.csv$")
    glasser_csvs <- list.files(path = group_dir, pattern = pattern, full.names = TRUE)
    tract_names <- lapply(glasser_csvs, function(path) {
      filename <- basename(path)
      sub("_\\d+\\.\\d+_glasser\\.csv$", "", filename)
    })
    tract_names <- unlist(tract_names)
    
  } else {
    glasser_csvs <- list.files(path = group_dir, pattern = paste0(age_group, "_.*\\.csv$"), full.names = TRUE)
    
    tract_names <- lapply(glasser_csvs, function(path) {
      filename <- basename(path)
      filename <- sub(paste0("^", age_group, "_"), "", filename)  # remove "youngest_" or "oldest_"
      sub("_\\d+\\.\\d+_glasser\\.csv$", "", filename)  # remove trailing numeric and suffix
    })
    
    tract_names <- unlist(tract_names)
  }
 
  glasser_maps <- lapply(glasser_csvs, read.csv)
  names(glasser_maps) <- tract_names
  
  # merge labels with my maps
  glasser_maps <- lapply(glasser_maps, merge, y = glasser_labels, by = "regionID")
  
  # separate lh and rh maps
  lh_idx <- grep("Left", names(glasser_maps))
  lh_maps <- glasser_maps[lh_idx]
  invisible(assign(paste0("lh_maps_", dataset), lh_maps, envir = .GlobalEnv))
  
  rh_idx <- grep("Right", names(glasser_maps))
  rh_maps <- glasser_maps[rh_idx]
  invisible(assign(paste0("rh_maps_", dataset), rh_maps, envir = .GlobalEnv))
  return(invisible(NULL)) # prevents dataframes from being printed when this function is called by lapply
  
}

# compute tract-end developmental effects (average of age effects at tract ends)
average_tractend_deveffect <- function(df, ageeffect_measure, bin_num_nodes, clipEnds) {
  sqrt_n <- sqrt(bin_num_nodes) # n = number of nodes included in each bin (or level of node_position)
  df <- df %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1))
  df_binned <- df %>%
    filter(
      nodeID < (bin_num_nodes + clipEnds) |  # Start of the tract
        nodeID > (99 - bin_num_nodes - clipEnds) # End of the tract
    ) %>%
    mutate(
      node_position = case_when(
        tract_label == "Corticospinal" & nodeID < (bin_num_nodes + clipEnds) ~ "end1",
        tract_label == "Corticospinal" & nodeID > (99 - bin_num_nodes - clipEnds) ~ "end2",
        tract_label != "Corticospinal" & nodeID < (bin_num_nodes + clipEnds) ~ "end1",
        tract_label != "Corticospinal" & nodeID > (99 - bin_num_nodes - clipEnds) ~ "end2", 
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(tractID, node_position) %>% # removed hemi
    summarise(
      mean_ageeffect = abs(mean(get(ageeffect_measure), na.rm = TRUE)),
      sd = abs(sd(get(ageeffect_measure), na.rm = TRUE)),
      ymin_sd = abs(mean(get(ageeffect_measure), na.rm = TRUE)) - sd,
      ymax_sd = abs(mean(get(ageeffect_measure), na.rm = TRUE)) + sd, 
      se = sd(get(ageeffect_measure), na.rm = TRUE) / sqrt_n,
      ymin_se = abs(mean(get(ageeffect_measure), na.rm = TRUE)) - se,
      ymax_se = abs(mean(get(ageeffect_measure), na.rm = TRUE)) + se
    )
  df_binned$node_position <- factor(df_binned$node_position, levels = c("end1", "end2"))
  
  return(df_binned)
}


# make tract to region maps of tract-end developmental effects
## have to assign cortical regions to end1 or end2 manually. There's no consistent way to automate this unfortunately.
make_maps <- function(dataset, bin_num_nodes) {
  
  lh_maps <- get(paste0("lh_maps_", dataset))
  rh_maps <- get(paste0("rh_maps_", dataset))
  deveffects <- get(paste0(dataset, "_deveffects_", bin_num_nodes))
  
  # for tracts with 1 endpoint (CST), extract the age effect for end1 (corresponds to cortical endpoint) 
  CSTL_deveffect <- lh_maps$LeftCorticospinal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Left_Corticospinal" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CSTR_deveffect <- rh_maps$RightCorticospinal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Right_Corticospinal" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  # for tracts with 2 distinct cortical endpoints: ARC, ILF, IFO, SLF
  ARCL_deveffect <- lh_maps$LeftArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = case_when(
        !is.na(thresh_probability) & (str_detect(cortex, "(?i)frontal") | str_detect(cortex, "(?i)motor") | str_detect(cortex, "(?i)Posterior_Opercular")) ~ 
          deveffects %>% filter(tractID == "Left_Arcuate", node_position == "end1") %>% pull(mean_ageeffect),
        !is.na(thresh_probability) & (str_detect(cortex, "(?i)Temp") | str_detect(cortex, "(?i)Auditory") | 
                                        str_detect(cortex, "(?i)Visual")) ~ 
          deveffects %>% filter(tractID == "Left_Arcuate", node_position == "end2") %>% pull(mean_ageeffect),
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      end = ifelse(
        !is.na(thresh_probability) & (str_detect(cortex, "(?i)frontal") | str_detect(cortex, "motor") | str_detect(cortex, "Posterior_Opercular")), 
        "end1", 
        ifelse(
          !is.na(thresh_probability) & (str_detect(cortex, "Temp") | str_detect(cortex, "Auditory") |
                                          str_detect(cortex, "(?i)Visual")), 
          "end2", 
          NA))
    ) %>%
    relocate(mean_age_effect)
  
  ARCR_deveffect <- rh_maps$RightArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = case_when(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal") | str_detect(cortex, "(?i)motor") | str_detect(cortex, "(?i)Posterior_Opercular")) ~ 
          deveffects %>% filter(tractID == "Left_Arcuate", node_position == "end1") %>% pull(mean_ageeffect),
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)Temp") | str_detect(cortex, "(?i)MT") | str_detect(cortex, "(?i)Auditory")) ~ 
          deveffects %>% filter(tractID == "Left_Arcuate", node_position == "end2") %>% pull(mean_ageeffect),
        TRUE ~ NA_real_
      ),
      end = case_when(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal") | str_detect(cortex, "(?i)motor") | str_detect(cortex, "(?i)Posterior_Opercular")) ~ "end1",
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)Temp") | str_detect(cortex, "(?i)MT") | str_detect(cortex, "(?i)Auditory")) ~ "end2",
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(mean_age_effect)
  
  ILFL_deveffect <- lh_maps$LeftInferiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = case_when(
        !is.na(thresh_probability) & 
          !str_detect(cortex, "(?i)visual|Inferior_Parietal|Occipital") ~ 
          deveffects %>% filter(tractID == "Left_Inferior_Longitudinal", node_position == "end1") %>% pull(mean_ageeffect),
        !is.na(thresh_probability) & 
          str_detect(cortex, "(?i)visual|Inferior_Parietal|Occipital") ~ 
          deveffects %>% filter(tractID == "Left_Inferior_Longitudinal", node_position == "end2") %>% pull(mean_ageeffect),
        TRUE ~ NA_real_
      ),
      end = case_when(
        !is.na(thresh_probability) & 
          !str_detect(cortex, "(?i)visual|Inferior_Parietal|Occipital") ~ "end1",
        !is.na(thresh_probability) & 
          str_detect(cortex, "(?i)visual|Inferior_Parietal|Occipital") ~ "end2",
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(mean_age_effect)
  
  
  ILFR_deveffect <- rh_maps$RightInferiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability) & (!str_detect(cortex, "(?i)visual")) & region != "PHA3", 
        deveffects %>%  filter(tractID == "Right_Inferior_Longitudinal", node_position == "end1") %>% pull(mean_ageeffect),
        ifelse(
          !is.na(thresh_probability) & (str_detect(cortex, "(?i)visual")) | region == "PHA3", 
          deveffects %>%  filter(tractID == "Right_Inferior_Longitudinal", node_position == "end2") %>% pull(mean_ageeffect),
          NA))
    ) %>%
    mutate(
      end = ifelse(
        !is.na(thresh_probability) & (!str_detect(cortex, "(?i)visual")) & region != "PHA3", 
        "end1", 
        ifelse(
          !is.na(thresh_probability) & (str_detect(cortex, "(?i)visual")) | region == "PHA3", 
          "end2", 
          NA))
    ) %>%
    relocate(mean_age_effect) %>% arrange(end)
  
  
  IFOL_deveffect <- lh_maps$LeftInferiorFrontooccipital %>%
    mutate(
      thresh_probability = ifelse(probability < 0.1, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(mean_age_effect = case_when(
      !is.na(thresh_probability) & 
        (str_detect(cortex, "(?i)frontal|Auditory")) ~ 
        deveffects %>% filter(tractID == "Left_Inferior_Fronto.occipital", node_position == "end1") %>% pull(mean_ageeffect),
      !is.na(thresh_probability) & 
        (str_detect(cortex, "(?i)visual") | str_detect(cortex, "Pariet")) ~ 
        deveffects %>% filter(tractID == "Left_Inferior_Fronto.occipital", node_position == "end2") %>% pull(mean_ageeffect),
      TRUE ~ NA_real_)) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  IFOR_deveffect <- rh_maps$RightInferiorFrontooccipital %>%
    mutate(
      thresh_probability = ifelse(probability < 0.1, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal")  |
             str_detect(cortex, "Auditory")), 
        deveffects %>% filter(tractID == "Right_Inferior_Fronto.occipital", node_position == "end1") %>% pull(mean_ageeffect),
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)visual") | str_detect(cortex, "Pariet")), 
          deveffects %>% filter(tractID == "Right_Inferior_Fronto.occipital", node_position == "end2") %>% pull(mean_ageeffect),
          NA
        )
      )
    ) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  SLFL_deveffect <- lh_maps$LeftSuperiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal") |
             str_detect(cortex, "(?i)motor") | 
             str_detect(cortex, "(?i)Opercular")), 
        deveffects %>% filter(tractID == "Left_Superior_Longitudinal", node_position == "end1") %>% pull(mean_ageeffect),
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)pariet|(?i)auditory")), 
          deveffects %>% filter(tractID == "Left_Superior_Longitudinal", node_position == "end2") %>% pull(mean_ageeffect),
          NA))) %>%
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
        NA))) %>%
    relocate(mean_age_effect)
  
  SLFR_deveffect <- rh_maps$RightSuperiorLongitudinal %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal") |
             str_detect(cortex, "(?i)motor") | 
             str_detect(cortex, "(?i)Opercular")), 
        deveffects %>% filter(tractID == "Right_Superior_Longitudinal", node_position == "end1") %>% pull(mean_ageeffect),
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)pariet")), 
          deveffects %>% filter(tractID == "Right_Superior_Longitudinal", node_position == "end2") %>% pull(mean_ageeffect),
          NA))) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  # for tracts with 2 cortical endpoints that require some manual work: pARC, VOF 
  pARCL_deveffect <- lh_maps$LeftPosteriorArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)parietal")),  # might exclude TPOJ1 since its between the two endpoints
        deveffects %>% filter(tractID == "Left_Posterior_Arcuate", node_position == "end1") %>% pull(mean_ageeffect),
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)tempor") |
               str_detect(cortex, "(?i)visual") |
               str_detect(cortex, "(?i)auditory")), 
          deveffects %>% filter(tractID == "Left_Posterior_Arcuate", node_position == "end2") %>% pull(mean_ageeffect), 
          NA))) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  pARCR_deveffect <- rh_maps$RightPosteriorArcuate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)parietal")),  # might exclude TPOJ1 since its between the two endpoints
        deveffects %>% filter(tractID == "Right_Posterior_Arcuate", node_position == "end1") %>% pull(mean_ageeffect), 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)tempor") |
               str_detect(cortex, "(?i)visual") |
               str_detect(cortex, "(?i)auditory")), 
          deveffects %>% filter(tractID == "Right_Posterior_Arcuate", node_position == "end2") %>% pull(mean_ageeffect), 
          NA
        )
      )
    ) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  UNCL_deveffect <- lh_maps$LeftUncinate %>% # connects parts of the limbic system in areas in the temporal lobe with inferior portions of the frontal lobe such as the OFC
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal")),   
        deveffects %>% filter(tractID == "Left_Uncinate", node_position == "end1") %>% pull(mean_ageeffect), 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)temporal")), 
          deveffects %>% filter(tractID == "Left_Uncinate", node_position == "end2") %>% pull(mean_ageeffect), 
          NA
        )
      )
    ) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  
  UNCR_deveffect <- rh_maps$RightUncinate %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)frontal")),   
        deveffects %>% filter(tractID == "Right_Uncinate", node_position == "end1") %>% pull(mean_ageeffect),
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)temporal") | 
               str_detect(cortex, "(?i)auditory")), 
          deveffects %>% filter(tractID == "Right_Uncinate", node_position == "end2") %>% pull(mean_ageeffect),
          NA
        )
      )
    ) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  VOFL_deveffect <- lh_maps$LeftVerticalOccipital %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse( 
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
        deveffects %>% filter(tractID == "Left_Vertical_Occipital", node_position == "end1") %>% pull(mean_ageeffect), 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)ventral") | 
               region == "PH" |
               region == "LO2" |
               region == "V3"), 
          deveffects %>% filter(tractID == "Left_Vertical_Occipital", node_position == "end2") %>% pull(mean_ageeffect), 
          NA))) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  
  VOFR_deveffect <- rh_maps$RightVerticalOccipital %>%
    mutate(
      thresh_probability = ifelse(probability < threshold, NA, probability)
    ) %>%
    select(thresh_probability, regionName, region,  cortex) %>%
    arrange(thresh_probability) %>%
    mutate(
      mean_age_effect = ifelse( 
        !is.na(thresh_probability) & 
          (str_detect(cortex, "(?i)dorsal") | 
             str_detect(cortex, "(?i)parietal") |
             region == "LO1" |
             region == "V3CD" |
             region == "V4" | # kind of arbitrarily placing V3 and V4  
             region == "LO3" | 
             region == "TPOJ3"),   
        deveffects %>% filter(tractID == "Right_Vertical_Occipital", node_position == "end1") %>% pull(mean_ageeffect), 
        ifelse(
          !is.na(thresh_probability) & 
            (str_detect(cortex, "(?i)ventral") | 
               region == "PH" |
               region == "LO2" |
               region == "MST" |
               region == "V3" | 
               region == "FST"), 
          deveffects %>% filter(tractID == "Right_Vertical_Occipital", node_position == "end2") %>% pull(mean_ageeffect), NA
        )
      )
    ) %>%
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
    ) %>%
    relocate(mean_age_effect)
  
  
  # callosum bundles
  COrbL_deveffect <- lh_maps$LeftCallosumOrbital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Orbital" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  COrbR_deveffect <- rh_maps$RightCallosumOrbital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Orbital" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  
  CAntFrL_deveffect <- lh_maps$LeftCallosumAnteriorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Anterior_Frontal" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CAntFrR_deveffect <- rh_maps$RightCallosumAnteriorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Anterior_Frontal" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CSupFrL_deveffect <- lh_maps$LeftCallosumSuperiorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Superior_Frontal" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CSupFrR_deveffect <- rh_maps$RightCallosumSuperiorFrontal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Superior_Frontal" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CMotL_deveffect <- lh_maps$LeftCallosumMotor %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Motor" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CMotR_deveffect <- rh_maps$RightCallosumMotor %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Motor" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CSupParL_deveffect <- lh_maps$LeftCallosumSuperiorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Superior_Parietal" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CSupParR_deveffect <- rh_maps$RightCallosumSuperiorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Superior_Parietal" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CPostParL_deveffect <- lh_maps$LeftCallosumPosteriorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Posterior_Parietal" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CPostParR_deveffect <- rh_maps$RightCallosumPosteriorParietal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Posterior_Parietal" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CTempL_deveffect <- lh_maps$LeftCallosumTemporal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Temporal" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  CTempR_deveffect <- rh_maps$RightCallosumTemporal %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Temporal" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  COccL_deveffect <- lh_maps$LeftCallosumOccipital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Occipital" & node_position == "end2") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end2", NA)
    ) %>%
    relocate(mean_age_effect)
  
  COccR_deveffect <- rh_maps$RightCallosumOccipital %>% mutate(
    thresh_probability = ifelse(probability < threshold, NA, probability)
  ) %>% 
    select(thresh_probability, regionName, region, cortex) %>%
    arrange(thresh_probability) %>% 
    mutate(
      mean_age_effect = ifelse(
        !is.na(thresh_probability), 
        deveffects %>% filter(tractID == "Callosum_Occipital" & node_position == "end1") %>% pull(mean_ageeffect), NA)
    ) %>% 
    mutate(
      end = ifelse(
        !is.na(thresh_probability), 
        "end1", NA)
    ) %>%
    relocate(mean_age_effect)
  
  invisible(assign(paste0("CSTL_deveffect_", dataset), CSTL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("CSTR_deveffect_", dataset), CSTR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("ARCL_deveffect_", dataset), ARCL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("ARCR_deveffect_", dataset), ARCR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("IFOL_deveffect_", dataset), IFOL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("IFOR_deveffect_", dataset), IFOR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("ILFL_deveffect_", dataset), ILFL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("ILFR_deveffect_", dataset), ILFR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("pARCL_deveffect_", dataset), pARCL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("pARCR_deveffect_", dataset), pARCR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("SLFL_deveffect_", dataset), SLFL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("SLFR_deveffect_", dataset), SLFR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("UNCL_deveffect_", dataset), UNCL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("UNCR_deveffect_", dataset), UNCR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("VOFL_deveffect_", dataset), VOFL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("VOFR_deveffect_", dataset), VOFR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("COrbL_deveffect_", dataset), COrbL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("COrbR_deveffect_", dataset), COrbR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("CAntFrL_deveffect_", dataset), CAntFrL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("CAntFrR_deveffect_", dataset), CAntFrR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("CSupFrL_deveffect_", dataset), CSupFrL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("CSupFrR_deveffect_", dataset), CSupFrR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("CMotL_deveffect_", dataset), CMotL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("CMotR_deveffect_", dataset), CMotR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("CSupParL_deveffect_", dataset), CSupParL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("CSupParR_deveffect_", dataset), CSupParR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("CPostParL_deveffect_", dataset), CPostParL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("CPostParR_deveffect_", dataset), CPostParR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("CTempL_deveffect_", dataset), CTempL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("CTempR_deveffect_", dataset), CTempR_deveffect, envir = .GlobalEnv))
  
  invisible(assign(paste0("COccL_deveffect_", dataset), COccL_deveffect, envir = .GlobalEnv))
  invisible(assign(paste0("COccR_deveffect_", dataset), COccR_deveffect, envir = .GlobalEnv))
}


# plot tract to region age effects on cortical surface
plot_cortex_ageeffects <- function(bundle_name, dataset, ylim1, ylim2) {
  df <- get(paste0(bundle_name, "_deveffect_", dataset))
  if(grepl("L$", bundle_name)) {
    hemi = "left"
    cortical_pos1 <- "left lateral" 
    cortical_pos2 <- "left medial"
  } else{
    hemi = "right"
    cortical_pos1 <- "right lateral" 
    cortical_pos2 <- "right medial"
  }
  plot_lateral <- ggplot() + 
    geom_brain(data = df, atlas= glasser, 
               mapping=aes(fill=mean_age_effect), 
               show.legend=TRUE, 
               hemi = hemi,
               position = position_brain(cortical_pos1)) +
    paletteer::scale_fill_paletteer_c("grDevices::RdYlBu", direction = -1 , limits = c(ylim1, ylim2), oob = squish, na.value = "white") +
    theme_void() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.margin = unit(c(0.1, -1, 0.1, -1), "cm"),
          plot.title = element_blank()) 
  
  plot_medial <- ggplot() + 
    geom_brain(data = df, atlas= glasser, 
               mapping=aes(fill=mean_age_effect), 
               show.legend=TRUE, 
               hemi = hemi,
               position = position_brain(cortical_pos2)) +
    paletteer::scale_fill_paletteer_c("grDevices::RdYlBu", direction = -1 , limits = c(ylim1, ylim2), oob = squish, na.value = "white") +
    theme_void() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.margin = unit(c(0.1, -1, 0.1, -1), "cm"),
          plot.title = element_blank())
    return(plot_grid(plot_lateral, plot_medial, ncol = 1, rel_heights = c(1, 1), align = "v", axis = "lr"))
}


# format derivatives df
format_derivs <- function(df) {
  # add tractID, tract_label, and nodeID
  df <- df %>% mutate(hemi = ifelse(grepl("Left", tract_node), "Left", 
                                    ifelse(grepl("Right", tract_node), "Right", NA))) %>%
    mutate(tractID = gsub("_[0-9]+", "", tract_node)) %>%
    mutate(tract_label = gsub("Left |Right ", "", gsub("_", " ", tractID))) %>%
    mutate(nodeID = str_extract(tract_node, "[0-9]+")) 
  df$nodeID <- as.numeric(df$nodeID)
  df$tract_label <- gsub("\\.", "-", df$tract_label) 
  # label main orientation
  df <- df %>% 
    mutate(main_orientation = case_when(
      tract_label %in% c("Anterior Thalamic", "Cingulum Cingulate", "Inferior Fronto-occipital",
                         "Inferior Longitudinal", "Superior Longitudinal") ~ "AP",
      tract_label %in% c("Arcuate", "Uncinate") ~ "AP_frontal_temporal",
      tract_label %in% c("Callosum Anterior Frontal", "Callosum Motor", "Callosum Occipital", "Callosum Orbital", 
                         "Callosum Posterior Parietal", "Callosum Superior Frontal", "Callosum Superior Parietal", "Callosum Temporal") ~ "RL",
      tract_label %in% c("Corticospinal", "Posterior Arcuate","Vertical Occipital") ~ "SI",
      TRUE ~ NA_character_
    ))
  df$main_orientation <- as.factor(df$main_orientation)
  df <- df %>% relocate(tract_label, tractID, nodeID, tract_node, hemi, main_orientation)
  return(df)
}


# format fitted smooths df
format_fitted <- function(df) {
  # add tractID, tract_label, and nodeID
  df <- df %>% mutate(tractID = gsub("_[0-9]+", "", tract)) %>%
    mutate(tract_label = gsub("Left |Right ", "", gsub("_", " ", tractID))) %>%
    mutate(nodeID = str_extract(tract, "[0-9]+")) %>% 
    mutate(hemi = ifelse(grepl("Left", tractID), "Left", 
                         ifelse(grepl("Right", tractID), "Right", NA)))  
  df$nodeID <- as.numeric(df$nodeID)
  df$tract_label <- gsub("\\.", "-", df$tract_label) 
  # label main orientation
  df <- df %>% 
    mutate(main_orientation = case_when(
      tract_label %in% c("Anterior Thalamic", "Cingulum Cingulate", "Inferior Fronto-occipital",
                         "Inferior Longitudinal", "Superior Longitudinal") ~ "AP",
      tract_label %in% c("Arcuate", "Uncinate") ~ "AP_frontal_temporal",
      tract_label %in% c("Callosum Anterior Frontal", "Callosum Motor", "Callosum Occipital", "Callosum Orbital", 
                         "Callosum Posterior Parietal", "Callosum Superior Frontal", "Callosum Superior Parietal", "Callosum Temporal") ~ "RL",
      tract_label %in% c("Corticospinal", "Posterior Arcuate","Vertical Occipital") ~ "SI",
      TRUE ~ NA_character_
    ))
  df$main_orientation <- as.factor(df$main_orientation)
  df <- df %>% relocate(tract_label, tractID, nodeID, hemi, main_orientation)
  return(df)
}

# plot NEST results comparing age effects at each tract end
color1 = "#C73000FF"
color2 = "#009DA8FF"
plot_NEST_tract_ends <- function(dataset, tract, color1, color2, ylim1, ylim2, significance_text) {
  bilateral <- rbind(get(paste0(tract, "L_deveffect_", dataset)) %>% filter(end == "end1" | end == "end2"), get(paste0(tract, "R_deveffect_", dataset)) %>% filter(end == "end1" | end == "end2"))
  
  if(str_detect(tract, "^C")) {
    end1_label <- "Right Motor"
    end2_label <- "Left Motor"
  } else {
    end1_label <- "Frontal \n (High S-A)"
    end2_label <- "Occipital \n (Low S-A)"
  }
  
  NEST_df <- bilateral %>% group_by(end) %>% summarise(bilat_mean_age_effect = mean(mean_age_effect, na.rm = TRUE),
                                                       n = sum(!is.na(mean_age_effect)),
                                                       se_age_effect = sd(mean_age_effect, na.rm = TRUE) / sqrt(n)) %>% 
    mutate(endpoint_labels = case_when(end == "end1" ~ end1_label,
                                       end == "end2" ~ end2_label,
                                       TRUE ~ NA_character_))
  if(str_detect(tract, "^C")) {
    NEST_df$endpoint_labels <- factor(NEST_df$endpoint_labels, levels = c("Right Motor", "Left Motor"))
  } else {
    NEST_df$endpoint_labels <- factor(NEST_df$endpoint_labels, levels = c("Frontal \n (High S-A)", "Occipital \n (Low S-A)"))
  }
  
  NEST_plot <- ggplot(NEST_df, aes(x = endpoint_labels, y = bilat_mean_age_effect, group = 1)) +
    geom_line(size = 1, color = "black", alpha = 0.7) +
    geom_point(aes(color = endpoint_labels, fill = end), size = 8) +
    geom_errorbar(aes(ymin = bilat_mean_age_effect - se_age_effect,
                      ymax = bilat_mean_age_effect + se_age_effect,
                      color = endpoint_labels),
                  width = 0.05, size = 1, alpha = 0.7) +
    scale_color_manual(values = setNames(c(color1, color2), c(end1_label, 
                                                              end2_label))) + 
    geom_text(aes(x = mean(c(1, 2)), y = 0.41), 
              label = significance_text, 
              color = "black", size = 7, vjust = 0) + 
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20, color = "black"),
          axis.text.y = element_text(size = 20, color = "black"),
          plot.margin = margin(20, 35, 10, 35)) + ylim(ylim1, ylim2)
  
  return(NEST_plot)
}


# plot developmental trajectories and derivatives bar for tract ends
color1 = "#C73000FF"
color2 = "#009DA8FF"
## @param tp_df, tract profiles df
dev_trajectory_plot <- function(dataset, tp_df, tract_name, fitted_df, derivs_df, end1, end2, clipEnds, bin_num_nodes, ylim1, ylim2) {
  
  df <- tp_df %>% dplyr::filter(tract_label == tract_name) %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1))
  df_binned <- df %>%
    filter(
      nodeID < (bin_num_nodes + clipEnds) | 
        nodeID > (99 - bin_num_nodes - clipEnds))  %>%
    mutate(
      node_position = case_when(
        nodeID < bin_num_nodes + clipEnds ~ end1,
        nodeID > 99 - bin_num_nodes - clipEnds ~ end2,
        TRUE ~ NA_character_
      )
    )
  
  fitted_df <- fitted_df %>% dplyr::filter(tract_label == tract_name) %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1))
  fitted_df_binned <- fitted_df %>%
    filter(
      nodeID < (bin_num_nodes + clipEnds) | 
        nodeID > (99 - bin_num_nodes - clipEnds))  %>%
    mutate(
      node_position = case_when(
        nodeID < bin_num_nodes + clipEnds ~ end1,
        nodeID > 99 - bin_num_nodes - clipEnds ~ end2,
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(tract_label, node_position, age) %>%  
    summarise(
      mean_fitted = mean(fitted, na.rm = TRUE),
      n = sum(!is.na(fitted)),   
      lower = mean_fitted - (sd(fitted, na.rm = TRUE) / sqrt(n)),  
      upper = mean_fitted + (sd(fitted, na.rm = TRUE) / sqrt(n)) 
    )
  
  
  derivs_df <- derivs_df  %>% filter(tract_label == tract_name) %>% filter(nodeID > (clipEnds-1) & nodeID < (99-clipEnds+1))
  derivatives.df <- derivs_df %>%
    filter(
      nodeID < (bin_num_nodes + clipEnds) | 
        nodeID > (99 - bin_num_nodes - clipEnds))  %>%
    mutate(
      node_position = case_when(
        nodeID < bin_num_nodes + clipEnds ~ end1,
        nodeID > 99 - bin_num_nodes - clipEnds ~ end2,
        TRUE ~ NA_character_
      )
    )
  
  # trajectory plot
  title = gsub("PD", "P-D", dataset)
  y_ticks <- seq(ylim1, ylim2, length.out = 4)
  
  trajectory.plot <- ggplot(data = df_binned, aes(x = age, y = dti_md, color = node_position)) +
    geom_point(aes(alpha = hemi), size = 0.1, alpha = 0.05) +
    geom_line(data = fitted_df_binned, aes(x = age, y = mean_fitted, color = node_position), linewidth = 1) +
    geom_ribbon(data = fitted_df_binned, aes(x = age, y = mean_fitted,  ymin = lower, ymax = upper, fill = node_position), alpha = 0.5, linetype = 0) +
    scale_colour_manual(values = setNames(c(color1, color2), c(end1, end2)), na.value = "grey50")  + 
    scale_fill_manual(values = setNames(c(color1, color2), c(end1, end2)), na.value = "grey50")  + 
    scale_x_continuous(limits = c(5, 23), expand = c(0.025,.45)) + 
    scale_y_continuous(limits = c(ylim1, ylim2), breaks = seq(ylim1, ylim2, length.out = 4),
                       labels = function(x) format(x, scientific = TRUE, digits = 2)) +  
    theme(plot.title = element_text(size =20, hjust = 0.5, color = "black"),
          legend.position = "none", 
          legend.text = element_text(size =20, color = "black"),
          axis.text = element_text(size =20, color = "black"),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank()) + labs(color = "", fill = "", title = title) + guides(fill=guide_legend(ncol=1))
  
  
  # significant derivatives (rate of age-related change) plot 
  derivatives.df_end1 <- derivatives.df %>% filter(node_position == end1)
  derivatives.df_end2 <- derivatives.df %>% filter(node_position == end2)
  
 derivatives_end1.plot <- ggplot(data = derivatives.df_end1) + 
        geom_tile(aes(x = age, y = 0.1, fill = significant.derivative), height = 0.2) + 
        scale_fill_gradient2(low = alpha(color1, 0.2), high = color1, na.value = "white") + 
        scale_x_continuous(limits = c(5, 23), expand = c(0.025, 0.45)) + 
        theme_classic() + 
        theme(
          legend.position = "none", 
          axis.title.y = element_blank(), 
          axis.text = element_blank(), 
          axis.line = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          plot.margin = unit(c(-5, 0, -1, 0), "pt"))
      
  derivatives_end2.plot <- ggplot(data = derivatives.df_end2) + 
    geom_tile(aes(x = age, y = 0.1, fill = significant.derivative), height = 0.2) + 
    scale_fill_gradient2(low = alpha(color2, 0.2), high = color2, na.value = "white") + 
    scale_x_continuous(limits = c(5, 23), expand = c(0.025, 0.45)) + 
    theme_classic() + 
    theme(
      legend.position = "none", 
      axis.title.y = element_blank(), 
      axis.text = element_blank(), 
      axis.line = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(), 
      plot.margin = unit(c(-5, 0, -1, 0), "pt"))
  
  allplots <- list(trajectory.plot, derivatives_end1.plot, derivatives_end2.plot)
  tract.plot <- plot_grid(rel_heights = c(16, 0.6, 0.5), plotlist = allplots, align = "v", axis = "tb", ncol = 1)
  tract.plot <- ggdraw() + draw_plot(tract.plot, 0, 0.1, 1, 0.9) + draw_label("Age (years)", x = 0.5, y = 0.07, size = 20, hjust = 0.25)
  return(tract.plot)
  
}

################################
# Figure 6
################################
# create dataframe that combines the age of maturation for all cortical endpoints across tracts (separate for hemisphere). 
## This is done for each dataset.
aggregate_age_maps <- function(dataset, write_out = NULL) {
  # left hemi
  lh_base_names <- c("COrbL_deveffect_", "CAntFrL_deveffect_", "CSupFrL_deveffect_", 
                     "CMotL_deveffect_", "CSupParL_deveffect_", "CPostParL_deveffect_", 
                     "CTempL_deveffect_", "COccL_deveffect_", "ARCL_deveffect_", 
                     "IFOL_deveffect_", "SLFL_deveffect_", "ILFL_deveffect_", "pARCL_deveffect_", 
                     "UNCL_deveffect_","VOFL_deveffect_")
  lh_dfs <- lapply(lh_base_names, function(name) get(paste0(name, dataset))) # tract_deveffect_dataset (age of maturation maps for each tract from make_maps)
  lh_by_region_all <- do.call(rbind, lh_dfs) %>% group_by(region) %>% 
    summarise(regional_mean_ageeffect = mean(mean_age_effect, na.rm=T))
  lh_by_region_all <- lh_by_region_all %>% mutate(coverage = ifelse(is.na(regional_mean_ageeffect), "no", "yes"))
  
  # right hemi
  rh_base_names <- c("COrbR_deveffect_", "CAntFrR_deveffect_", "CSupFrR_deveffect_", 
                     "CMotR_deveffect_", "CSupParR_deveffect_", "CPostParR_deveffect_", 
                     "CTempR_deveffect_", "COccR_deveffect_", "ARCR_deveffect_", 
                     "IFOR_deveffect_", "SLFR_deveffect_", "ILFR_deveffect_", "pARCR_deveffect_", 
                     "UNCR_deveffect_","VOFR_deveffect_")
  
  rh_dfs <- lapply(rh_base_names, function(name) get(paste0(name, dataset)))
  rh_by_region_all <- do.call(rbind, rh_dfs) %>% group_by(region) %>% 
    summarise(regional_mean_ageeffect = mean(mean_age_effect, na.rm=T))
  rh_by_region_all <- rh_by_region_all %>% mutate(coverage = ifelse(is.na(regional_mean_ageeffect), "no", "yes"))
  
  if(!is.null(write_out)){
    write.csv(lh_by_region_all, sprintf("/cbica/projects/luo_wm_dev/output/%1$s/tract_profiles/tract_to_cortex/lh_agg_age_mat.csv", dataset), row.names=F)
    write.csv(rh_by_region_all, sprintf("/cbica/projects/luo_wm_dev/output/%1$s/tract_profiles/tract_to_cortex/rh_agg_age_mat.csv", dataset), row.names=F)
  }
  
  return(list(lh_by_region_all, rh_by_region_all))
}



# for each tract's endpoint, compute the mean S-A axis rank
merge_SAaxis <- function(bundle_name, df_to_return = "endpoints_only", dataset, perm_SAaxis = NULL) {
  df <- get(paste0(bundle_name, "_deveffect_", dataset))
  if(is.null(perm_SAaxis)) {
    merged_df <- merge(df, glasser_SAaxis,  by = "regionName")
  } else if (!is.null(perm_SAaxis)) {
    merged_df <- cbind(df, perm_SAaxis)
  }
  merged_df <- merged_df %>% arrange(thresh_probability)
  
  mean_df <- merged_df %>% group_by(end) %>% summarise(mean_SA = mean(SA.axis_rank, na.rm = T),
                                                       median_SA = median(SA.axis_rank, na.rm = T),
                                                       age_effect = mean(mean_age_effect)) %>% mutate(bundle_name = bundle_name)
  
  if(df_to_return == "all_regions") {
    return(merged_df)
  } else {
    return(mean_df)
  }
}


# compute mean SA rank for each cortical endpoint
compute_mean_SA <- function(dataset, SAaxis.perm = NULL) {
  lh_names <- c("ARCL", "ILFL", "IFOL", "SLFL", "pARCL", "UNCL", "VOFL", 
                "COrbL", "CAntFrL" ,"CSupFrL", "CMotL", "CSupParL", "CPostParL", "CTempL", "COccL")
  if (is.null(SAaxis.perm)) {
    lh_SAranks <- lapply(lh_names, merge_SAaxis, "all_regions", dataset=dataset)
    lh_SAranks_endpoints <- lapply(lh_names, merge_SAaxis, "endpoints_only", dataset=dataset)
  } else if (!is.null(SAaxis.perm)) {
    lh_SAranks <- lapply(lh_names, merge_SAaxis, perm_SAaxis = data.frame(SAaxis.perm) %>% setNames("SA.axis_rank"), 
                         df_to_return = "all_regions", dataset = dataset)
    lh_SAranks_endpoints <- lapply(lh_names, merge_SAaxis, perm_SAaxis = data.frame(SAaxis.perm) %>% setNames("SA.axis_rank"), 
                                   df_to_return = "endpoints_only", dataset=dataset) 
  }
  lh_SAranks_endpoints <- lapply(lh_SAranks_endpoints, function(df) na.omit(df))
  
  rh_names <- c("ARCR", "ILFR", "IFOR", "SLFR", "pARCR", "UNCR", "VOFR", 
                "COrbR", "CAntFrR" ,"CSupFrR", "CMotR", "CSupParR", "CPostParR", "CTempR", "COccR")
  if (is.null(SAaxis.perm)) {
    rh_SAranks <- lapply(rh_names, merge_SAaxis, "all_regions", dataset=dataset)
    rh_SAranks_endpoints <- lapply(rh_names, merge_SAaxis, "endpoints_only", dataset=dataset)
  } else if (!is.null(SAaxis.perm)) {
    rh_SAranks <- lapply(rh_names, merge_SAaxis, perm_SAaxis = data.frame(SAaxis.perm) %>% setNames("SA.axis_rank"), 
                         df_to_return = "all_regions", dataset = dataset)
    rh_SAranks_endpoints <- lapply(rh_names, merge_SAaxis, perm_SAaxis = data.frame(SAaxis.perm) %>% setNames("SA.axis_rank"), 
                                   df_to_return = "endpoints_only", dataset=dataset) 
  }
  rh_SAranks_endpoints <- lapply(rh_SAranks_endpoints, function(df) na.omit(df))
  names(lh_SAranks) <- lh_names
  names(rh_SAranks) <- rh_names
  names(lh_SAranks_endpoints) <- lh_names
  names(rh_SAranks_endpoints) <- rh_names
  lh_all_endpoints <- bind_rows(lh_SAranks_endpoints) %>% filter(!is.na(end))
  rh_all_endpoints <- bind_rows(rh_SAranks_endpoints) %>% filter(!is.na(end))
  all_endpoints <- rbind(lh_all_endpoints, rh_all_endpoints) %>% arrange(bundle_name)
  return(list(lh_all_endpoints, rh_all_endpoints, all_endpoints))
}

# compute the difference in mean S-A rank between cortical endpoints
compute_endpoint_absdiffs <- function(bundle_name, lh_all_endpoints, rh_all_endpoints) {
  
  callosum <- c("COrb", "CAntFr", "CSupFr", "CMot", "CSupPar", "CPostPar", "CTemp", "COcc")
  
  if (any(sapply(callosum, function(x) grepl(x, bundle_name)))) {
    bundle_type = "callosum"
  } else {
    bundle_type = "assoc"
  }
  
  # for assocation bundles, get the difference in age of maturation and diff in mean SA
  if(bundle_type == "assoc") {
    bundle_name_string <- paste0(bundle_name) # ugh R idiosyncracies. 
    if(grepl("L$", bundle_name)) {
      SAranks_endpoints <- lh_all_endpoints %>% filter(bundle_name == bundle_name_string) # can't have bundle_name == bundle_name bc it confuses dplyr
    } else{
      SAranks_endpoints <- rh_all_endpoints  %>% filter(bundle_name == bundle_name_string)
    }
    
    endpoints_diffs <- SAranks_endpoints %>% filter(if_all(everything(), ~ !is.na(.))) %>%
      summarise(mean_SA_diff = mean_SA[end == "end1"] - mean_SA[end == "end2"],
                age_effect_diff = abs(age_effect[end == "end1"] - age_effect[end == "end2"])) %>% 
      mutate(bundle_name = bundle_name)
    
    # for callosum bundles, get the difference in age of maturation and diff in mean SA
  } else if (bundle_type == "callosum") {
    bundle_name_lh =  paste0(substr(bundle_name, 1, nchar(bundle_name) - 1), "L")
    bundle_name_rh =  paste0(substr(bundle_name, 1, nchar(bundle_name) - 1), "R")
    
    SAranks_endpoints1 <- lh_all_endpoints %>% filter(bundle_name == bundle_name_lh)  
    SAranks_endpoints2 <- rh_all_endpoints %>% filter(bundle_name == bundle_name_rh) 
    SAranks_endpoints <- rbind(SAranks_endpoints1, SAranks_endpoints2)
    
    endpoints_diffs <- SAranks_endpoints %>% filter(if_all(everything(), ~ !is.na(.))) %>%
      summarise(mean_SA_diff = mean_SA[end == "end1"] - mean_SA[end == "end2"],
                age_effect_diff = abs(age_effect[end == "end1"] - age_effect[end == "end2"])) %>% 
      mutate(bundle_name = bundle_name)
  }
  
  return(endpoints_diffs)
}


# compute delta_delta df's (difference between S-A rank and difference between age of maturationn of 2 cortical endpoints)
compute_absdiffs_wrapper <- function(lh_all_endpoints, rh_all_endpoints) {
  # difference in age of maturation
  lh_diffs <- lapply(lh_names, compute_endpoint_absdiffs, lh_all_endpoints, rh_all_endpoints)
  rh_diffs <- lapply(rh_names, compute_endpoint_absdiffs, lh_all_endpoints, rh_all_endpoints)
  names(lh_diffs) <- lh_names
  names(rh_diffs) <- rh_names
  lh_diffs_all <- bind_rows(lh_diffs) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  rh_diffs_all <- bind_rows(rh_diffs) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  diffs_all <- rbind(lh_diffs_all, rh_diffs_all)
  diffs_all <- diffs_all %>% filter(!grepl("^C.*R$", bundle_name)) # remove redundant callosum bundles (keep just left hemi callosum bundles to show difference between rh and lh endpoints)
  diffs_all$bundle_name <- gsub("^(C.*)L$", "\\1", diffs_all$bundle_name) # rename callosum bundles to NOT have hemisphere
  # make a column to label IFO and ILF, and label Callosum Bundles and Association Bundles - for plotting
  diffs_all <- diffs_all %>% mutate(ifo_ilf_label = case_when(str_detect(bundle_name, "^C") ~ "Callosum",
                                                              TRUE ~ "Association")) 
  diffs_all$ifo_ilf_label <- factor(diffs_all$ifo_ilf_label, levels = c("Association", "Callosum"))
  return(diffs_all)
}


# plot delta delta figure!!
plot_diffs <- function(dataset, p_label, legend_position, labels = NULL) {
  diffs_all <- get(paste0("diffs_", dataset))
  title <- gsub("PD", "P-D", dataset) # for HCPD
  # add jittered x-coordinates to the data frame
  set.seed(42)
  diffs_all$jittered_x <- jitter(as.numeric(diffs_all$mean_SA_diff), amount = 0.3)
  
  color_assoc = "#FF9E9DFF"
  color_callosum = "#cc0468"
  
  # calculate mean age_effect_diff and x-range for each group
  mean_lines <- diffs_all %>%
    group_by(group) %>%
    summarize(
      mean_age_effect = mean(age_effect_diff, na.rm = TRUE))
  mean_lines$min_x <- c(-30, 170)
  mean_lines$max_x <- c(30, 230)
  
  diffs_plot <- ggplot(diffs_all, aes(x = jittered_x, y = age_effect_diff, label = ifo_ilf_label)) + 
    geom_point(size = 6, aes(fill = ifo_ilf_label, color = ifo_ilf_label),  shape = 21, alpha = 0.8) +
    geom_segment(data = mean_lines, inherit.aes = FALSE, 
                 aes(x = min_x, xend = max_x, y = mean_age_effect, yend = mean_age_effect),color = 'black', size = 2, alpha = 0.8) +
    scale_fill_manual(values = c("Callosum" = color_callosum, "Association" = color_assoc)) +
    scale_color_manual(values = c("Callosum" = color_callosum, "Association" = color_assoc)) +
    annotate("text", x = 105, y = 10, hjust = 0.5, size = 6, label = p_label) +
    labs(title = title) +
    theme(legend.position = legend_position,
          legend.title = element_blank(),
          legend.text = element_text(size = 20, color = "black"),
          plot.title = element_text(hjust = 0.5, size = 20, color = "black"),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 20, color = "black"),
          axis.text.y = element_text(size = 20, color = "black")) + 
    ylim(-1, 10) + xlim(-50, 252) + scale_y_continuous(limits = c(-1, 10), breaks = c(0, 5, 10))
  
  if (!is.null(labels)) {
    diffs_plot <- diffs_plot + 
      geom_text_repel(aes(label = bundle_name), 
                      size = 5, 
                      max.overlaps = Inf,   
                      box.padding = 0.5,    
                      point.padding = 0.5,  
                      segment.color = "gray50",  
                      segment.size = 0.5)
  } else {
    diffs_plot <- diffs_plot
  }
  
  return(diffs_plot)
}

# plot S-A rank of tract endpoints on cortical surface
plot_SA_surface <- function(bundle_name, df, position = NULL) {
  if(grepl("L$", bundle_name)) {
    hemi = "left"
    cortical_pos1 <- "left lateral" 
    cortical_pos2 <- "left medial" 
  } else{
    hemi = "right"
    cortical_pos1 <- "right lateral" 
    cortical_pos2 <- "right medial" 
  }
  
  if(is.null(position)) {
    plot_lateral <- ggplot() + 
      geom_brain(data = df, atlas= glasser, 
                 mapping=aes(fill=SA.axis_rank), 
                 show.legend=TRUE, 
                 hemi = hemi,
                 position = position_brain(cortical_pos1)) +
      scale_fill_viridis_c(option = "magma", na.value = "white", limits = c(1, 360), direction = -1 ) + 
      theme_void() +
      theme(legend.position = "none",
            legend.title = element_blank(),
            plot.margin =  margin(0, -1, -5, 0),
            plot.title = element_blank()) 
    
    plot_medial <- ggplot() + 
      geom_brain(data = df, atlas= glasser, 
                 mapping=aes(fill=SA.axis_rank), 
                 show.legend=TRUE, 
                 hemi = hemi,
                 position = position_brain(cortical_pos2)) +
      scale_fill_viridis_c(option = "magma", na.value = "white", limits = c(1, 360), direction = -1 ) + 
      theme_void() +
      theme(legend.position = "none",
            legend.title = element_blank(),
            plot.margin =  margin(0, -1, -5, 0),
            plot.title = element_blank()) 
    return(plot_grid(plot_lateral, plot_medial, ncol = 1, rel_heights = c(1, 1), align = "v", axis = "lr"))
  } else if (position == "lateral") {
    plot_lateral <- ggplot() + 
      geom_brain(data = df, atlas= glasser, 
                 mapping=aes(fill=SA.axis_rank), 
                 show.legend=TRUE, 
                 hemi = hemi,
                 position = position_brain(cortical_pos1)) +
      scale_fill_viridis_c(option = "magma", na.value = "white", limits = c(1, 360), direction = -1 ) + 
      theme_void() +
      theme(legend.position = "none",
            legend.title = element_blank(),
            plot.margin =  margin(0, -1, -5, 0),
            plot.title = element_blank()) 
    return(plot_lateral)
    
  }
  
}

#########################
# Figure 7
#########################

# merge S-A axis with age of maturation maps
merge_SA_parcel <- function(dataset) {
  lh_by_region <- get(paste0("lh_by_region_", dataset))
  rh_by_region <- get(paste0("rh_by_region_", dataset))
  
  lh_by_region$region <- paste0(lh_by_region$region, "_L")
  rh_by_region$region <- paste0(rh_by_region$region, "_R")
  
  aggregated_df <- rbind(lh_by_region, rh_by_region) %>% mutate(regionName = region)
  aggregated_axis <- merge(glasser_SAaxis, aggregated_df, by = "regionName")
  
  return(aggregated_axis)
}

# plot aggregated map of age of maturation across datasets
plot_aggregated_maps <- function(lh_by_region_df, rh_by_region_df, ylim1, ylim2, dataset) {
  hemi = "left"
  lh_lateral <- ggplot() + 
    geom_brain(data = lh_by_region_df, atlas = glasser, 
               mapping=aes(fill = regional_mean_ageeffect, colour = coverage, size = I(0.4)), 
               show.legend=TRUE, 
               hemi = hemi,
               position = position_brain("left lateral")) + 
    
    paletteer::scale_fill_paletteer_c("grDevices::RdYlBu", na.value = "grey100", limits = c(ylim1, ylim2), oob=squish, direction = -1) +  
    scale_colour_manual(values = c(yes = "gray30", no = "grey84")) +  
    theme_void() +
    theme(legend.position = "left",
          legend.key.height = unit(0.75, 'cm'),
          legend.key.width = unit(0.5, 'cm'),
          legend.margin=margin(0,0,0,0),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          plot.margin = unit(c(-1, 0.1, 0.1, 0.1), "cm"),
          plot.title = element_text(size=12, hjust = 0.5))  + 
    guides(colour = "none", size = "none")  
  
  lh_medial <- ggplot() + 
    geom_brain(data = lh_by_region_df, atlas = glasser, 
               mapping=aes(fill = regional_mean_ageeffect, colour = coverage, size = I(0.4)), 
               show.legend=TRUE, 
               hemi = hemi,
               position = position_brain("left medial")) + 
    
    paletteer::scale_fill_paletteer_c("grDevices::RdYlBu", na.value = "grey100", limits = c(ylim1, ylim2), oob=squish, direction = -1) +  
    scale_colour_manual(values = c(yes = "gray30", no = "grey84")) + # limits = c(0.12, 0.4)
    theme_void() +
    theme(legend.position = "left",
          legend.key.height = unit(0.75, 'cm'),
          legend.key.width = unit(0.5, 'cm'),
          legend.margin=margin(0,0,0,0),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          plot.margin = unit(c(-1, 0.1, 0.1, 0.1), "cm"),
          plot.title = element_text(size=12, hjust = 0.5))  + 
    guides(colour = "none", size = "none")  
  
  hemi = "right"
  rh_lateral <- ggplot() + 
    geom_brain(data = rh_by_region_df, atlas = glasser, 
               mapping=aes(fill = regional_mean_ageeffect, colour = coverage, size = I(0.4)), 
               show.legend=TRUE, 
               hemi = hemi,
               position = position_brain("right lateral")) + 
    
    paletteer::scale_fill_paletteer_c("grDevices::RdYlBu", na.value = "grey100", limits = c(ylim1, ylim2), oob=squish, direction = -1) +  
    scale_colour_manual(values = c(yes = "gray30", no = "grey84")) + # limits = c(0.12, 0.4)
    theme_void() +
    theme(legend.position = "left",
          legend.key.height = unit(0.75, 'cm'),
          legend.key.width = unit(0.5, 'cm'),
          legend.margin=margin(0,0,0,0),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          plot.margin = unit(c(-1, 0.1, 0.1, 0.1), "cm"),
          plot.title = element_text(size=12, hjust = 0.5))  + 
    guides(colour = "none", size = "none")  
  
  rh_medial <- ggplot() + 
    geom_brain(data = rh_by_region_df, atlas = glasser, 
               mapping=aes(fill = regional_mean_ageeffect, colour = coverage, size = I(0.4)), 
               show.legend=TRUE, 
               hemi = hemi,
               position = position_brain("right medial")) + 
    
    paletteer::scale_fill_paletteer_c("grDevices::RdYlBu", na.value = "grey100", limits = c(ylim1, ylim2), oob=squish, direction = -1) +  
    scale_colour_manual(values = c(yes = "gray30", no = "grey84")) + # limits = c(0.12, 0.4)
    theme_void() +
    theme(legend.position = "left",
          legend.key.height = unit(0.75, 'cm'),
          legend.key.width = unit(0.5, 'cm'),
          legend.margin=margin(0,0,0,0),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          plot.margin = unit(c(-1, 0.1, 0.1, 0.1), "cm"),
          plot.title = element_text(size=12, hjust = 0.5))  + 
    guides(colour = "none", size = "none")  
  
  agg_plots <- ggarrange(lh_lateral, lh_medial, rh_lateral, rh_medial, ncol=4, nrow = 1, common.legend = T, legend = "left")
  agg_final <- annotate_figure(agg_plots, top = text_grob(dataset, 
                                                          color = "black", size = 12, vjust = -0.75)) + theme(plot.margin = unit(c(1, 0, 0, 0), "cm")) 
  return(agg_final)
}

# plot scatterplot of age of maturation vs. S-A axis rank (parcel-level)
plot_agg_SA_parcel <- function(dataset, annot_text, all_parcels = NULL) {
  if(is.null(all_parcels)) {
    aggregated_axis <- get(paste0("aggregated_axis_", dataset, "_binary"))
  } else {
    aggregated_axis <- get(paste0("aggregated_axis_", dataset))
  }
  
  title = gsub("PD", "P-D", dataset) # for HCPD
  SA_plot <- ggplot(aggregated_axis, aes(x = SA.axis_rank, y = regional_mean_ageeffect, fill = SA.axis_rank)) +
    geom_point(color = "gray", shape = 21, size=4, alpha = 0.9) + 
    scale_fill_viridis_c(option = "magma", na.value = "white", limits = c(1, 360), direction = -1 ) + 
    geom_smooth(data = aggregated_axis, method='lm', se=TRUE, fill=alpha(c("gray70"),.9), col="black") + 
    labs(title = title) + theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 20, color = "black"),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 20, color = "black"),
          axis.text.y = element_text(size = 20, color = "black")) + 
    annotate(geom="text", x=180, y=25, label=annot_text, color="black", size=7) +
    xlim(15, 340) + ylim(14, 26)
  return(SA_plot)
}

############################################### 
# spin tests for tract-to-region analyses 
# (used in figures 6 and 7)
############################################### 

source("/cbica/projects/luo_wm_dev/software/spin_test/perm.sphere.p.R")
perm.id.full <- readRDS("/cbica/projects/luo_wm_dev/software/spin_test/rotate_parcellation/glasser.coords_sphericalrotations_N10k.rds")

# spin test for delta-delta: spin the S-A axis then recompute average S-A rank for each end. Then calculate difference in rank for delta-delta analysis. Then compute t.test and spun p-value for t-test
## @param aggregate_age_map, vector of regional (parcel-level) age of maturation values
perm.sphere.age_map_delta_absdiff <- function(aggregate_age_map, perm.id, dataset, tract_names, all_endpoints, alternative = "greater", var.equal = FALSE) {
  
  nroi = dim(perm.id)[1]  # number of regions
  nperm = dim(perm.id)[2] # number of permutations
  
  # spin parcel-level age of maturation values: permutation of measures
  age_map.perm = array(NA,dim=c(nroi,nperm))
  for (r in 1:nperm) {
    for (i in 1:nroi) {
      age_map.perm[i,r] = aggregate_age_map[perm.id[i,r]] 
    }
  }
  
  # if want to compute spun p-value for the delta-delta plot: 
  # empirical t value
  diffs_emp <- get(paste0("diffs_", dataset))
  diffs_emp <- diffs_emp %>% mutate(group = case_when(bundle_name %in% c("ILFL", "IFOL", "ILFR", "IFOR") ~ "Large Difference in SA Rank",
                                                      !(bundle_name %in% c("ILFL", "IFOL", "ILFR", "IFOR")) ~ "Small Difference in SA Rank",
                                                      TRUE ~ NA_character_))
  t.emp <- t.test(diffs_emp$age_effect_diff[which(diffs_emp$group == "Large Difference in SA Rank")], # compare Difference in sa rank
                  diffs_emp$age_effect_diff[which(diffs_emp$group == "Small Difference in SA Rank")], 
                  alternative = alternative, var.equal = var.equal)$statistic 
  
  df.emp <- t.test(diffs_emp$age_effect_diff[which(diffs_emp$group == "Large Difference in SA Rank")], # compare Difference in sa rank
                   diffs_emp$age_effect_diff[which(diffs_emp$group == "Small Difference in SA Rank")], 
                   alternative = alternative, var.equal = var.equal)$parameter 
  # t-test between permuted S-A axis Difference and SA Rank Difference between endpoints
  t.null.age_map = vector(length=nperm)
  
  # when averaging across datasets, use PNC's bundle-to-cortex probability map
  if(dataset=="avg_datasets") {
    dataset <- "PNC"
  }
  
  for (r in 1:nperm) {
    print(r)
    spun_aggregated_axis = aggregated_axis %>% mutate(regional_mean_ageeffect = age_map.perm[,r])
    all_endpoints_spun  <- lapply(tract_names, make_null_age_maps, spun_aggregated_axis = spun_aggregated_axis)
    names(all_endpoints_spun) <- tract_names
    
    
    all_endpoints_spun <- lapply(all_endpoints_spun, function(df) na.omit(df))
    
    all_endpoints_spun <- map2(all_endpoints_spun, names(all_endpoints_spun), 
                               ~ mutate(.x,
                                        bundle_name = paste0(.y, case_when(hemi == "lh" ~ "L",
                                                                           hemi == "rh" ~ "R", 
                                                                           TRUE ~ ""))))
    
    all_endpoints_spun <- lapply(all_endpoints_spun, function(df) na.omit(df))
    all_endpoints_spun <- bind_rows(all_endpoints_spun) %>% filter(!is.na(end))
    
    # collapse all_endpoints_spun across all tracts and merge with all_endpoints to have mean_SA
    all_endpoints_spun <- merge(all_endpoints_spun, all_endpoints) %>% 
      select(end, null_age_of_maturation, mean_SA,bundle_name) %>% arrange(bundle_name, end)
    
    # split all_endpoints_spun to lh and rh so we can use compute_absdiffs_wrapper function
    lh_endpoints_spun <- all_endpoints_spun %>% filter(str_detect(bundle_name, "L$")) %>% 
      rename(age_effect = null_age_of_maturation) # rename null_age_of_maturation to fit requires for compute_absdiffs_wrapper
    rh_endpoints_spun <- all_endpoints_spun %>% filter(str_detect(bundle_name, "R$")) %>% 
      rename(age_effect = null_age_of_maturation)
    
    # calculate difference in rank
    perm_diffs <- compute_absdiffs_wrapper(lh_endpoints_spun, rh_endpoints_spun)
    perm_diffs <- perm_diffs %>% mutate(group = case_when(bundle_name %in% c("ILFL", "IFOL", "ILFR", "IFOR") ~ "Large Difference in SA Rank",
                                                          !(bundle_name %in% c("ILFL", "IFOL", "ILFR", "IFOR")) ~ "Small Difference in SA Rank",
                                                          TRUE ~ NA_character_))
    
    # calculate t-test and permuted t-test: do tracts with very diff ages of maturation between endpoints actually have different s-a ranks?? 
    t.null.age_map[r] <- t.test(perm_diffs$age_effect_diff[which(perm_diffs$group == "Large Difference in SA Rank")], 
                                perm_diffs$age_effect_diff[which(perm_diffs$group == "Small Difference in SA Rank")], 
                                alternative = alternative, var.equal = var.equal )$statistic
  }
  
  # p-value definition  
  if (t.emp>0) {
    p.perm = sum(t.null.age_map>t.emp)/nperm
  } else { 
    p.perm = sum(t.null.age_map<t.emp)/nperm
  } 
  
  return(list(p.perm = p.perm, t.emp = t.emp, df.emp = df.emp))
  # return p-value for the t-test to see if tracts with large diffs in S-A rank have significant diffs in age of maturation
  
}

# helper function for perm.sphere.age_map_delta_absdiff
make_null_age_maps <- function(tract_name, spun_aggregated_axis) {
  # get output from "make_maps" for each tract (df includes average age of maturation for each Glasser region and which tract end each region belongs)
  lh_deveffect <- get(paste0(tract_name, "L_deveffect_", dataset)) 
  rh_deveeffect <- get(paste0(tract_name, "R_deveffect_", dataset))
  bilat_deveffect <- rbind(lh_deveffect, rh_deveeffect) %>% select(-region) %>% rename(region = regionName) %>% 
    mutate(hemi = ifelse(str_detect(region, "_L$"), "lh",
                         ifelse(str_detect(region, "_R$"), "rh", NA))) %>% arrange(end)
  
  # merge spun age map with tract-ends map. Then select the aggregated, averaged age of maturation for each Glasser parcel and end assignments
  spun_age_map <- merge(spun_aggregated_axis, bilat_deveffect, by = "region") %>% arrange(end) %>% select(end, hemi, SA.axis_rank, regional_mean_ageeffect, region) 
  
  # map ages of maturation (parcel-level) back to tract endpoints
  spun_tract_ends <- spun_age_map %>% group_by(end, hemi) %>% summarise(null_age_of_maturation = mean(regional_mean_ageeffect, na.rm = T))
  return(spun_tract_ends)
}


# spun p-value for t-test of S-A rank between matured vs. not matured (parcel-level)
perm.sphere.p.ttest = function(SAaxis, perm.id, dataset, alternative = "greater", var.equal = FALSE) {
  
  nroi = dim(perm.id)[1]  # number of regions
  nperm = dim(perm.id)[2] # number of permutations
  
  # spin SA axis: permutation of measures
  SAaxis.perm = array(NA,dim=c(nroi,nperm))
  for (r in 1:nperm) {
    for (i in 1:nroi) {
      SAaxis.perm[i,r] = SAaxis[perm.id[i,r]]
    }
  }
  aggregated_axis_binary <- get(paste0("aggregated_axis_", dataset, "_binary"))
  aggregated_axis_binary <- aggregated_axis_binary %>% mutate(maturation_status = ifelse(is.na(regional_mean_ageeffect), 0, 1))
  
  # empirical t value
  # compare differences in sa rank. hypothesis is non-matured tract ends have greater S-A rank
  t.emp =  t.test(aggregated_axis_binary$SA.axis_rank[which(aggregated_axis_binary$maturation_status == 0)], 
                  aggregated_axis_binary$SA.axis_rank[which(aggregated_axis_binary$maturation_status == 1)], 
                  alternative = alternative, var.equal = var.equal)$statistic 
  df.emp = t.test(aggregated_axis_binary$SA.axis_rank[which(aggregated_axis_binary$maturation_status == 0)], 
         aggregated_axis_binary$SA.axis_rank[which(aggregated_axis_binary$maturation_status == 1)], 
         alternative = alternative, var.equal = var.equal)$parameter
  
  # t-test between permuted S-A axis differences and age of maturation differences between endpoints
  t.null.SAaxis = vector(length=nperm)
  for (r in 1:nperm) {
    # merge permuted S-A axis to age of maturation df
    perm_SAaxis <- data.frame(SAaxis.perm[,r]) %>% setNames("perm_SA.axis_rank")
    perm_aggregated_axis_binary <- cbind(aggregated_axis_binary, perm_SAaxis)
    
    # calculate t-test and permuted t-test: do tract ends that have NOT matured between endpoints actually have greater s-a ranks than tract ends that have matured?? 
    t.null.SAaxis[r] <- t.test(perm_aggregated_axis_binary$perm_SA.axis_rank[which(perm_aggregated_axis_binary$maturation_status == 0)], 
                               perm_aggregated_axis_binary$perm_SA.axis_rank[which(perm_aggregated_axis_binary$maturation_status == 1)], 
                               alternative = alternative, var.equal = var.equal)$statistic
  }
  
  # p-value definition  
  if (t.emp>0) {
    p.perm.t = sum(t.null.SAaxis>t.emp)/nperm
  } else { 
    p.perm.t= sum(t.null.SAaxis<t.emp)/nperm
  } 
  return(list(t.emp = t.emp, p.perm.t = p.perm.t, df.emp = df.emp))
}


