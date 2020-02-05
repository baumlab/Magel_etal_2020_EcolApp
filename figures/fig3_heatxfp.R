
# Direct and indirect effects of climate change-amplified pulse heat stress events on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script to create Fig. 3 (multi-panel figure plotting the biomass of fish at each local human disturbance level
# across the heat stress event, for the reef fish community as a whole and for individual trophic groups)


##############################

## Load necessary packages
library(ggplot2)
library(cowplot)
library(dplyr)
library(grid)
library(gridExtra)

## Set your working directory
# Make sure that this contains the "ki_fish_sum_data_sum.Rdata" file
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

## Load the data
load("ki_fish_data_sum.Rdata")

## Run the 'summary' script (be sure to set file path accordingly)
source("../figures/summarySE.R")
library(plyr)


##############################

# Set theme and colours for plotting

## Create colour palette for plots
fp_cols <- c("#01665E", "#80CDC1", "#DFC27D", "#663300")
palette(fp_cols)

## Set plot theme
theme_set(theme_cowplot() + theme(plot.title = element_text(size = 9, vjust = 0, hjust = 0.5, face = "plain"),
                                  axis.title = element_blank(),
                                  axis.text = element_text(size = 8),
                                  legend.title = element_text(size = 7),
                                  legend.text = element_text(size = 6),
                                  legend.key.height = unit(3, "mm"),
                                  legend.justification = c(1,1), legend.position = c(1,1),
                                  plot.margin = unit(c(0.2,0.1,0,0), "cm")))


##############################

# Generate individual plots
# Trophic groups are ordered from most to least biomass

## All fish
# Calculate mean and SE values for each heat x f.pressure combination
BM_total_se <- summarySE(ki_fish_sum, measurevar = "BM_total", groupvars = c("heat","f.pressure"))
# Plot the data
p1 <- ggplot(BM_total_se, aes(heat, BM_total_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_total_mean - se, ymax = BM_total_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  labs(x = "", y = expression("Biomass"~(g~m^{-2}))) + labs(title = "All fish") +
  geom_point(position = position_dodge(0.5), size = 3) +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))

## Herbivores
BM_herb_se <- summarySE(ki_fish_sum, measurevar = "BM_herb", groupvars = c("heat","f.pressure"))

p2 <- ggplot(BM_herb_se, aes(heat, BM_herb_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_herb_mean - se, ymax = BM_herb_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "", y = "") + labs(title = "Herbivores") +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))

## Invertivores
BM_inv_se <- summarySE(ki_fish_sum, measurevar = "BM_inv", groupvars = c("heat","f.pressure"))

p3 <- ggplot(BM_inv_se, aes(heat, BM_inv_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_inv_mean - se, ymax = BM_inv_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "", y = "") + labs(title = "Invertivores") +
  scale_colour_manual(values = fp_cols) + 
  guides(colour = guide_legend(title = "Disturbance"))

## Planktivores
BM_plank_se <- summarySE(ki_fish_sum, measurevar = "BM_plank", groupvars = c("heat","f.pressure"))

p4 <- ggplot(BM_plank_se, aes(heat, BM_plank_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_plank_mean - se, ymax = BM_plank_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "", y = expression("Biomass"~(g~m^{-2}))) + labs(title = "Planktivores") +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))

## Piscivores
BM_pisc_se <- summarySE(ki_fish_sum, measurevar = "BM_pisc", groupvars = c("heat","f.pressure"))

p5 <- ggplot(BM_pisc_se, aes(heat, BM_pisc_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_pisc_mean - se, ymax = BM_pisc_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "", y = "") + labs(title = "Piscivores") +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))

## Omnivores
BM_omn_se <- summarySE(ki_fish_sum, measurevar = "BM_omn", groupvars = c("heat","f.pressure"))

p6 <- ggplot(BM_omn_se, aes(heat, BM_omn_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_omn_mean - se, ymax = BM_omn_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "", y = "") + labs(title = "Omnivores") +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))

## Generalist carnivores
BM_gen_se <- summarySE(ki_fish_sum, measurevar = "BM_gen", groupvars = c("heat","f.pressure"))

p7 <- ggplot(BM_gen_se, aes(heat, BM_gen_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_gen_mean - se, ymax = BM_gen_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "Heat stress", y = expression("Biomass"~(g~m^{-2}))) + labs(title = "Generalist carnivores") +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))

## Corallivores
BM_coral_se <- summarySE(ki_fish_sum, measurevar = "BM_coral", groupvars = c("heat","f.pressure"))

p8 <- ggplot(BM_coral_se, aes(heat, BM_coral_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_coral_mean - se, ymax = BM_coral_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "Heat stress", y = "") + labs(title = "Corallivores") +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))

## Detritivores
BM_det_se <- summarySE(ki_fish_sum, measurevar = "BM_det", groupvars = c("heat","f.pressure"))

p9 <- ggplot(BM_det_se, aes(heat, BM_det_mean, col = f.pressure)) + 
  geom_errorbar(aes(ymin = BM_det_mean - se, ymax = BM_det_mean + se), 
                width = 0, size = 0.5, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) + 
  labs(x = "Heat stress", y = "") + labs(title = "Detritivores") +
  scale_colour_manual(values = fp_cols) + guides(colour = guide_legend(title = "Disturbance"))


##############################

## Combine all plots into a single high-resolution figure

p10 <- plot_grid(p1 + theme(legend.position = "none"),
                 p2 + theme(legend.position = "none"), 
                 p3,
                 p4 + theme(legend.position = "none"),
                 p5 + theme(legend.position = "none"),
                 p6 + theme(legend.position = "none"),
                 p7 + theme(legend.position = "none"),
                 p8 + theme(legend.position = "none"),
                 p9 + theme(legend.position = "none"),
                 align = 'vh',
                 labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)"),
                 label_size = 9,
                 label_x = rep(0.06, 9),
                 vjust = 2.2,
                 nrow = 3)

# Set text and position of axis titles
y.title <- textGrob(expression("      Biomass"~(g~m^{-2})),
                    gp = gpar(fontsize = 9), rot = 90)
x.title <- textGrob("        Heat stress", gp = gpar(fontsize = 9))

# Specify the desired output folder at "..."
jpeg(filename = ".../Figure_3.jpg",
     width = 180, height = 130, units = "mm", res = 600)

grid.arrange(arrangeGrob(p10, left = y.title, bottom = x.title))

dev.off()
