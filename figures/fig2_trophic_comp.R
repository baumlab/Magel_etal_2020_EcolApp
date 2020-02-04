
# Direct and indirect effects of climate change-amplified pulse heat stress events on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script to create Fig. 2 (multi-panel figure plotting the relative contributions of each trophic group
# to total reef fish biomass, abundance, and species richness across the heat stress event)


##############################

## Load necessary packages
library(dplyr)
library(ggplot2)
library(cowplot)

## Set your working directory
# Make sure that this contains the "ki_fish_sum_data_raw.Rdata" file
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

## Load the data
load("ki_fish_data_raw.Rdata")

## Additional data cleaning
# Double number of small fish to account for smaller survey area (300m^2 compared to 600m^2 for large fish))
ki_small <- ki_full[ki_full$length < 20, ]
ki_large <- ki_full[ki_full$length >= 20, ]
ki_small$number <- ki_small$number*2
ki_full <- rbind(ki_small, ki_large)
# Calculate biomass
ki_full$biomass <- ki_full$number * ki_full$mass


##############################

## Prepare data for plotting

## Reorder trophic groups
# Groups are ordered from least to most biomass (i.e., detritivore < corallivore < generalist carnivore <
# omnivore < piscivore < planktivore < invertivore < herbivore)
ki_full$trophic <- as.factor(ki_full$trophic)
ki_full$trophic <- factor(ki_full$trophic, levels(factor(ki_full$trophic))[c(2,1,3,6,7,8,5,4)])

## Create different data frame for species richness
ki_full_SR <- ki_full
# Remove all observations that were not identified to the species level
ki_full_SR <- ki_full_SR[!grepl("Acanthurus sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Blenniidae sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Chlorurus sp.", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Cirripectes sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Epinephelus sp.", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Gymnothorax sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Halichoeres sp.", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Kyphosus sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Parapercis sp.", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Pervagor sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Plagiotremus sp.", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Pseudanthias sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Ptereleotris sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Pterocaesio sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Pterois sp.", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Scarus sp", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Synodus sp.", ki_full_SR$species), ]
ki_full_SR <- ki_full_SR[!grepl("Valenciennea sp.", ki_full_SR$species), ]

## Set colour palette for trophic groups
trophic_pal <- c("#1B2145", "#49557F", "#7891BA", "#B9DBF1", "#73B2D8", "#4990C2", "#255D98", "#182B64")


##############################

## Calculate mean site-level biomass, abundance, and species richness for each trophic group at each heat stress time point


#############
## BIOMASS ##
#############

# Sum biomass values to the observer level (for each site in each year)
BM_trophic <- ki_full %>% group_by(heat, year, ki.date, site, observer, trophic) %>% 
  summarise(biomass = sum(biomass)/300)
# Average site-level values across observers
BM_trophic <- BM_trophic %>% group_by(heat, year, ki.date, site, trophic) %>% 
  summarise(biomass = mean(biomass))
# Average site-level values across dates in the same year (for sites suryeved multiple times in the same field season)
BM_trophic <- BM_trophic %>% group_by(heat, year, site, trophic) %>% 
  summarise(biomass = mean(biomass))
# Average site-level values across years
BM_trophic <- BM_trophic %>% group_by(heat, site, trophic) %>% 
  summarise(biomass = mean(biomass))
# Average values across all sites in the same field season
BM_trophic <- BM_trophic %>% group_by(heat, trophic) %>% 
  summarise(biomass = mean(biomass))

# Biomass plot
p1 <- ggplot(BM_trophic, aes(heat, biomass)) +
  geom_bar(aes(fill = trophic), stat = "identity", position = "stack") +
  labs(x = "Heat stress", y = expression("Biomass"~(g~m^{-2}))) +
  guides(fill = guide_legend(title = "Trophic group")) +
  scale_fill_manual(values = trophic_pal) + theme_cowplot()


###############
## ABUNDANCE ##
###############

# Sum abundance values to the observer level (for each site in each year)
AB_trophic <- ki_full %>% group_by(heat, year, ki.date, site, observer, trophic) %>% 
  summarise(abundance = sum(number))
# Average site-level values across observers
AB_trophic <- AB_trophic %>% group_by(heat, year, ki.date, site, trophic) %>% 
  summarise(abundance = mean(abundance))
# Average site-level values across dates in the same year (for sites suryeved multiple times in the same field season)
AB_trophic <- AB_trophic %>% group_by(heat, year, site, trophic) %>% 
  summarise(abundance = mean(abundance))
# Average site-level values across years
AB_trophic <- AB_trophic %>% group_by(heat, site, trophic) %>% 
  summarise(abundance = mean(abundance))
# Average values across all sites in the same field season
AB_trophic <- AB_trophic %>% group_by(heat, trophic) %>% 
  summarise(abundance = mean(abundance))

# Abundance plot
p2 <- ggplot(AB_trophic, aes(heat, abundance)) +
  geom_bar(aes(fill = trophic), stat = "identity", position = "stack") +
  labs(x = "Heat stress", y = "Abundance") +
  guides(fill = guide_legend(title = "Trophic group")) +
  scale_fill_manual(values = trophic_pal) + theme_cowplot()


######################
## SPECIES RICHNESS ##
######################

# Calculate the number of unique species recorded by each observer (for each site in each year)
SR_trophic <- ki_full_SR %>% group_by(heat, year, ki.date, site, observer, trophic) %>% 
  summarise(richness = n_distinct(species))
# Average site-level values across observers
SR_trophic <- SR_trophic %>% group_by(heat, year, ki.date, site, trophic) %>% 
  summarise(richness = mean(richness))
# Average site-level values across dates in the same year (for sites suryeved multiple times in the same field season)
SR_trophic <- SR_trophic %>% group_by(heat, year, site, trophic) %>% 
  summarise(richness = mean(richness))
# Average site-level values across years
SR_trophic <- SR_trophic %>% group_by(heat, site, trophic) %>% 
  summarise(richness = mean(richness))
# Average values across all sites in the same field season
SR_trophic <- SR_trophic %>% group_by(heat, trophic) %>% 
  summarise(richness = mean(richness))

# Richness plot
p3 <- ggplot(SR_trophic, aes(heat, richness)) +
  geom_bar(aes(fill = trophic), stat = "identity", position = "stack") +
  xlab("Heat stress") + ylab("species richness") +
  guides(fill = guide_legend(title = "Trophic group")) +
  scale_fill_manual(values = trophic_pal) + theme_cowplot()


######################
### COMPOSITE PLOT ###
######################

## Combine all three plots into a single high-resolution figure

legend <- get_legend(p1)

p4 <- plot_grid(p1 + theme(legend.position = "none"),
                p2 + theme(legend.position = "none"),
                p3 + theme(legend.position = "none"),
                align = 'vh',
                labels = c("(a)", "(b)", "(c)"),
                hjust = -0.75,
                nrow = 1)

# Specify the desired output folder at "..."
jpeg(filename = ".../Figure_2.jpg",
     width = 12, height = 4, units = "in", res = 600)

plot_grid(p4, legend, rel_widths = c(2.5,.5))

dev.off()

