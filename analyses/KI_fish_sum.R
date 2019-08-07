
# Pulse heat stress events foreshadow long-term climate change impacts on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script used to produce the "KI_fish_data_sum.Rdata" dataset
# Calculating site-level biomass, abundance, and species richness values for the reef fish data


##############################

# Load necessary packages
library(dplyr)

# Set your working directory
# Make sure that this contains the "KI_fish_data_raw.Rdata" file
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

# Load the data
load("KI_fish_data_raw.Rdata")

## Additional data cleaning
# Double number of small fish to account for smaller survey area (300m^2 compared to 600m^2 for large fish))
ki_small <- ki_fish[ki_fish$length < 20, ]
ki_large <- ki_fish[ki_fish$length >= 20, ]
ki_small$number <- ki_small$number*2
ki_fish <- rbind(ki_small, ki_large)
# Remove sharks and jacks (due to potential survey biases)
ki_fish <- ki_fish[!grepl("Caranx", ki_fish$species), ] # Removes 29 observations
ki_fish <- ki_fish[!grepl("Carangoides", ki_fish$species), ] # Removes 3 observations
ki_fish <- ki_fish[!grepl("Scomberoides", ki_fish$species), ] # Removes 14 observations
ki_fish <- ki_fish[!grepl("Carcharhinus", ki_fish$species), ] # Removes 1 observation
# Calculate biomass
ki_fish$biomass <- ki_fish$number * ki_fish$mass

# Create a separate data frame for each functional group
ki_coral <- ki_fish[ki_fish$trophic == "Corallivore", ]
ki_det <- ki_fish[ki_fish$trophic == "Detritivore", ]
ki_gen <- ki_fish[ki_fish$trophic == "Generalist carnivore", ]
ki_herb <- ki_fish[ki_fish$trophic == "Herbivore", ]
ki_inv <- ki_fish[ki_fish$trophic == "Invertivore", ]
ki_omn <- ki_fish[ki_fish$trophic == "Omnivore", ]
ki_pisc <- ki_fish[ki_fish$trophic == "Piscivore", ]
ki_plank <- ki_fish[ki_fish$trophic == "Planktivore", ]


##############################

# Calculate site-level sums for reef fish biomass, abundance, and species richness (values for each observer remain separate)


###############
### BIOMASS ###
###############

# Note that summed values are divided by 300 to give biomass in units of g/m^2

## Total biomass
ki_BM <- ki_fish %>% group_by(heat, year, ki.date, site, observer, f.pressure, npp_max, time_of_day) %>% 
  summarise(BM_total = sum(biomass)/300)
# Note that f.pressure and npp_max both map to site, while time_of_day maps to the combination of ki.date and site; these
# variables are included in the above calculation to ensure that they are retained in the final dataset, but are excluded
# from subsequent calculations for the sake or simplicity

## Functional group biomass
# Corallivores
ki_BM_coral <- ki_coral %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_coral = sum(biomass)/300)
# Detritivores
ki_BM_det <- ki_det %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_det = sum(biomass)/300)
# Generalist carnivores
ki_BM_gen <- ki_gen %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_gen = sum(biomass)/300)
# Herbivores
ki_BM_herb <- ki_herb %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_herb = sum(biomass)/300)
# Invertivores
ki_BM_inv <- ki_inv %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_inv = sum(biomass)/300)
# Omnivores
ki_BM_omn <- ki_omn %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_omn = sum(biomass)/300)
# Piscivores
ki_BM_pisc <- ki_pisc %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_pisc = sum(biomass)/300)
# Planktivores
ki_BM_plank <- ki_plank %>% group_by(heat, year, ki.date, site, observer) %>% summarise(BM_plank = sum(biomass)/300)


#################
### ABUNDANCE ###
#################

## Total abundance
ki_AB <- ki_fish %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_total = sum(number))

## Functional group abundance
# Corallivores
ki_AB_coral <- ki_coral %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_coral = sum(number))
# Detritivores
ki_AB_det <- ki_det %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_det = sum(number))
# Generalist carnivores
ki_AB_gen <- ki_gen %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_gen = sum(number))
# Herbivores
ki_AB_herb <- ki_herb %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_herb = sum(number))
# Invertivores
ki_AB_inv <- ki_inv %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_inv = sum(number))
# Omnivores
ki_AB_omn <- ki_omn %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_omn = sum(number))
# Piscivores
ki_AB_pisc <- ki_pisc %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_pisc = sum(number))
# Planktivores
ki_AB_plank <- ki_plank %>% group_by(heat, year, ki.date, site, observer) %>% summarise(AB_plank = sum(number))


########################
### species RICHNESS ###
########################

# Remove observations that were not identified to species level
ki_fish_SR <- ki_fish
ki_fish_SR <- ki_fish_SR[!grepl("Acanthurus sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Blenniidae sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Chlorurus sp.", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Cirripectes sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Epinephelus sp.", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Gymnothorax sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Halichoeres sp.", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Kyphosus sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Parapercis sp.", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Pervagor sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Plagiotremus sp.", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Pseudanthias sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Ptereleotris sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Pterocaesio sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Pterois sp.", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Scarus sp", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Synodus sp.", ki_fish_SR$species), ]
ki_fish_SR <- ki_fish_SR[!grepl("Valenciennea sp.", ki_fish_SR$species), ]

# Total species richness
ki_SR <- ki_fish_SR %>% group_by(heat, year, ki.date, site, observer) %>% summarise(SR_total = n_distinct(species))


##############################

## Combine calculated values into a single data frame

# Rename the ki_BM data frame
ki_fish_sum <- ki_BM

# Create a unique identifier to match ... by combining the site, date, and observer values for each ...
ki_fish_sum$dso <- paste(ki_fish_sum$site, ki_fish_sum$ki.date, ki_fish_sum$observer, sep = "-")

ki_BM_coral$dso <- paste(ki_BM_coral$site, ki_BM_coral$ki.date, ki_BM_coral$observer, sep = "-")
ki_BM_det$dso <- paste(ki_BM_det$site, ki_BM_det$ki.date, ki_BM_det$observer, sep = "-")
ki_BM_gen$dso <- paste(ki_BM_gen$site, ki_BM_gen$ki.date, ki_BM_gen$observer, sep = "-")
ki_BM_herb$dso <- paste(ki_BM_herb$site, ki_BM_herb$ki.date, ki_BM_herb$observer, sep = "-")
ki_BM_inv$dso <- paste(ki_BM_inv$site, ki_BM_inv$ki.date, ki_BM_inv$observer, sep = "-")
ki_BM_omn$dso <- paste(ki_BM_omn$site, ki_BM_omn$ki.date, ki_BM_omn$observer, sep = "-")
ki_BM_pisc$dso <- paste(ki_BM_pisc$site, ki_BM_pisc$ki.date, ki_BM_pisc$observer, sep = "-")
ki_BM_plank$dso <- paste(ki_BM_plank$site, ki_BM_plank$ki.date, ki_BM_plank$observer, sep = "-")
ki_AB$dso <- paste(ki_AB$site, ki_AB$ki.date, ki_AB$observer, sep = "-")
ki_AB_coral$dso <- paste(ki_AB_coral$site, ki_AB_coral$ki.date, ki_AB_coral$observer, sep = "-")
ki_AB_det$dso <- paste(ki_AB_det$site, ki_AB_det$ki.date, ki_AB_det$observer, sep = "-")
ki_AB_gen$dso <- paste(ki_AB_gen$site, ki_AB_gen$ki.date, ki_AB_gen$observer, sep = "-")
ki_AB_herb$dso <- paste(ki_AB_herb$site, ki_AB_herb$ki.date, ki_AB_herb$observer, sep = "-")
ki_AB_inv$dso <- paste(ki_AB_inv$site, ki_AB_inv$ki.date, ki_AB_inv$observer, sep = "-")
ki_AB_omn$dso <- paste(ki_AB_omn$site, ki_AB_omn$ki.date, ki_AB_omn$observer, sep = "-")
ki_AB_pisc$dso <- paste(ki_AB_pisc$site, ki_AB_pisc$ki.date, ki_AB_pisc$observer, sep = "-")
ki_AB_plank$dso <- paste(ki_AB_plank$site, ki_AB_plank$ki.date, ki_AB_plank$observer, sep = "-")
ki_SR$dso <- paste(ki_SR$site, ki_SR$ki.date, ki_SR$observer, sep = "-")

# Combine values into single data frame using 'match' function
ki_fish_sum$BM_coral <- ki_BM_coral$BM_coral[match(ki_fish_sum$dso, ki_BM_coral$dso)]
ki_fish_sum$BM_det <- ki_BM_det$BM_det[match(ki_fish_sum$dso, ki_BM_det$dso)]
ki_fish_sum$BM_gen <- ki_BM_gen$BM_gen[match(ki_fish_sum$dso, ki_BM_gen$dso)]
ki_fish_sum$BM_herb <- ki_BM_herb$BM_herb[match(ki_fish_sum$dso, ki_BM_herb$dso)]
ki_fish_sum$BM_inv <- ki_BM_inv$BM_inv[match(ki_fish_sum$dso, ki_BM_inv$dso)]
ki_fish_sum$BM_omn <- ki_BM_omn$BM_omn[match(ki_fish_sum$dso, ki_BM_omn$dso)]
ki_fish_sum$BM_pisc <- ki_BM_pisc$BM_pisc[match(ki_fish_sum$dso, ki_BM_pisc$dso)]
ki_fish_sum$BM_plank <- ki_BM_plank$BM_plank[match(ki_fish_sum$dso, ki_BM_plank$dso)]
ki_fish_sum$AB_total <- ki_AB$AB_total[match(ki_fish_sum$dso, ki_AB$dso)]
ki_fish_sum$AB_coral <- ki_AB_coral$AB_coral[match(ki_fish$dso, ki_AB_coral$dso)]
ki_fish_sum$AB_det <- ki_AB_det$AB_det[match(ki_fish_sum$dso, ki_AB_det$dso)]
ki_fish_sum$AB_gen <- ki_AB_gen$AB_gen[match(ki_fish_sum$dso, ki_AB_gen$dso)]
ki_fish_sum$AB_herb <- ki_AB_herb$AB_herb[match(ki_fish_sum$dso, ki_AB_herb$dso)]
ki_fish_sum$AB_inv <- ki_AB_inv$AB_inv[match(ki_fish_sum$dso, ki_AB_inv$dso)]
ki_fish_sum$AB_omn <- ki_AB_omn$AB_omn[match(ki_fish_sum$dso, ki_AB_omn$dso)]
ki_fish_sum$AB_pisc <- ki_AB_pisc$AB_pisc[match(ki_fish_sum$dso, ki_AB_pisc$dso)]
ki_fish_sum$AB_plank <- ki_AB_plank$AB_plank[match(ki_fish_sum$dso, ki_AB_plank$dso)]
ki_fish_sum$SR_total <- ki_SR$SR_total[match(ki_fish_sum$dso, ki_SR$dso)]

# Assign a value of 0 to any NA values
ki_fish_sum[is.na(ki_fish_sum)] <- 0

# Check for NA values anywhere in the data frame
any(is.na(ki_fish_sum))


## Save and quit
save(ki_fish_sum, file = "KI_fish_data_sum.Rdata")
