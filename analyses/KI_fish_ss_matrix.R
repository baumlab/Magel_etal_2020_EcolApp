
# Pulse heat stress events foreshadow long-term climate change impacts on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script used to create site x species matrices for total biomass and for the biomass of each reef fish functional group


##############################

# Load necessary packages
library(reshape)

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

# Create separate data frame for each functional group
ki_coral <- ki_fish[ki_fish$trophic == "Corallivore", ]
ki_det <- ki_fish[ki_fish$trophic == "Detritivore", ]
ki_gen <- ki_fish[ki_fish$trophic == "Generalist carnivore", ]
ki_herb <- ki_fish[ki_fish$trophic == "Herbivore", ]
ki_inv <- ki_fish[ki_fish$trophic == "Invertivore", ]
ki_omn <- ki_fish[ki_fish$trophic == "Omnivore", ]
ki_pisc <- ki_fish[ki_fish$trophic == "Piscivore", ]
ki_plank <- ki_fish[ki_fish$trophic == "Planktivore", ]

# Check that no data was lost
dim(ki_fish)[1]; dim(ki_coral)[1] + dim(ki_det)[1] + dim(ki_gen)[1] + dim(ki_herb)[1] + dim(ki_inv)[1] + dim(ki_omn)[1] + dim(ki_pisc)[1] + dim(ki_plank)[1]


##############################

### ALL FISH ###
# Sum biomass by site
ki_BM <- ki_fish %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.full <- cast(ki_BM, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                     value = "biomass", FUN = sum)
ss.full <- as.data.frame(ss.full)
ss.full[is.na(ss.full)] <- 0

# Create separate data frames for species and site data
ss.full.meta <- ss.full[, c(1:6)]
ss.full <- ss.full[, c(7:264)]


### CORALLIVORES ###
# Sum biomass by site
ki_BM_coral <- ki_coral %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.coral <- cast(ki_BM_coral, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                      value = "biomass", FUN = sum)
ss.coral <- as.data.frame(ss.coral)
ss.coral[is.na(ss.coral)] <- 0

# Create separate data frames for species and site data
ss.coral.meta <- ss.coral[, c(1:6)]
ss.coral <- ss.coral[, c(7:26)]


### DETRITIVORES ###
# Sum biomass by site
ki_BM_det <- ki_det %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.det <- cast(ki_BM_det, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                    value = "biomass", FUN = sum)
ss.det <- as.data.frame(ss.det)
ss.det[is.na(ss.det)] <- 0

# Create separate data frames for species and site data
ss.det.meta <- ss.det[, c(1:6)]
ss.det <- ss.det[, c(7:12)]


### GENERALIST CARNIVORES ###
# Sum biomass by site
ki_BM_gen <- ki_gen %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.gen <- cast(ki_BM_gen, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                    value = "biomass", FUN = sum)
ss.gen <- as.data.frame(ss.gen)
ss.gen[is.na(ss.gen)] <- 0

# Create separate data frames for species and site data
ss.gen.meta <- ss.gen[, c(1:6)]
ss.gen <- ss.gen[, c(7:28)]


### HERBIVORES ###
# Sum biomass by site
ki_BM_herb <- ki_herb %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.herb <- cast(ki_BM_herb, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                     value = "biomass", FUN = sum)
ss.herb <- as.data.frame(ss.herb)
ss.herb[is.na(ss.herb)] <- 0

# Create separate data frames for species and site data
ss.herb.meta <- ss.herb[, c(1:6)]
ss.herb <- ss.herb[, c(7:51)]


### INVERTIVORES ###
# Sum biomass by site 
ki_BM_inv <- ki_inv %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.inv <- cast(ki_BM_inv, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                    value = "biomass", FUN = sum)
ss.inv <- as.data.frame(ss.inv)
ss.inv[is.na(ss.inv)] <- 0

# Create separate data frames for species and site data
ss.inv.meta <- ss.inv[, c(1:6)]
ss.inv <- ss.inv[, c(7:72)]


### OMNIVORES ###
# Sum biomass by site 
ki_BM_omn <- ki_omn %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.omn <- cast(ki_BM_omn, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                    value = "biomass", FUN = sum)
ss.omn <- as.data.frame(ss.omn)
ss.omn[is.na(ss.omn)] <- 0

# Create separate data frames for species and site data
ss.omn.meta <- ss.omn[, c(1:6)]
ss.omn <- ss.omn[, c(7:22)]


### PISCIVORES ###
# Sum biomass by site
ki_BM_pisc <- ki_pisc %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.pisc <- cast(ki_BM_pisc, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                     value = "biomass", FUN = sum)
ss.pisc <- as.data.frame(ss.pisc)
ss.pisc[is.na(ss.pisc)] <- 0

# Create separate data frames for species and site data
ss.pisc.meta <- ss.pisc[, c(1:6)]
ss.pisc <- ss.pisc[, c(7:47)]


### PLANKTIVORES ###
# Sum biomass by site 
ki_BM_plank <- ki_plank %>% group_by(heat, year, ki.date, site, f.pressure, time_of_day, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.plank <- cast(ki_BM_plank, heat + year + ki.date + site + f.pressure + time_of_day ~ species, 
                      value = "biomass", FUN = sum)
ss.plank <- as.data.frame(ss.plank)
ss.plank[is.na(ss.plank)] <- 0

# Create separate data frames for species and site data
ss.plank.meta <- ss.plank[, c(1:6)]
ss.plank <- ss.plank[, c(7:48)]


##############################

## Save and quit
# All fish
save(ss.full, file = "multivariate/SS_full.Rdata")
save(ss.full.meta, file = "multivariate/SS_full_meta.Rdata")
# Corallivores
save(ss.coral, file = "multivariate/SS_coral.Rdata")
save(ss.coral.meta, file = "multivariate/SS_coral_meta.Rdata")
# Detritivores
save(ss.det, file = "multivariate/SS_det.Rdata")
save(ss.det.meta, file = "multivariate/SS_det_meta.Rdata")
# Generalist carnivores
save(ss.gen, file = "multivariate/SS_gen.Rdata")
save(ss.gen.meta, file = "multivariate/SS_gen_meta.Rdata")
# Herbivores
save(ss.herb, file = "multivariate/SS_herb.Rdata")
save(ss.herb.meta, file = "multivariate/SS_herb_meta.Rdata")
# Invertivores
save(ss.inv, file = "multivariate/SS_inv.Rdata")
save(ss.inv.meta, file = "multivariate/SS_inv_meta.Rdata")
# Omnivores
save(ss.omn, file = "multivariate/SS_omn.Rdata")
save(ss.omn.meta, file = "multivariate/SS_omn_meta.Rdata")
# Piscivores
save(ss.pisc, file = "multivariate/SS_pisc.Rdata")
save(ss.pisc.meta, file = "multivariate/SS_pisc_meta.Rdata")
# Planktivores
save(ss.plank, file = "multivariate/SS_plank.Rdata")
save(ss.plank.meta, file = "multivariate/SS_plank_meta.Rdata")
