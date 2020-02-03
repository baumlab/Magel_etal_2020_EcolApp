
# Direct and indirect effects of climate change-amplified pulse heat stress events on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script used to create site x species matrices for total biomass and for the biomass of each reef fish functional group


##############################

# Load necessary packages
library(reshape)

# Set your working directory
# Make sure that this contains the "ki_full_data_raw.Rdata" file
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

# Load the data
load("ki_fish_data_raw.Rdata")

## Additional data cleaning
# Double number of small fish to account for smaller survey area (300m^2 compared to 600m^2 for large fish))
ki_small <- ki_full[ki_full$length < 20, ]
ki_large <- ki_full[ki_full$length >= 20, ]
ki_small$number <- ki_small$number*2
ki_full <- rbind(ki_small, ki_large)
# Calculate biomass
ki_full$biomass <- ki_full$number * ki_full$mass

# Create separate data frame for each functional group
ki_coral <- ki_full[ki_full$trophic == "Corallivore", ]
ki_det <- ki_full[ki_full$trophic == "Detritivore", ]
ki_gen <- ki_full[ki_full$trophic == "Generalist carnivore", ]
ki_herb <- ki_full[ki_full$trophic == "Herbivore", ]
ki_inv <- ki_full[ki_full$trophic == "Invertivore", ]
ki_omn <- ki_full[ki_full$trophic == "Omnivore", ]
ki_pisc <- ki_full[ki_full$trophic == "Piscivore", ]
ki_plank <- ki_full[ki_full$trophic == "Planktivore", ]

# Check that no data was lost
dim(ki_full)[1]; dim(ki_coral)[1] + dim(ki_det)[1] + dim(ki_gen)[1] + dim(ki_herb)[1] + dim(ki_inv)[1] + dim(ki_omn)[1] + dim(ki_pisc)[1] + dim(ki_plank)[1]


##############################

### ALL FISH ###
# Sum biomass by site
ki_BM <- ki_full %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.full <- cast(ki_BM, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z ~ species, 
                     value = "biomass", FUN = sum)
ss.full <- as.data.frame(ss.full)
ss.full[is.na(ss.full)] <- 0

# Create separate data frames for species and site data
ss.full.meta <- ss.full[, c(1:8)]
ss.full <- ss.full[, c(9:266)]


### CORALLIVORES ###
# Sum biomass by site
ki_BM_coral <- ki_coral %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.coral <- cast(ki_BM_coral, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z~ species, 
                      value = "biomass", FUN = sum)
ss.coral <- as.data.frame(ss.coral)
ss.coral[is.na(ss.coral)] <- 0

# Create separate data frames for species and site data
ss.coral.meta <- ss.coral[, c(1:8)]
ss.coral <- ss.coral[, c(9:28)]


### DETRITIVORES ###
# Sum biomass by site
ki_BM_det <- ki_det %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.det <- cast(ki_BM_det, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z~ species, 
                    value = "biomass", FUN = sum)
ss.det <- as.data.frame(ss.det)
ss.det[is.na(ss.det)] <- 0

# Create separate data frames for species and site data
ss.det.meta <- ss.det[, c(1:8)]
ss.det <- ss.det[, c(9:14)]


### GENERALIST CARNIVORES ###
# Sum biomass by site
ki_BM_gen <- ki_gen %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.gen <- cast(ki_BM_gen, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z ~ species, 
                    value = "biomass", FUN = sum)
ss.gen <- as.data.frame(ss.gen)
ss.gen[is.na(ss.gen)] <- 0

# Create separate data frames for species and site data
ss.gen.meta <- ss.gen[, c(1:8)]
ss.gen <- ss.gen[, c(9:30)]


### HERBIVORES ###
# Sum biomass by site
ki_BM_herb <- ki_herb %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.herb <- cast(ki_BM_herb, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z ~ species, 
                     value = "biomass", FUN = sum)
ss.herb <- as.data.frame(ss.herb)
ss.herb[is.na(ss.herb)] <- 0

# Create separate data frames for species and site data
ss.herb.meta <- ss.herb[, c(1:8)]
ss.herb <- ss.herb[, c(9:53)]


### INVERTIVORES ###
# Sum biomass by site 
ki_BM_inv <- ki_inv %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.inv <- cast(ki_BM_inv, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z ~ species, 
                    value = "biomass", FUN = sum)
ss.inv <- as.data.frame(ss.inv)
ss.inv[is.na(ss.inv)] <- 0

# Create separate data frames for species and site data
ss.inv.meta <- ss.inv[, c(1:8)]
ss.inv <- ss.inv[, c(9:74)]


### OMNIVORES ###
# Sum biomass by site 
ki_BM_omn <- ki_omn %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.omn <- cast(ki_BM_omn, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z ~ species, 
                    value = "biomass", FUN = sum)
ss.omn <- as.data.frame(ss.omn)
ss.omn[is.na(ss.omn)] <- 0

# Create separate data frames for species and site data
ss.omn.meta <- ss.omn[, c(1:8)]
ss.omn <- ss.omn[, c(9:24)]


### PISCIVORES ###
# Sum biomass by site
ki_BM_pisc <- ki_pisc %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.pisc <- cast(ki_BM_pisc, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z ~ species, 
                     value = "biomass", FUN = sum)
ss.pisc <- as.data.frame(ss.pisc)
ss.pisc[is.na(ss.pisc)] <- 0

# Create separate data frames for species and site data
ss.pisc.meta <- ss.pisc[, c(1:8)]
ss.pisc <- ss.pisc[, c(9:49)]


### PLANKTIVORES ###
# Sum biomass by site 
ki_BM_plank <- ki_plank %>% group_by(heat, year, ki.date, site, fp.cont.z, npp.max.z, time.poly.z, lunar.sine.z, species) %>% 
  summarise(biomass = sum(biomass)/300)

## Cast data into a site x species matrix
ss.plank <- cast(ki_BM_plank, heat + year + ki.date + site + fp.cont.z + npp.max.z + time.poly.z + lunar.sine.z ~ species, 
                      value = "biomass", FUN = sum)
ss.plank <- as.data.frame(ss.plank)
ss.plank[is.na(ss.plank)] <- 0

# Create separate data frames for species and site data
ss.plank.meta <- ss.plank[, c(1:8)]
ss.plank <- ss.plank[, c(9:50)]


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
