
# Direct and indirect effects of climate change-amplified pulse heat stress events on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script to run PERMANOVA and simper analyses for reef fish biomass


##############################

## Load necessary packages
library(vegan)

## Set your working directory to the 'data/multivariate' folder
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

## Load site x species matrices
load("SS_full.Rdata")
load("SS_coral.Rdata")
load("SS_det.Rdata")
load("SS_gen.Rdata")
load("SS_herb.Rdata")
load("SS_inv.Rdata")
load("SS_omn.Rdata")
load("SS_pisc.Rdata")
load("SS_plank.Rdata")

# Load environmental data
load("SS_full_meta.Rdata")
load("SS_coral_meta.Rdata")
load("SS_det_meta.Rdata")
load("SS_gen_meta.Rdata")
load("SS_herb_meta.Rdata")
load("SS_inv_meta.Rdata")
load("SS_omn_meta.Rdata")
load("SS_pisc_meta.Rdata")
load("SS_plank_meta.Rdata")


##############################

# Run PERMANOVAs and simper analyses for the overall fish community ('all fish') and each functional group


## All fish
# Conduct PERMANOVA
adonis(ss.full ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.full.meta, 
       permutations = 999, method = "bray", strata = ss.full.meta$Site)
# Conduct simper analysis
simper.full <- simper(ss.full, ss.full.meta$heat)
summary(simper.full, ordered = TRUE)


## Corallivores
# Conduct PERMANOVA
adonis(ss.coral ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.coral.meta, 
       permutations = 999, method = "bray", strata = ss.coral.meta$Site)
# Conduct simper analysis
simper.coral <- simper(ss.coral, ss.coral.meta$heat)
summary(simper.coral, ordered = TRUE)


## Detritivores
# Conduct PERMANOVA
adonis(ss.det ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.det.meta, 
       permutations = 999, method = "bray", strata = ss.det.meta$Site)
# Conduct simper analysis
simper.det <- simper(ss.det, ss.det.meta$heat)
summary(simper.det, ordered = TRUE)


## Generalist carnivores
# Conduct PERMANOVA
adonis(ss.gen ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.gen.meta, 
       permutations = 999, method = "bray", strata = ss.gen.meta$Site)
# Conduct simper analysis
simper.gen <- simper(ss.gen, ss.gen.meta$heat)
summary(simper.gen, ordered = TRUE)


## Herbivores
# Conduct PERMANOVA
adonis(ss.herb ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.herb.meta, 
       permutations = 999, method = "bray", strata = ss.herb.meta$Site)
# Conduct simper analysis
simper.herb <- simper(ss.herb, ss.herb.meta$heat)
summary(simper.herb, ordered = TRUE)


## Invertivores
# Conduct PERMANOVA
adonis(ss.inv ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.inv.meta, 
       permutations = 999, method = "bray", strata = ss.inv.meta$Site)
# Conduct simper analysis
simper.inv <- simper(ss.inv, ss.inv.meta$heat)
summary(simper.inv, ordered = TRUE)


## Omnivores
# Conduct PERMANOVA
adonis(ss.omn ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.omn.meta, 
       permutations = 999, method = "bray", strata = ss.omn.meta$Site)
# Conduct simper analysis
simper.omn <- simper(ss.omn, ss.omn.meta$heat)
summary(simper.omn, ordered = TRUE)


## Piscivores
# Conduct PERMANOVA
adonis(ss.pisc ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.pisc.meta, 
       permutations = 999, method = "bray", strata = ss.pisc.meta$Site)
# Conduct simper analysis
simper.pisc <- simper(ss.pisc, ss.pisc.meta$heat)
summary(simper.pisc, ordered = TRUE)


## Planktivores
# Conduct PERMANOVA
adonis(ss.plank ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z, data = ss.plank.meta, 
       permutations = 999, method = "bray", strata = ss.plank.meta$Site)
# Conduct simper analysis
simper.plank <- simper(ss.plank, ss.plank.meta$heat)
summary(simper.plank, ordered = TRUE)

