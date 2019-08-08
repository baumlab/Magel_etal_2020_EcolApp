
# Pulse heat stress events foreshadow long-term climate change impacts on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script to fit generalized linear mixed-effects models for reef fish biomass, abundance, and species richness


##############################

## Load necessary packages
library(dplyr)
library(lme4)
library(arm)
library(optimx)
library(glmmADMB) # See http://glmmadmb.r-forge.r-project.org/ for instructions on how to install 'glmmADMB'

## Set your working directory
# Make sure that this contains the "ki_fish_sum_data_sum.Rdata" file
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

## Load the data
load("ki_fish_data_sum.Rdata")

## Standardize continuous predictor variables
ki_fish_sum$npp_max_z <- rescale(ki_fish_sum$npp_max)


##############################

## Calculate mean, standard deviation, and standard error for each fish metric

# Biomass
bm <- aggregate(BM_total ~ heat, ki_fish_sum, mean)
bm.sd <- aggregate(BM_total ~ heat, ki_fish_sum, function(x) sd(x))
bm.se <- aggregate(BM_total ~ heat, ki_fish_sum, function(x) sd(x)/sqrt(length(x)))

# Abundance
ab <- aggregate(AB_total ~ heat, ki_fish_sum, mean)
ab.sd <- aggregate(AB_total ~ heat, ki_fish_sum, function(x) sd(x))
ab.se <- aggregate(AB_total ~ heat, ki_fish_sum, function(x) sd(x)/sqrt(length(x)))

# Species richness
sr <- aggregate(SR_total ~ heat, ki_fish_sum, mean)
sr.sd <- aggregate(SR_total ~ heat, ki_fish_sum, function(x) sd(x))
sr.se <- aggregate(SR_total ~ heat, ki_fish_sum, function(x) sd(x)/sqrt(length(x)))


##############################

## Fit models for reef fish biomass, abundance, and species richness


###############
### BIOMASS ###
###############

## Create function to add half of the minimum non-zero value to zeroes
gammadd <- function(x) {
  ifelse(x > 0, x, x + 0.5*min(x[x > 0]))
}

## All fish
model1b <- glmer(BM_total ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model1b), resid(model1b))
summary(model1b)

## Corallivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
model2b <- glmer(gammadd(BM_coral) ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model2b), resid(model2b))
summary(model2b)

## Detritivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
model3b <- glmer(gammadd(BM_det) ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model3b), resid(model3b))
summary(model3b)

## Generalist carnivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
# Model fit with a different optimizer (optimx, nlminb) to get the model to converge
model4b <- glmer(gammadd(BM_gen) ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb")))
plot(fitted(model4b), resid(model4b))
summary(model4b)

## Herbivores
model5b <- glmer(BM_herb ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model5b), resid(model5b))
summary(model5b)

## Invertivores
# Model fit with a different optimizer (optimx, bobyqa) to get the model to converge
model6b <- glmer(BM_inv ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "optimx", optCtrl = list(method = "bobyqa")))
plot(fitted(model6b), resid(model6b))
summary(model6b)

## Omnivores
model7b <- glmer(BM_omn ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model7b), resid(model7b))
summary(model7b)

## Piscivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
model8b <- glmer(gammadd(BM_pisc) ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model8b), resid(model8b))
summary(model8b)

## Planktivores
# Model fit with a different optimizer (optimx, nlminb) to get the model to converge
model9b <- glmer(BM_plank ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb")))
plot(fitted(model9b), resid(model9b))
summary(model9b)


#################
### ABUNDANCE ###
#################

## All fish
model1a <- glmmadmb(AB_total ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model1a), resid(model1a))
summary(model1a)

## Corallivores
model2a <- glmmadmb(AB_coral ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, zeroInflation = TRUE, family = "nbinom")
plot(fitted(model2a), resid(model2a))
summary(model2a)

## Detritivores
model3a <- glmmadmb(AB_det ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, zeroInflation = TRUE, family = "nbinom")
plot(fitted(model3a), resid(model3a))
summary(model3a)

## Generalist carnivores
model4a <- glmmadmb(AB_gen ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model4a), resid(model4a))
summary(model4a)

## Herbivores
model5a <- glmmadmb(AB_herb ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model5a), resid(model5a))
summary(model5a)

## Invertivores
model6a <- glmmadmb(AB_inv ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model6a), resid(model6a))
summary(model6a)

## Omnivores
model7a <- glmmadmb(AB_omn ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model7a), resid(model7a))
summary(model7a)

## Piscivores
model8a <- glmmadmb(AB_pisc ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model8a), resid(model8a))
summary(model8a)

## Planktivores
model9a <- glmmadmb(AB_plank ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model9a), resid(model9a))
summary(model9a)


########################
### SPECIES RICHNESS ###
########################

## All fish
model1s <- glmer(SR_total ~ heat * f.pressure + npp_max_z + time_of_day + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = poisson(link = "log"))
plot(fitted(model1s), resid(model1s))
summary(model1s)


