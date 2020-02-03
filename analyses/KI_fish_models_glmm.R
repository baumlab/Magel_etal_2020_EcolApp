
# Direct and indirect effects of climate change-amplified pulse heat stress events on coral reef fish communities

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

## Calculate the number of non-zero observations for each trophic group
zeroes <- as.data.frame(colSums(ki_fish_sum[, c(11:18)] != 0))

# Proportion of zeroes for groups with at least one zero observation: Corallivores = 9.2%, Detritivores = 8.0%, 
# Generalist carnivores = 0.6%, Piscivores = 2.5%.


##############################

## Fit models for reef fish biomass, abundance, and species richness


###############
### BIOMASS ###
###############

## All models are fit with a Gamma distribution, using the 'log' link function.

## As the Gamma distribution cannot take zero values, half of the minimum non-zero value is added to the zeroes for the 
## corallivore, detritivore, generalist carnivore, and piscivore models.


## Create function to add half of the minimum non-zero value to zeroes
gammadd <- function(x) {
  ifelse(x > 0, x, x + 0.5*min(x[x > 0]))
}

## All fish
model1b <- glmer(BM_total ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model1b), resid(model1b))
summary(model1b)

## Corallivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
model2b <- glmer(gammadd(BM_coral) ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model2b), resid(model2b))
summary(model2b)

## Detritivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
model3b <- glmer(gammadd(BM_det) ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model3b), resid(model3b))
summary(model3b)

## Generalist carnivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
model4b <- glmer(gammadd(BM_gen) ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model4b), resid(model4b))
summary(model4b)

## Herbivores
model5b <- glmer(BM_herb ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model5b), resid(model5b))
summary(model5b)

## Invertivores
model6b <- glmer(BM_inv ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model6b), resid(model6b))
summary(model6b)

## Omnivores
model7b <- glmer(BM_omn ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model7b), resid(model7b))
summary(model7b)

## Piscivores
# This trophic group contains zeroes, therefore the 'gammadd' function is used
model8b <- glmer(gammadd(BM_pisc) ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model8b), resid(model8b))
summary(model8b)

## Planktivores
model9b <- glmer(BM_plank ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = Gamma(link = "log"), 
                 control = glmerControl(optimizer = "bobyqa"))
plot(fitted(model9b), resid(model9b))
summary(model9b)


#################
### ABUNDANCE ###
#################

## All models are fit with a negative binomial distribution.

## Both the corallivore and detritivore functional groups contain over 5% zeroes. Therefore, we fit zero-inflated negative
## binomial models for both of these groups, and compared them to the resular negative binomial models using AIC to 
## determine whether accounting for zero inflation improved model fit.


## All fish
model1a <- glmmadmb(AB_total ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model1a), resid(model1a))
summary(model1a)

## Corallivores
# Regular model
model2a <- glmmadmb(AB_coral ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                    data = ki_fish_sum, family = "nbinom")
plot(fitted(model2a), resid(model2a))
summary(model2a)
# Zero-inflated model
model2az <- glmmadmb(AB_coral ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, zeroInflation = TRUE, family = "nbinom")
plot(fitted(model2az), resid(model2az))
summary(model2az)
# Compare fit with AIC
AIC(model2a, model2az) # The non-zero-inflated model provides a better fit for the corallivores.

## Detritivores
# Regular model
model3a <- glmmadmb(AB_det ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                    data = ki_fish_sum, family = "nbinom")
plot(fitted(model3a), resid(model3a))
summary(model3a)
# Zero-inflated model
model3az <- glmmadmb(AB_det ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, zeroInflation = TRUE, family = "nbinom")
plot(fitted(model3az), resid(model3az))
summary(model3az)
# Compare fit with AIC
AIC(model3a, model3az) # The zero-inflated model provides a better fit for the detritivores.

## Generalist carnivores
model4a <- glmmadmb(AB_gen ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model4a), resid(model4a))
summary(model4a)

## Herbivores
model5a <- glmmadmb(AB_herb ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model5a), resid(model5a))
summary(model5a)

## Invertivores
model6a <- glmmadmb(AB_inv ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model6a), resid(model6a))
summary(model6a)

## Omnivores
model7a <- glmmadmb(AB_omn ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model7a), resid(model7a))
summary(model7a)

## Piscivores
model8a <- glmmadmb(AB_pisc ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model8a), resid(model8a))
summary(model8a)

## Planktivores
model9a <- glmmadmb(AB_plank ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                   data = ki_fish_sum, family = "nbinom")
plot(fitted(model9a), resid(model9a))
summary(model9a)


########################
### SPECIES RICHNESS ###
########################

## The model is fit with a Poisson distribution, using the 'log' link function.


## All fish
model1s <- glmer(SR_total ~ heat * fp.cont.z + npp.max.z + poly(time.poly.z, 2) + lunar.sine.z + (1|site) + (1|observer), 
                 data = ki_fish_sum, family = poisson(link = "log"))
plot(fitted(model1s), resid(model1s))
summary(model1s)


