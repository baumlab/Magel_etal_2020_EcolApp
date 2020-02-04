
# Direct and indirect effects of climate change-amplified pulse heat stress events on coral reef fish communities

# Authors: Jennifer M.T. Magel [1], Sean A. Dimoff [1], Julia K. Baum [1,2]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, Hawai'i, 96744, USA
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script to create Fig. 4 (multi-panel figure plotting changes in overall reef fish community structure and the 
# structure of each trophic group across the heat stress event, using multivariate ordination [PCoA])


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

## Load environmental data
load("SS_full_meta.Rdata")
# Note that all of the trophic groups aside from corallivores and detritivores have the same number of observations as
# the full data set; the metadata from the full data set can therefore be applied to these trophic groups, since the
# observations for all trophic groups are in the same order
load("SS_coral_meta.Rdata")
load("SS_det_meta.Rdata")

## Set seed for random processes
set.seed(9)

## Set colours for time points
timecols <- c("#006699", "#FF6666", "#330099")
palette(timecols)


#############
### PLOTS ###
#############

## Set desired specifications (size, etc.) for the resulting figure
# Specify the desired output folder at "..."
jpeg(filename = ".../Figure_4.jpg",
     width = 180, height = 200, units = "mm", res = 600)
par(mfrow = c(3,3), mar = c(1,1,2.5,1), oma = c(0.5,1,1,1))


## All Fish
# Compute Bray-Curtis dissimilarity indices using the site x species matrix
dist.all <- vegdist(ss.full, method = "bray")
# Calculate multivariate dispersions
PCoA.all <- betadisper(d = dist.all, group = ss.full.meta$heat, type = "centroid")
# Plot PCoA ordination
plot(PCoA.all, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("All fish", line = 1, cex.main = 1.5, font.main = 1)
title("(a)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.all, ss.full.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
legend("topright", bty = "n", pch = c(0,1,2), cex = 1.25,
       legend = levels(ss.full.meta$heat),
       col = timecols)
text(0.47, -0.365, "p = 0.001", cex = 1.25)


## Herbivores
dist.herb <- vegdist(ss.herb, method = "bray")
PCoA.herb <- betadisper(d = dist.herb, group = ss.full.meta$heat, type = "centroid")

plot(PCoA.herb, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Herbivores", line = 1, cex.main = 1.5, font.main = 1)
title("(b)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.herb, ss.full.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(0.50, -0.335, "p = 0.001", cex = 1.25)


## Invertivores
dist.inv <- vegdist(ss.inv, method = "bray")
PCoA.inv <- betadisper(d = dist.inv, group = ss.full.meta$heat, type = "centroid")

plot(PCoA.inv, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Invertivores", line = 1, cex.main = 1.5, font.main = 1)
title("(c)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.inv, ss.full.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(-0.2625, -0.32, "p = 0.001", cex = 1.25)


## Planktivores
dist.plank <- vegdist(ss.plank, method = "bray")
PCoA.plank <- betadisper(d = dist.plank, group = ss.full.meta$heat, type = "centroid")

plot(PCoA.plank, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Planktivores", line = 1, cex.main = 1.5, font.main = 1)
title("(d)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.plank, ss.full.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(0.41, -0.3925, "p = 0.001", cex = 1.25)


## Piscivores
dist.pisc <- vegdist(ss.pisc, method = "bray")
PCoA.pisc <- betadisper(d = dist.pisc, group = ss.full.meta$heat, type = "centroid")

plot(PCoA.pisc, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Piscivores", line = 1, cex.main = 1.5, font.main = 1)
title("(e)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.pisc, ss.full.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(-0.31, -0.55, "p = 0.031", cex = 1.25)


## Omnivores
dist.omn <- vegdist(ss.omn, method = "bray")
PCoA.omn <- betadisper(d = dist.omn, group = ss.full.meta$heat, type = "centroid")

plot(PCoA.omn, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Omnivores", line = 1, cex.main = 1.5, font.main = 1)
title("(f)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.omn, ss.full.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(-0.31, -0.35, "p = 0.002", cex = 1.25)


## Generalist carnivores
dist.gen <- vegdist(ss.gen, method = "bray")
PCoA.gen <- betadisper(d = dist.gen, group = ss.full.meta$heat, type = "centroid")

plot(PCoA.gen, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Gen. carnivores", line = 1, cex.main = 1.5, font.main = 1)
title("(g)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.gen, ss.full.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(-0.225, -0.535, "p = 0.002", cex = 1.25)


## Corallivores
dist.coral <- vegdist(ss.coral, method = "bray")
PCoA.coral <- betadisper(d = dist.coral, group = ss.coral.meta$heat, type = "centroid")

plot(PCoA.coral, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Corallivores", line = 1, cex.main = 1.5, font.main = 1)
title("(h)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.coral, ss.coral.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(0.49, -0.415, "p = 0.001", cex = 1.25)


## Detritivores
dist.det <- vegdist(ss.det, method = "bray")
PCoA.det <- betadisper(d = dist.det, group = ss.det.meta$heat, type = "centroid")

plot(PCoA.det, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(0,1,2),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
title("Detritivores", line = 1, cex.main = 1.5, font.main = 1)
title("(i)", line = 1, adj = 0, cex.main = 1.5, font.main = 2)
ordihull(PCoA.det, ss.det.meta$heat,
         draw = c("polygon"), col = timecols, alpha = 0.3, 
         border = timecols)
text(0.225, -0.5, "p = 0.009", cex = 1.25)


dev.off()
