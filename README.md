****

Data and R code accompanying:

**Direct and indirect effects of climate change-amplified pulse heat stress events on coral reef fish communities**

Authors: Jennifer M.T. Magel, Sean A. Dimoff, Julia K. Baum

****

In the **[data](data/)** folder, [KI_fish_data_raw.Rdata](data/KI_fish_data_raw.Rdata) contains observation-level data for all fish observed in our underwater visual census surveys. Variables are defined as follows:

* ```ki.date``` = Date (DD_MM_YYYY) that the data was collected on Kiritimati (GMT+14)
* ```year``` = Year that the data was collected
* ```site``` = Site number
* ```transect``` = Transect number
* ```observer``` =  Initials of the observer who conducted the survey
* ```species``` = Scientific name
* ```trophic``` = Trophic group assigned to each species (see AppendixS1: Table S3)
* ```size``` = Estimated length of the observed fish in cm
* ```number``` = Number of individuals observed (of a particular length)
* ```lw_a``` = Length-weight parameter 'a'
* ```lw_b``` = Length-weight parameter 'b'
* ```lcf``` = Length conversion factor
* ```length``` = Actual length of the fish (size x lcf)
* ```mass``` = Body mass of the fish (lw_a x (length ^ lw_b))
* ```heat``` = Heat stress time point; indicates whether data were collected before (2011, 2013), during (2015), or after (2017) the 2015-2016 El Niño
* ```f.pressure``` = Estimate of local human disturbance at each site as a categorical variable
* ```fp.cont``` = Estimate of local human disturbance at each site as a continuous variable
* ```npp.max``` = Maximum local primary productivity at each site (mg C m^-2 day^-1)
* ```survey_time``` = Start time for the first transect in the survey
* ```time.poly``` = Survey time converted into a continuous variable, to be modelled as a polynomial
* ```lunar.day``` = Lunar day (calculated from ki.date using the 'lunar' package)
* ```lunar.sine``` = Sine of lunar day

Note that variables ending in '.z' are the standardized versions of the continuous predictor variables.

[KI_fish_data_sum.Rdata](data/KI_fish_data_sum.Rdata) contains data on the mean site-level biomass (BM), abundance (AB), and species richness (SR) of the reef fish community as a whole ('total'), and for each individual trophic group (corallivores = 'coral', detritivores = 'det', generalist carnivores = 'gen', herbivores = 'herb', invertivores = 'inv', omnivores = 'omn', piscivores = 'pisc', planktivores = 'plank').
