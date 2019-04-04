#Goals: 
#1 using all of the cadillac data, extract a value of "local differentiation" for each species
#2 using all of the cadillac data, extract a value of "acclimation response" for each species

library(tidyverse)
library(cowplot)
library(broom)

intratherm<-read_csv("data-processed/intratherm-cadillac-limits-traits.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))

intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>%
	ggplot(aes(x = acclim_temp, y = parameter_value, color = latitude)) + 
	geom_point() + 
	ylab("CTmax") + xlab("Acclimation")

model<-intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(population_id) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.))) %>% View


	
temperatures <- read_csv("data-raw/IntTh.dailymax.AVGTEMP.csv")

temp_long <- temperatures %>%
	gather(key = day, value = temperature, 4:368) %>%
	group_by(latitude, longitude) %>%
	summarise(sd_temperature = sd(temperature),
			  range_temperature = max(temperature)-min(temperature),
			  interquantile_temp = quantile(temperature, probs=0.95)-quantile(temperature, probs=0.05))

intratherm_temps <- left_join(intratherm, temp_long, by = c("latitude", "longitude"))

#now ask how the slope (acclimation) is predicted by CV of temperature:
intratherm_acclimation<-model %>% filter(term=="acclim_temp") %>%
	left_join(.,  intratherm_temps, by = "population_id") %>% 
	rename(acclimation_slope=estimate, acclimation.sd=std.error)

intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	ggplot(aes(x = interquantile_temp, y = acclimation_slope, color = abs(latitude))) + 
	geom_point() + 
	geom_errorbar(aes(ymax=acclimation_slope+acclimation.sd, ymin=acclimation_slope-acclimation.sd)) +
	geom_smooth(method=lm)

intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	ungroup() %>%
	do(glance(lm(acclimation_slope~interquantile_temp, data=.)))
#later ask how the differentiation (intercept) is predicted by differences in the temperatures:


	
#intercept diff
