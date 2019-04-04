#Goals: 
#1 using all of the cadillac data, extract a value of "local differentiation" for each species
#2 using all of the cadillac data, extract a value of "acclimation response" for each species

library(tidyverse)
library(cowplot)
library(broom)

intratherm<-read_csv("data-processed/intratherm-cadillac-limits-traits.csv")
names(intratherm)

intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>%
	ggplot(aes(x = acclim_temp, y = parameter_value, color = latitude)) + 
	geom_point() + geom_line() +  
	ylab("CTmax") + xlab("Acclimation")

intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>%
	group_by(genus_species) %>% 
	do(tidy(lm(parameter_value~acclim_temp+latitude, data=.))) %>% View

