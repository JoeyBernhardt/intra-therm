

library(tidyverse)
library(broom)
library(janitor)
library(cowplot)

rohr <- read_csv("data-processed/rohr_amphib_multi_pop.csv")
comte <- read_csv("data-processed/comte_fish_multi_pop.csv") %>% 
	clean_names()


rohr %>% 
	group_by(genus_species) %>% 
do(tidy(lm(raw_ctm1 ~ acclim_temp, data = .), conf.int = TRUE)) %>% View


comte2 <- comte %>% 
	rename(ctmax = thermal_limit_c,
		   ctmax_sd = sd_thermal_limit,
		   genus_species = species,
		   acclim_temp = temperature_of_acclimation_c,
		   heating_rate = heating_rate_c_min) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_"))
	
	
rohr2 <- rohr %>% 
	rename(ctmax = raw_ctm1)




### create exposure time
comte2$exposuretime <- (comte2$ctmax- comte2$acclim_temp)/comte2$heating_rate


### calculate ARR

arr <- comte2 %>% 
	group_by(genus_species,lat_long, latitude) %>%
	do(tidy(lm(ctmax ~ acclim_temp, data = .), conf.int = TRUE)) %>% 
	filter(term == "acclim_temp") %>% 
	rename(arr = estimate)
	
arr %>% 
	filter(arr > 0) %>% 
	ggplot(aes(x = latitude, y = arr)) + geom_point() +
	ylab("ARR") + xlab("Latitude")
ggsave("figures/ARR_comte_lat.pdf", width = 5, height = 5)
