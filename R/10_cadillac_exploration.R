
library(tidyverse)
library(janitor)

#### cadillac dataset messing around

cadillac <- read_csv("data-processed/intratherm-multi-pop-multi-acclim.csv")
traits <- read_csv("data-raw/intratherm-traits.csv") %>% 
	clean_names() 

	
cadillac_locations <- cadillac %>% 
	select(genus_species, latitude, longitude) %>% 
	distinct(genus_species, latitude, longitude)

write_csv(cadillac_locations, "data-processed/intratherm_cadillac_locations.csv")
