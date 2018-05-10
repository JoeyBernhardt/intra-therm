

library(tidyverse)
library(janitor)


amphib <- read_csv("data-raw/amphib-rohr.csv") %>% 
	clean_names()


am2 <- amphib %>% 
	unite(col = genus_species, sep = " ", remove = FALSE, genus3, species3) %>% 
	unite(col = record_species, sep = "_", remove = FALSE, record, genus_species) 

mult_pop <- am2 %>% 
	distinct(genus_species, latitude) %>% 
	group_by(genus_species) %>% 
	tally() %>%
	filter(n > 1) %>% 
	select(genus_species)

amphib_multi_pop <- am2 %>% 
	filter(genus_species %in% mult_pop$genus_species) 

write_csv(amphib_multi_pop, "data-processed/rohr_amphib_multi_pop.csv")
