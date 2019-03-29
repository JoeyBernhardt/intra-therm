

### globetherm traits

library(tidyverse)
library(janitor)

gtraits <- read_csv("data-raw/globtherm_traits_collated_180617.csv") %>% 
	clean_names()


globetherm_species <- gtraits %>% 
	unite(genus_species, genus, species, sep = " ") %>% 
	distinct(genus_species) %>% 
	filter(!is.na(genus_species))


intratherm <- read_csv("data-processed/combined-thermal-limits.csv") %>% 
	select(genus_species) %>% 
	distinct() %>% 
	filter(!is.na(genus_species))

gspecies <- globetherm_species$genus_species
ispecies <- intratherm$genus_species

overlapping_species <- intersect(gspecies, ispecies)
non_overlapping_species <- setdiff(ispecies, gspecies)

library(fuzzyjoin)

fjoined <- gspecies %>%
	stringdist_inner_join(ispecies, by = "genus_species", max_dist = 1, distance_col = "distance")
		   

fjoined <- stringdist_left_join(intratherm, globetherm_species, by = c(genus_species = "genus_species"), max_dist = 2)



ispecies_sub <- ispecies[3]

library(taxize)


classification(ispecies_sub, db = 'itis')
classes <- tax_name(query = ispecies, get = "class", db = "itis")
classes2 <- tax_name(query = ispecies, get = "class", db = "both")

write_csv(classes2, "data-processed/intratherm-classes.csv")
