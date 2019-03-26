
library(tidyverse)
library(broom)
library(janitor)
library(cowplot)
library(readxl)


### New multipop data




intra <- read_csv("data-raw/Globtherm2_within_species_2018_12_17.csv") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character))

so <- read_excel("data-raw/Globtherm2_within_species_SO.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character))

fl <- read_excel("data-raw/Globtherm2_within_species_FL.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character))

ab <- read_excel("data-raw/Globtherm2_within_species_AB.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character))

all_mult <- bind_rows(so, ab, fl, intra)




intra %>% 
	# filter(parameter_tmax_or_tmin == "tmax") %>% 
	# group_by(genus_species) %>% 
	# distinct() %>% 
	# tally() %>% View 
	filter(genus_species == "Baccharis_pilularis") %>% 
	ggplot(aes(x = lat_of_collection, y = parameter_value)) + geom_point() + 
	facet_wrap( ~ genus_species)
