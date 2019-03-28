
library(tidyverse)
library(broom)
library(janitor)
library(cowplot)
library(readxl)
library(stringr)


### New multipop data




intra <- read_csv("data-raw/Globtherm2_within_species_2018_12_17.csv") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "group")

so <- read_excel("data-raw/Globtherm2_within_species_SO.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "SO")

fl <- read_excel("data-raw/Globtherm2_within_species_FL.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "FL")

ab <- read_excel("data-raw/Globtherm2_within_species_AB.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "AB")

fv <- read_excel("data-raw/Globtherm2_FV_Test.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "FV")

all_mult <- bind_rows(so, ab, fl, intra, fv)

all_mult2 <- all_mult %>% 
	mutate(parameter_value = str_replace(parameter_value, "<", "")) %>% 
	mutate(parameter_value = as.numeric(parameter_value)) %>% 
	mutate(error_estimate = as.numeric(error_estimate)) %>% 
	mutate(lat_of_collection = as.numeric(lat_of_collection)) %>% 
	mutate(original_compilation = "intratherm_team") %>% 
	rename(latitude = lat_of_collection,
		   longitude = long_of_collection) %>% 
	mutate(latitude = as.numeric(latitude),
		   longitude = as.numeric(longitude)) %>% 
	rename(acclim_temp = pretreatment_temp) %>% 
	rename(acclim_time = pretreatment_duration) %>% 
	mutate(acclim_temp = as.numeric(acclim_temp)) %>% 
	mutate(ramping_rate = as.numeric(ramping_rate)) %>% 
	mutate(n_cat = NA) %>% 
	mutate(n_cat = ifelse(grepl(">", sample_size), sample_size, n_cat)) %>%
	mutate(n_cat = ifelse(grepl("<", sample_size), sample_size, n_cat)) %>%
	mutate(n_cat = ifelse(grepl("-", sample_size), sample_size, n_cat)) %>% 
	mutate(sample_size = ifelse(!grepl("[^0-9]", sample_size), sample_size, NA)) %>% 
	mutate(sample_size = as.numeric(sample_size))

unique(all_mult2$n_cat)

all_mult2 %>% 
	filter(sample_size == "15-Oct") %>% View


all_mult2 %>% 
	ggplot(aes(x = lat_of_collection, y = parameter_value, color = parameter_tmax_or_tmin)) + geom_point()



names(all_mult2)

rohr <- read_csv("data-processed/rohr_amphib_multi_pop.csv")

rohr2 <- rohr %>% 
	mutate(parameter_tmax_or_tmin = "tmax") %>% 
	rename(parameter_value = raw_ctm1) %>% 
	# select(genus1, species1, parameter_tmax_or_tmin, parameter_value, acclim_time, acclim_temp) %>% 
	rename(genus = genus1,
		   species = species1,
		   ramping_rate = heating_rate) %>% 
	mutate(original_compilation = "Rohr") %>% 
	mutate(acclim_time = as.character(acclim_time)) %>% 
	rename(life_stage = stage) %>% 
	rename(sample_size = n_numb) %>% 
	rename(realm_general = habitat)


mult_species <- all_mult2 %>% 
	distinct(genus, species)

rohr_species <- rohr %>% 
	distinct(genus1, species1) %>% 
	rename(genus = genus1,
		   species = species1)

comte_species <- read_csv("data-processed/comte_fish_multi_pop.csv") %>% 
	clean_names() %>% 
	separate(species, into = c("genus", "species")) %>% 
	distinct(genus, species)


comte <- read_csv("data-processed/comte_fish_multi_pop.csv") %>% 
	clean_names() %>% 
	separate(species, into = c("genus", "species")) %>% 
	mutate(parameter_tmax_or_tmin = "tmax") %>% 
	rename(acclim_temp = temperature_of_acclimation_c) %>% 
	rename(acclim_time = length_acclimation_period_days,
		   ramping_rate = heating_rate_c_min) %>%
	# select(family, genus, species, latitude, longitude, thermal_limit_c,
	# 	   sd_thermal_limit, nindividuals, parameter_tmax_or_tmin,
	# 	   acclim_temp, acclim_time, acclimation, rate_acclimation) %>% 
	rename(parameter_value = thermal_limit_c) %>% 
	mutate(original_compilation = "Comte") %>% 
	mutate(acclim_time = as.character(acclim_time)) %>% 
	mutate(reference = as.character(reference)) %>% 
	rename(realm_general = realm_affinity) %>% 
	rename(sample_size = nindividuals) %>% 
	mutate(error_type = "SD") %>% 
	rename(error_estimate = sd_thermal_limit)
	

all_species <- bind_rows(mult_species, rohr_species, comte_species) %>% 
	distinct(genus, species)

write_csv(all_species, "data-processed/intratherm-species-list.csv")



# merge all the datasets --------------------------------------------------

unique(all_mult2$ramping_rate)

all_mult2 %>% 
	group_by(phylum) %>% tally()


combined_tmax <- bind_rows(all_mult2, rohr2, comte) %>% 
	filter(!is.na(parameter_value)) %>% 
	mutate(realm_general2 = case_when(realm_general == "aquatic" ~ "Aquatic",
									  realm_general == "marine" ~ "Marine",
									  realm_general == "marine littoral" ~ "Marine",
									  realm_general == "terrestrial" ~ "Terrestrial",
									  realm_general == "Arboreal" ~ "Terrestrial",
									  realm_general == "freshwater" ~ "Freshwater",
									  realm_general == "freshwater native" ~ "Freshwater",
									  TRUE ~ realm_general)) %>% 
	mutate(realm_general3 = case_when(realm_general2 %in% c("Aquatic", "Aquatic & terrestrial", "Freshwater", 'Marine') ~ "Aquatic",
									  realm_general2 == "Terrestrial" ~ "Terrestrial"))

write_csv(combined_tmax, "data-processed/combined-tmax.csv")


names(combined_tmax)

combined_tmax %>% 
	filter(is.na(realm_general3)) %>% View


combined_tmax %>% 
	ggplot(aes(x = latitude, y = parameter_value, color = realm_general3)) + geom_point() +
	ylab("Thermal limit (°C)") + xlab("Latitude") + facet_grid(realm_general3 ~ parameter_tmax_or_tmin)


combined_tmax %>% 
	filter(is.na(realm_general)) %>% View

unique(combined_tmax$phylum)

### ARR is the slope of the relationship between CTmax and acclimation temp, PRR is the slope of the 
### relationship between temperature at collection site and CTmax
