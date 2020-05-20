

### initial analysis

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(geosphere)


intratherm <- read_csv("data-processed/intratherm-may-2020-squeaky-clean.csv") %>% 
	mutate(population_id_old = population_id) %>% 
	mutate(population_id = paste(population_id_old, longitude, sep = "_")) 


### how many populations do we have per species?

intratherm %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	distinct(lat_long) %>% 
	tally() %>% 
	ggplot(aes(x = n)) + geom_histogram() +
	ylab("Frequency") + xlab("Number of distinct population locations") +
	scale_x_continuous(breaks = seq(1, 16, by = 1))
ggsave("figures/number-locations.png", width = 6, height = 4)

### from how many studies does each species have data from?

intratherm %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	distinct(ref) %>% 
	tally() %>% 
	ggplot(aes(x = n)) + geom_histogram() +
	ylab("Frequency") + xlab("Number of studies per species") +
	scale_x_continuous(breaks = seq(1, 10, by = 1))
ggsave("figures/number-studies-per-species.png", width = 6, height = 4)


### how to calculate distance between two points?


# distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

### get all the the species that only have two populations

names(intratherm)

intra2 <- intratherm %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	select(elevation_of_collection, everything()) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	select(genus_species, lat_long) %>% 
	distinct(genus_species, lat_long) %>% 
	tally() %>% 
	filter(n == 2)

intra3 <- intratherm %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	filter(genus_species %in% c(intra2$genus_species)) %>% 
	group_by(genus_species) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	distinct(lat_long, .keep_all = TRUE) %>% 
	select(genus_species, latitude, longitude)

intra_split <- intra3 %>% 
	split(.$genus_species) 



get_distance <- function(df){
	distance = distm(c(df[[1]]$longitude[1], df[[1]]$latitude[1]),
					 c(df[[1]]$longitude[2], df[[1]]$latitude[2]), fun = distHaversine)
	out <- data.frame(distance = distance[1])
	out
}

	

result <- data.frame() 
for (i in 1:length(intra_split)) {
	squared <- get_distance(intra_split[i])
	hold <- data.frame(species = intra_split[i][[1]]$genus_species[1], distance = squared)
	result <- bind_rows(result, hold)
}

distances <- result %>% 
	mutate(distance_km = distance/1000)
	

distances %>% 
	ggplot(aes(x = distance_km)) + geom_histogram() +
	ylab("Frequency") + xlab("Distance among populations (km)")
ggsave("figures/distances-km.png", width = 6, height = 4)


### read in temperature data
	
	intra_temps <- read_csv("~/Documents/intratherm-temp-data-may-2020.csv") 
	
	intra_long <- intra_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = 4, into = c("year", "decimal_year"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 4:745) %>% 
		group_by(population_id, year) %>% 
		summarise(max_temp = max(temperature)) %>% 
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(mean_max_temp = mean(max_temp))

	intra_long2 <- intra_long %>% 
		filter(!is.na(mean_max_temp))
	
	write_csv(intra_long, "data-processed/intratherm-temp-data-yearly-maxes.csv")
	
#### ok which of these are terrestrial? Only use the terrestrial species for now
	
	unique(intratherm$realm_general2)
	
	intra_temps <- intratherm %>% 
		filter(realm_general2 == "Terrestrial") %>% 
		select(genus_species, population_id, latitude, longitude) %>% 
		distinct(.) %>% 
		left_join(., intra_long2, by = c("population_id")) 
		
#### daily mean maximum temperature
	
	intra_species <- intratherm %>% 
		select(genus_species, latitude, longitude, population_id) %>% 
		distinct() 
	
	differences <- intra_long2 %>%
		left_join(., intra_species) %>% View
		# filter(!is.na(mean_max_temp)) %>% 
		filter(genus_species %in% c(distances$species)) %>% 
		select(genus_species, mean_max_temp, population_id, latitude, longitude) %>% 
		left_join(., distances, by = c("genus_species" = "species")) %>% 
		group_by(genus_species) %>% 
		mutate(temp_diff = max(mean_max_temp) - min(mean_max_temp)) %>% 
		filter(temp_diff != 0) 
	
	realms <- intratherm %>% 
		select(genus_species, realm_general2) %>% 
		distinct()
	
	differences %>% 
		# distinct(temp_diff, .keep_all = TRUE) %>% 
		left_join(., realms) %>% 
		filter(realm_general2 == "Terrestrial") %>%
		ggplot(aes(x = distance_km, y = temp_diff, color = realm_general2)) + geom_point() +
		ylab("Max temperature difference") + xlab("Distance between populations (km)")
	ggsave("figures/temp_diffs_distances.png", width = 6, height = 4)
	

	
	
	
# Freshwater temperatures -------------------------------------------------

fw <- read_csv("data-processed/intratherm-freshwater-temp-data.csv")
intra_fw <- read_csv("data-processed/intratherm-may-2020-squeaky-clean.csv") %>% 
	mutate(population_id = paste(population_id, longitude, sep = "_")) %>% 
	filter(realm_general2 == "Freshwater")
	

fw2 <- fw %>% 
	gather(key = population_id, value = monthly_temp, 4:194) %>% 
	separate(date_number, into = c("year", "fraction")) %>% 
	group_by(population_id, year) %>% 
	summarise(max_yearly_temp = max(monthly_temp)) %>% 
	group_by(population_id) %>% 
	summarise(mean_yearly_max_temp = mean(max_yearly_temp))

intra_fw2 <- intra_fw %>% 
	left_join(., fw2)

	
fw3 <- fw2 %>% 
	separate(population_id, into = c("genus_species", "coordinates"), sep = "_", remove = FALSE) %>% 
	filter(genus_species %in% distances$species) %>% 
	left_join(., distances, by = c("genus_species" = "species")) %>% 
	group_by(genus_species) %>% 
	mutate(temp_difference = max(mean_yearly_max_temp) - min(mean_yearly_max_temp)) %>%
	filter(temp_difference != 0)


fw3 %>% 
	ggplot(aes(x = distance_km, y = temp_difference)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)")
ggsave("figures/temp-differences-freshwater.png", width = 6, height = 4)

