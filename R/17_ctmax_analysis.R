

### initial analysis

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


intratherm <- read_csv("data-processed/intratherm-may-2020-squeaky-clean.csv")


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

library(geosphere)
# distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

### get all the the species that only have two populations

intra2 <- intratherm %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	distinct(ref) %>% 
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

df <- intra_split[1]
str(df)
df[1][name_df]$longitude[1]

name_df <- names(df)
str(name_df)
function(df){
	name_df <- names(df)[[1]]
	distance = distm(c(df[1]$name_df$longitude[1], df[1]$`name_df`$latitude[1]),
					 c(df[1]$`name_df`$longitude[2], df[1]$`name_df`$latitude[2]), fun = distHaversine)
}

	mutate(distance = distm(c(longitude[[1]], latitude[[1]]), c(longitude[[2]], latitude[[2]]), fun = distHaversine)) %>% View
	
	
	
	
### read in temperature data
	
	intra_temps <- read_csv("~/Documents/intratherm-temp-data-may-2020.csv")

	names(intra_temps)
	