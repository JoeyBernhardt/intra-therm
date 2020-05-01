


### Data cleaning etc.

## this is the latest datafile sent by Nikki (April 30 2020)

intratherm <- read_csv("data-raw/intratherm-merged-nikkis-traits-clean.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_")) %>% 
	arrange(genus_species)


names(intratherm)[grepl("stage", names(intratherm))] ## find the list of columns that have 'general'

unique(intratherm$dispersal_distance_category)



intratherm %>% 
	filter(is.na(longitude)) %>% View
	mutate(dispersal_distance_category = ifelse(dispersal_distance_category == "10-Jan", "1-10", dispersal_distance_category))
