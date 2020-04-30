


### Data cleaning etc.

## this is the latest datafile sent by Nikki (April 30 2020)

intratherm <- read_csv("data-raw/intratherm-merged-nikkis-traits-clean.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))
