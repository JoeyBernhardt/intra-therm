

### data wrangling
library(tidyverse)


AB <- read_csv("data-raw/Globtherm2_within_species_AB.csv")
FL <- read_csv("data-raw/Globtherm2_within_species_FL.csv")
FV <- read_csv("data-raw/Globtherm2_within_species_FV.csv")
SO <- read_csv("data-raw/Globtherm2_within_species_SO.csv")


FL2 <- FL %>% 
	select(1:9) %>% 
	select(-sample_size) %>% 
	mutate(person = "FL")

AB2 <- AB %>% 
	select(1:9) %>% 
	select(-sample_size) %>% 
	mutate(person = "AB")

SO2 <- SO %>% 
	select(1:9)%>% 
	select(-sample_size) %>% 
	mutate(person = "SO")
FV2 <- FV %>% 
	select(1:9)%>% 
	select(-sample_size) %>% 
	mutate(person = "FV")

all <- bind_rows(AB2, FL2, SO2, FV2)


str(AB)

write_csv(all, "data-processed/intra-therm-mini.csv")

library(stringr)
FV_lat <- FV %>% 
	mutate(hemisphere = ifelse(grepl("N", lat_of_collection), "northern", "southern")) %>% 
	select(hemisphere, everything()) %>% 
	mutate(lat_of_collection = str_replace(lat_of_collection, "N", "")) %>% 
	mutate(lat_of_collection = str_replace(lat_of_collection, "S", "")) %>% 
	separate(lat_of_collection, into = c("degrees", "minutes"), sep = "°") %>% 
	select(degrees, minutes, everything()) %>% 
	mutate(minutes = str_replace(minutes, "'", "")) 

write_csv(FV_lat, "data-processed/FV_lat.csv")


FV_lat <- read_csv("data-processed/FV_lat.csv")




FV_lat %>% 
	mutate(minutes = ifelse(is.na(minutes), 0, minutes)) %>% 
	mutate(minutes = as.numeric(minutes)) %>%
	mutate(degrees = as.numeric(degrees)) %>% 
	mutate(dec_degree = degrees + (minutes)/60) %>% 
	select(dec_degree, everything()) %>% View

