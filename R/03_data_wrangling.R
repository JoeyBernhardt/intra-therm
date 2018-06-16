

### data wrangling
library(tidyverse)


AB <- read_csv("data-raw/Globtherm2_within_species_AB.csv")
FL <- read_csv("data-raw/Globtherm2_within_species_FL.csv")
FV <- read_csv("data-raw/Globtherm2_within_species_FV.csv")
SO <- read_csv("data-raw/Globtherm2_within_species_SO.csv")


FL2 <- FL %>% 
	select(1:9) %>% 
	select(-sample_size) %>% 
	mutate(person == "FL")

AB2 <- FL %>% 
	select(1:9) %>% 
	select(-sample_size)

SO2 <- SO %>% 
	select(1:9)%>% 
	select(-sample_size)
FV2 <- FV %>% 
	select(1:9)%>% 
	select(-sample_size)

all <- bind_rows(AB2, FL2, SO2, FV2)


str(AB)

write_csv(all, "data-processed/intra-therm-mini.csv")

