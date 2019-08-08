

### figuring out how many locations are still missing

library(tidyverse)
library(readxl)

locations <- read_xlsx("data-raw/missing-locations-intratherm.xlsx") %>% 
	mutate(longitude = ifelse(longitude ==  "NA", NA, longitude)) %>% 
	mutate(latitude = ifelse(latitude ==  "NA", NA, latitude))
	


locations %>% 
	filter(is.na(longitude)) %>%
	select(ref, location_description, everything()) %>% View
