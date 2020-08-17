


library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(geosphere)
library(broom)


intratherm <- read_csv("data-processed/intratherm-with-elev.csv") %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	mutate(population_id = paste(genus_species, latitude, elevation_of_collection, longitude, sep = "_"))

multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin =="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>% 
	filter(n > 1)

arrs <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(realm_general2, genus_species, population_id, lat_long) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.))) 

intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	# filter(population_id == "Lithobates sylvaticus_37.56_NA_-84.3") %>% 
	ggplot(aes(x = acclim_temp, y = parameter_value, group = population_id)) + 
	geom_smooth(method = "lm", se = FALSE, alpha = 0.5) + geom_point() +
	ylab("CTmax") + xlab("Acclimation temperature")

arr_slopes <- arrs %>% 
	filter(term != "(Intercept)") %>% ## ok fewer than half of the data have more than 2 acclimation temperatures
	select(genus_species, population_id, estimate) %>% 
	rename(slope = estimate) %>% 
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") %>% ## this is the super high ARR, it's only got two data points
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") ### this is the super low ARR

intercepts <- arrs %>% 
	filter(term == "(Intercept)")  %>% 
	select(genus_species, population_id, estimate) %>% 
	rename(intercept = estimate) %>% 
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") %>%  ### this is the super low ARR
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") ## this is the super high ARR, it's only got two data points


slopes_int <- left_join(intercepts, arr_slopes)

ctmax_20 <- arrs %>% 
	select(genus_species, population_id, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	rename(intercept = `(Intercept)`) %>% 
	mutate(ctmax_20 = acclim_temp*20 + intercept) %>% 
	filter(!is.na(ctmax_20)) 

all_ctmax <- multi_acc %>% 
	left_join(., ctmax_20)


fw <- read_csv("data-processed/intratherm-freshwater-temp-data-daily.csv") 

fw_ctmax <- all_ctmax %>% 
	filter(realm_general2 == "Freshwater")


fw2 <- fw %>% 
	gather(key = population_id, value = monthly_temp, 2:386) %>%
	separate(date, into = c("year", "fraction")) %>% 
	group_by(population_id, year) %>% 
	summarise(max_yearly_temp = max(monthly_temp)) %>% 
	group_by(population_id) %>% 
	summarise(mean_yearly_max_temp = mean(max_yearly_temp))

intratherm_traits <- intratherm %>% 
	select(population_id, age_maturity_days_female, dispersal_distance_category, lifespan_days, average_body_size_female)

intra_fw2 <- fw_ctmax %>% 
	left_join(., fw2) %>% 
	left_join(., intratherm_traits) %>% 
	mutate(age_maturity_days_female = as.numeric(age_maturity_days_female)) %>% 
	mutate(average_body_size_female = as.numeric(average_body_size_female)) %>% 
	mutate(lifespan_days  = as.numeric(lifespan_days)) %>% 
	distinct(., .keep_all = TRUE) %>% 
	filter(!is.na(ctmax_20))




library(nlme)

intra_fw2 %>% 
	ggplot(aes(x = age_maturity_days_female, y = lifespan_days)) + geom_point()

str(intra_fw2)
mod1 <- lme(ctmax_20 ~ age_maturity_days_female + dispersal_distance_category +
				lifespan_days + average_body_size_female, data = intra_fw2, random = ~ 1 | genus_species)

mod1 <- lm(ctmax_20 ~ age_maturity_days_female + dispersal_distance_category +
		lifespan_days + average_body_size_female, data = intra_fw2)

summary(mod1)


### all temperatures
## this is the pairwise temperatures
temps <- read_csv("data-processed/initialize_pairwise_differences_experienced_and_topt.csv")

## raw daily temperature data
terr_temps <- read_csv("data-processed/intratherm-terrestrial-temps-tavg.csv")
fw <- read_csv("data-processed/intratherm-freshwater-temp-data-daily.csv") 
marine_temps <- read_csv("data-processed/intratherm-marine-temp-data.csv")
operative_temps <- read.csv("data-processed/OperativeTemperatures_shade.csv", sep = "\t")
topt <- read.delim("data-processed/OperativeTemperatures_shade.csv", sep = " ") %>%
	rename(intratherm_id = ID)


View(marine_temps)

### now the ctmax data
intratherm <- read_csv("data-processed/intratherm-with-elev.csv") %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	mutate(population_id = paste(genus_species, latitude, elevation_of_collection, longitude, sep = "_"))

multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin =="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>% 
	filter(n > 1)

# View(multi_acc)

### ok we are aiming for something like this: 
# mod1 <- lme(ctmax_20 ~ age_maturity_days_female*temperature + dispersal_distance_category*temperature + 
# ARR*temperature + lifespan_days*temperature + average_body_size_female + sd_temperature, data = intra_fw2, random = ~ 1 | genus_species)




