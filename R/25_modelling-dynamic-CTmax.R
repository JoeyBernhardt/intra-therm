## predicted CTmax and population dynamics analysis 
library(tidyverse)
library(broom)

## read in R object of dataframes:
pops <- readRDS("data-processed/population-dynamics-with-temp-ts.rds")


## as a test, use temperature 14 days before to 'acclimate' and model CTmax of organism using average ARR for species from intratherm
## our model will be more complicated 
intratherm <- read_csv("data-processed/intratherm-with-elev.csv") %>%
	mutate(population_id = paste(population_id, longitude, sep = "_"))

multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>%   
	filter(n > 1)

## make predictions for each population based on groups of each species:
arr_fits <- intratherm %>% 
	filter(parameter_tmax_or_tmin == "tmax") %>% 
	filter(!is.na(acclmultiim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) %>% 
	group_by(genus_species) %>% 
	filter(length(unique(acclim_temp)) > 1) %>%
	do(fit = MASS::rlm(parameter_value~acclim_temp, data=.))

pops <- bind_rows(pops) %>%
	mutate(genus_species = str_split_fixed(population_id, pattern = "_", n = 3)[,1]) %>%
	mutate(acclim_temp = temperature) %>%
	## mutate(acclim_temp = lag(temperature, n = 14)) %>%
	filter(!is.na(acclim_temp))



predictions <- pops %>%
	select(genus_species, acclim_temp, date, temperature, population_id, abundance) %>%
	group_by(genus_species) %>% 
	nest() %>% 
	left_join(., arr_fits) %>% 
	group_by(genus_species) %>% 
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(predictedCTmax = .fitted, se = .se.fit) 
	

##check out each population:

i = 1
while (i < length(unique(predictions$population_id))+1) {
	pop <- predictions %>%
		filter(population_id == unique(predictions$population_id)[i])
	
	avg <- (mean(pop$temperature, na.rm=TRUE) +  mean(pop$predictedCTmax, na.rm=TRUE)) /2
	avg_abd <- mean(pop$abundance, na.rm=TRUE)
	
	scale <- avg/avg_abd
	
	pop_id_split <- str_split_fixed(pop$population_id, n = 4, pattern = "_")
	label <- paste(pop_id_split[1,1], "\n", pop_id_split[1,2], ", ", pop_id_split[1,3], sep = "")
	
	pop %>% 
		ggplot(aes(x = date, y = temperature)) + 
		geom_line(aes(colour = "Temperature")) +
		geom_line(aes(y = predictedCTmax, x = date, colour = "Predicted CTmax")) +
		geom_line(data = na.omit(pop), aes(y = abundance*scale, x = date, colour = "Abundance")) + 
		labs(x = "Date", y = "Temperature (°C)") +
		scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Abundance")) + 
		theme(axis.title.y.right = element_text(color = "blue")) +
		scale_colour_manual(name = label,
							values = c(`Predicted CTmax`="orange", 
												 Abundance="blue", Temperature="black"))
	
	ggsave(filename = paste("figures/temp-ctmax-abundance-figures/", unique(pop$population_id), ".png", sep = ""), height = 6, width = 12, units = "in", device = "png")
	
	i = i + 1
	
}

 
## see how many times temperature exceeds predicted CTmax when time lag in acclimation is 0 days 
count <- predictions %>%
	mutate(temp_dif = predictedCTmax - temperature) 

length(which(count$temp_dif < 0)) ## CTmax is exceeded a total of 23 times 

## how many different populations is CTmax exceeded in?
length(unique(count$population_id[which(count$temp_dif < 0)])) 

count <- count %>% 
	filter(population_id %in% unique(count$population_id[which(count$temp_dif < 0)]))



## see how much that changes when you incorporate a delay in acclimation
pops_7dayacclim <- pops %>%
	mutate(acclim_temp = lag(temperature, n = 7))

predictions_7dayacclim <- pops %>%
	select(genus_species, acclim_temp, date, temperature, population_id, abundance) %>%
	group_by(genus_species) %>% 
	nest() %>% 
	left_join(., arr_fits) %>% 
	group_by(genus_species) %>% 
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(predictedCTmax = .fitted, se = .se.fit) 

count_7dayacclim <- predictions_7dayacclim %>%
	mutate(temp_dif = predictedCTmax - temperature) 

length(which(count$temp_dif < 0)) ## 23

count_7dayacclim <- count_7dayacclim %>% 
	filter(population_id %in% unique(count_7dayacclim$population_id[which(count_7dayacclim$temp_dif < 0)]))
