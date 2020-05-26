## calculating pairwise population distances 
library(tidyverse)
library(utils)
library(geosphere)


#
###
#######
##############
##########################
####################################
###############################################
# FUNCTION TO INITILIZE THE DISTANCE DATAFRAME 
###############################################
## no input
## returns a dataframe of distance matricies
initialize_distance_data <- function() {
	data <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")
	
	## subset to data that has both latitude and longitude 
	data <- data %>%
		filter(!is.na(latitude)) %>%
		filter(!is.na(longitude))
	
	## split into chunks of each species
	species_list <- split(data, data$genus_species)
	
	all_combinations <- data.frame(matrix(ncol = 4))
	colnames(all_combinations) <- c("lat_lon_1", "lat_lon_2", "distance", "genus_species")
	
	i = 1
	while(i < length(species_list) + 1) {
		species <- as.data.frame(species_list[i])
		colnames(species) <- colnames(data)
		
		## make new column lat_lon to use to find unique lat lon pairs
		unique_pops <- species %>%
			mutate(lat_lon = paste(latitude, longitude, sep = "_")) %>%
			filter(!duplicated(lat_lon)) %>%
			select(genus_species, latitude, longitude, lat_lon)
		
		if(nrow(unique_pops) > 1) {
			## find all unique combinations of lat_lon
			combinations <- combn(unique_pops$lat_lon, m=2, simplify = TRUE,)
			
			## split lat_lon string of the unique combinations so distance between the coordinates can be calculated
			## cooridnates must be in format c(longitude, latitude) so reverse column order
			split1 <- str_split_fixed(combinations[1,], n=3, pattern = "_")
			first_coord <- cbind(as.numeric(split1[,2]), as.numeric(split1[,1]))
			split2 <- str_split_fixed(combinations[2,], n=3, pattern = "_")
			second_coord <- cbind(as.numeric(split2[,2]), as.numeric(split2[,1]))
			
			## calculate distance between each unique lat lon pair 
			distance <- distHaversine(first_coord, second_coord)
			
			all <- as.data.frame(cbind(combinations[1,], combinations[2,], distance))
			all$genus_species <- unique_pops$genus_species[1]
			colnames(all)[1:2]<- c("lat_lon_1", "lat_lon_2")
			
			all_combinations <- rbind(all_combinations, all)
		}
		
		i = i + 1
		
	}
	
	all_combinations <- all_combinations[-1,]
	
	return (all_combinations)
}

#
###
#######
##############
##########################
####################################################
############################################################################
# FUNCTION TO PICK OUT TWO POPULATIONDS FROM THE LIST OF PAIRWISE DISTANCES
############################################################################
## takes two vectors of the format c(genus_species, latitude, longitude) representing the two populations to calculate the distance between and the distance dataframe 
distance_between <- function(population1, population2, all_combinations) {
	
	lat_lon1 = paste(population1[2], population1[3], sep = "_")
	lat_lon2 = paste(population2[2], population2[3], sep = "_")
	
	match = which(population1[1] == all_combinations$genus_species & population2[1] == all_combinations$genus_species & lat_lon1 == all_combinations$lat_lon_1 & lat_lon2 == all_combinations$lat_lon_2)
	
	if (length(match) == 0) {
		match = which(population1[1] == all_combinations$genus_species & population2[1] == all_combinations$genus_species & lat_lon2 == all_combinations$lat_lon_1 & lat_lon1 == all_combinations$lat_lon_2)
	}
	
	if (length(match) == 0) {
		print("Error: no match found for one of the populations.") 
		if (!population1[1] %in% all_combinations$genus_species) {
			print("There seems to be a problem with the second population's genus_species.")
		}
		if (!population2[1] %in% all_combinations$genus_species) {
			print("There seems to be a problem with the first population's genus_species.")
		}
		if (!lat_lon1 %in% all_combinations$lat_lon_1) {
			print("There seems to be a problem with the second population's coordinates.")
		}
		if (!lat_lon2 %in% all_combinations$lat_lon_2) {
			print("There seems to be a problem with the first population's coordinates.")
		}
		return()
	}
	else {
		dist <- as.numeric(as.character(all_combinations$distance[match]))
		
		return(dist)
	}
}


#
###
#######
##############
##########################
####################################################
############################################################################
# FUNCTION TO GET PAIRWISE DISTANCES OF SPECIES ############################
############################################################################
## takes a genus_species and the distance dataframe and returns all pairwise combinations of populations of that species 
populations_of_species <- function(species, all_combinations) {
	
	match = which(species == all_combinations$genus_species)
	
	if (length(match) == 0) {
		print("Error: no species match found.") 
		return()
	}
	else {
		output <- all_combinations[match,]
		
		return(output)
	}
}



