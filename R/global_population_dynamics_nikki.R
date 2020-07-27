library(rerddap)
library(rgpdd)
library(tidyverse)
library(cowplot)
library(taxize)
library(ncdf4)
theme_set(theme_cowplot())


#######################################################
## FIGURE OUT WHICH SPECIES  OVERLAP WITH INTRATHERM ##
#######################################################
taxa <- gpdd_taxon
gdata <- gpdd_data
main <- gpdd_main
location <- gpdd_location
names(main)

## must taxize the species names in TaxonName -- know some like Rana are out of date and will not match database 
intratherm <- read.csv("data-processed/intratherm-with-elev.csv", stringsAsFactors = FALSE) 
names <- taxa$TaxonName
species <- str_split_fixed(pattern = " ", names, n = 2) %>%
	.[,2]

names <- taxa$TaxonName[which(species %in% intratherm$species)]
tsn_search <- get_tsn(as.character(names), accepted = FALSE) ## find tsn for each unique taxa
tsn_search <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/tsn_search_popdynam.rds")
tsns <- data.frame(tsn_search) %>%
	mutate(taxa = names)

found <- tsns %>%
	subset(match == "found") 

report <- lapply(found$ids, itis_acceptname)
report_df <- data.frame(matrix(unlist(report), nrow=55, byrow=T),stringsAsFactors=FALSE)
report_df <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/report_df_popdynam.rds")

found <- found %>%
	mutate(genus_species_corrected = report_df$X2)

merged_unique <- left_join(tsns, found) %>%
	mutate(taxa = as.character(taxa)) %>%
	mutate(genus_species = taxa) %>%
	select(genus_species, taxa, genus_species_corrected)

## if names found are not accepted names, then change to accepted name
i = 1
while (i < length(merged_unique$taxa)+1) {
	if (!is.na(merged_unique$genus_species_corrected[i])) {
		merged_unique$taxa[i] <- merged_unique$genus_species_corrected[i]
	}
	i = i+1
}


## check the overlap between intratherm and gpdd AFTER taxize 
overlap <- intersect(unique(merged_unique$taxa), unique(intratherm$genus_species))

overlap_intra <- intratherm %>% 
	filter(genus_species %in% overlap) %>%
	select(genus_species, phylum, class, order, family)

overlap_taxa <- taxa %>% 
	left_join(., merged_unique, by = c("TaxonName" = "genus_species")) %>%
	mutate(genus_species = taxa) %>%
	select(-genus_species_corrected, -taxa) %>%
	filter(TaxonName %in% overlap)

overlap_main <- main %>% 
	filter(TaxonID %in% overlap_taxa$TaxonID)

overlap_gdata <- gdata %>% 
	filter(MainID %in% overlap_main$MainID)


ol <- left_join(overlap_gdata, overlap_main) %>% 
	left_join(., overlap_taxa, by  = "TaxonID") %>% 
	left_join(., overlap_intra, by = c("TaxonName" = "genus_species")) %>% 
	left_join(., location, by = "LocationID") %>%
	distinct()

ol %>% 
	mutate(unique_population = paste(TaxonName, MainID, sep = "_")) %>% 
	select(unique_population, everything()) %>% 
	ggplot(aes(x = SeriesStep, y = Population, group = unique_population, color = TaxonName)) + 
	geom_line() +
	facet_wrap( ~ unique_population, scales = "free") 


ggsave("figures/gpdd-taxon_nikki.pdf", width = 20, height = 12)
ggsave("figures/gpdd_nikki.png", width = 14, height = 12)

write_csv(ol, "data-processed/intratherm-gpdd_nikki.csv")



## Living Planet Index
library(rlpi)

lpi <- read_csv("data-raw/LPI_LPR2016data_public.csv") %>% 
	mutate(genus_species = str_replace(Binomial, "_", " "))

## taxize names
names <- unique(lpi$genus_species[which(lpi$Species %in% intratherm$species)])
tsn_search <- get_tsn(as.character(names), accepted = FALSE) ## find tsn for each unique taxa
tsn_search <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/tsn_search_popdynam2.rds")
tsns <- data.frame(tsn_search) %>%
	mutate(taxa = names)

found <- tsns %>%
	subset(match == "found") 

report <- lapply(found$ids, itis_acceptname)
report_df <- data.frame(matrix(unlist(report), nrow=270, byrow=T),stringsAsFactors=FALSE)
report_df <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/report_df_popdynam2.rds")

found <- found %>%
	mutate(genus_species_corrected = report_df$X2)

merged_unique <- left_join(tsns, found) %>%
	mutate(taxa = as.character(taxa)) %>%
	mutate(genus_species = taxa) %>%
	select(genus_species, taxa, genus_species_corrected)

## if names found are not accepted names, then change to accepted name
i = 1
while (i < length(merged_unique$taxa)+1) {
	if (!is.na(merged_unique$genus_species_corrected[i])) {
		merged_unique$taxa[i] <- merged_unique$genus_species_corrected[i]
	}
	i = i+1
}


over <- intersect(unique(intratherm$genus_species), unique(merged_unique$taxa))

lpi_over <- lpi %>% 
	left_join(., merged_unique) %>%
	mutate(genus_species = taxa) %>%
	select(-taxa, -genus_species_corrected) %>%
	filter(genus_species %in% over) %>% 
	gather(key = year, value = abundance, "1950":"2015") %>% 
	mutate(abundance = ifelse(abundance == "NULL", NA, abundance)) %>% 
	mutate(abundance = as.numeric(abundance)) %>% 
	mutate(year = as.numeric(year))

write_csv(lpi_over, "data-processed/lpi-intratherm-overlap_nikki.csv")




##########################################################################################
## getting temperature data for popultions with known population dynamics to predict CTmax
##########################################################################################
intratherm <- read.csv("data-processed/intratherm-with-elev.csv", stringsAsFactors = FALSE) %>%
	select(genus_species, realm_general2)
lpi_ol <- read.csv("data-processed/lpi-intratherm-overlap_nikki.csv", stringsAsFactors = FALSE)
gpdd_ol <- read.csv( "data-processed/intratherm-gpdd_nikki.csv", stringsAsFactors = FALSE)
location <- gpdd_location

## figure out realm of populations 
ol_locs_gdpp <- gpdd_ol %>%
	left_join(., intratherm) %>% 
	select(realm_general2, LocationID) %>%
	unique() %>%
	left_join(location[location$LocationID %in% unique(gpdd_ol$LocationID),], .) %>%
	mutate(realm_of_population = ifelse(!str_detect(as.character(Ocean),
													pattern = "Not applicable"), "Marine", 
										ifelse(realm_general2 != "Terrestrial", "Freshwater","Terrestrial")
	)
	) %>%
	select(LocationID, realm_of_population, LatDD, LongDD, -realm_general2) %>%
	mutate(temp_id = paste(LatDD, LongDD, sep = "_")) %>%
	rename(latitude = LatDD, longitude = LongDD) %>%
	mutate(population_source = "GPDD") %>%
	distinct()

ol_locs_lpi <- lpi_ol %>%
	mutate(temp_id = paste(Latitude, Longitude, sep = "_"), LocationID = NA) %>%
	rename(realm_of_population = System) %>%
	mutate(realm_of_population = ifelse(str_detect(Class, "Amphibia"), "Terrestrial", as.character(realm_of_population))) %>%
	select(LocationID, temp_id, realm_of_population, Latitude, Longitude) %>%
	rename(latitude = Latitude, longitude = Longitude) %>%
	mutate(population_source = "LPI") %>%
	unique()

ol_locs_all <- rbind(ol_locs_lpi, ol_locs_gdpp)


#########################################################
##      GETTING ELEVATION FOR POPDYNAM POPULATIONS     ##
#########################################################
latitude_of_raster <- c() 
longitude_of_raster <- c()
## "raster_of_latitude" and "raster_of_longitude" represent the centre coordinates of 1 degree lat x 1 degree long grid cells 

unique_locs <- ol_locs_all %>%
	filter(realm_of_population == "Terrestrial") %>%
	filter(!duplicated(temp_id))

filename <- paste("./Berkeley_Tavg/Complete_TAVG_Daily_LatLong1_1930.nc", sep = "")
ncfile <- nc_open(filename)

lat <- ncvar_get(ncfile, "latitude")
long <- ncvar_get(ncfile, "longitude")

nc_close(ncfile)


## get grid square coordinates for each population
num_unique <- 1
while (num_unique < nrow(unique_locs)+1) {
	loc_long_index <- which.min(abs(long - unique_locs$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - unique_locs$latitude[num_unique]))
	
	latitude_of_raster <- append(latitude_of_raster, lat[loc_lat_index])
	longitude_of_raster <- append(longitude_of_raster, long[loc_long_index])
	
	num_unique <- num_unique + 1
}

unique_locs$latitude_of_raster <- latitude_of_raster
unique_locs$longitude_of_raster <- longitude_of_raster

#########################################################
##      GETTING RASTER ELEVATION      ##
########################################
## first, get the average elevation across each raster we took Berekely Earth temperature data from 
library(elevatr)
library(raster)
library(rgbif)
library(sp)
library(conflicted)
library(rlist)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("extract", "raster")

latitude <- unique_locs$latitude_of_raster
longitude <- unique_locs$longitude_of_raster


## 1. create a SpatialPolygonsDataFrame of 1 degree lat x 1 degree long rectangles representing each grid cell we need elevation for

## set projection to lat long WGS84
myProj <- "+proj=longlat +datum=WGS84 +ellps=WGS84"

raster_means <- c()

## for each grid cell we took temp data from:
i <- 1
while (i < nrow(unique_locs)+1) {
	
	## 1. create a rectangle representing grid cell:
	## draw square with coords corresponding to corners 
	ybottom = unique_locs$latitude_of_raster[i] - 0.5
	xleft = unique_locs$longitude_of_raster[i] - 0.5
	ytop = unique_locs$latitude_of_raster[i] + 0.5
	xright = unique_locs$longitude_of_raster[i] + 0.5
	
	print("Drawing grid cell...")
	rectangle <- Polygon(cbind(c(xleft,xleft,xright,xright,xleft),c(ybottom, ytop, ytop, ybottom, ybottom)))
	
	poly <- Polygons(list(rectangle), ID = "A")
	
	## create spatial object of polygon 
	spPolygon = SpatialPolygons(list(poly))
	## assign projection
	proj4string(spPolygon) <- myProj
	
	## create dataframe: 
	df = matrix(data = c(0))
	rownames(df) = "A"
	spp = SpatialPolygonsDataFrame(spPolygon, data = as.data.frame(df))
	
	
	## 2. get GDEM raster object with data for every cell:
	lat_og <- unique_locs$latitude_of_raster[i]
	long_og <- unique_locs$longitude_of_raster[i]
	
	y <- c(lat_og) ## latitudes of points to get GDEM data for 
	x <- c(long_og) ## longitudes of points to get GDEM data for 
	
	## add 9 points in and around edge of grid cell to query to ensure whole grid cell GDEM data is downloaded
	n = 1
	while (n < 3) {
		if(n < 2) {
			latitude <- lat_og + 0.5
			longitude <- long_og + 0.5
			
			y <- append(y, latitude)
			x <- append(x, longitude)
		}
		if (n >= 2) {
			latitude <- lat_og - 0.5
			longitude <- long_og - 0.5
			
			y <- append(y, latitude)
			x <- append(x, longitude)
		}
		n = n + 1
	}
	loc_df <- data.frame(crossing(x, y)) ## data frame with the 9 query coordinates 
	
	
	## get GDEM data for loc_df
	print(paste("Getting GDEM data for grid cell ", i, "...", sep = ""))
	cell_raster <- get_elev_raster(locations = loc_df, prj = myProj, z=10)
	##plot(cell_raster)
	
	## check that dimensions overlap and projections are the same:
	##show(cell_raster)
	##show(spp)
	##plot(cell_raster)
	##plot(spp, add=TRUE)
	
	
	## 3. calculate average elevation in each grid cell and record: 
	print(paste("Done! Extracting pixels under grid cell ", i, "...", sep = ""))
	under_cell <- data.frame(extract(cell_raster, spp)) ## extract elevation of all GDEM pixels under the grid cell rectangle
	colnames(under_cell) <- c("pixel_elevation")
	print("Done! Calculating mean... ")
	under_cell <- subset(under_cell, subset = under_cell$pixel_elevation >= 0) ## remove all negative elevations corresponding to ocean 
	raster_mean <- lapply(under_cell, FUN=mean) ## calculate the mean of the pixels to get mean elevation of the grid cell
	raster_means <- append(raster_means, raster_mean)
	
	
	## 4. move on to the next grid cell 
	print(paste("Finished grid cell number ", i, ", moving on to next cell...", sep = ""))
	i <- i + 1
}

unique_locs$raster_mean <- unlist(raster_means, use.names=FALSE)
unique_pairs <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_elevation_popdynam.rds")

########################################
##       GETTING POINT ELEVATION      ##
########################################
elev_input <- unique_pairs %>%
	rename(decimalLatitude = latitude, decimalLongitude = longitude) 

point_elev <- elevation(input = elev_input, elevation_model = "astergdem", username = "nicole_a_moore")

point_elev <- point_elev %>%
	rename(elevation_of_collection = elevation_geonames) 

unique_pairs <- left_join(unique_pairs, point_elev)
ol_locs_all <- left_join(ol_locs_all, unique_pairs) 

lpi <- lpi_ol %>%
	select(genus_species, Latitude, Longitude, System, Class) %>%
	unique() %>%
	rename(latitude = Latitude, longitude = Longitude, realm_of_population = System) %>%
	mutate(realm_of_population = ifelse(str_detect(Class, "Amphibia"), "Terrestrial", 
										as.character(realm_of_population))) %>%
	select(-Class) %>%
	left_join(., ol_locs_all) %>%
	filter(population_source == "LPI") %>%
	arrange(temp_id, latitude, longitude)

gpdd <- gpdd_ol %>%
	select(TaxonName, LocationID) %>%
	unique() %>%
	left_join(.,location[location$LocationID %in% unique(gpdd_ol$LocationID),]) %>%
	select(TaxonName, LatDD, LongDD, LocationID) %>%
	rename(latitude = LatDD, longitude = LongDD, genus_species = TaxonName) %>%
	left_join(., ol_locs_all)

population_overlap <- rbind(lpi, gpdd)



#########################################
##      GETTING TERRESTRIAL TEMPS      ##
#########################################
temperature_data <- data.frame(matrix(nrow = 32294))
unique_pairs <- ol_locs_all %>%
	filter(realm_of_population == "Terrestrial") %>%
	filter(!duplicated(temp_id))

num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
	
	loc_cumulative <- data.frame(matrix(ncol=2))
	colnames(loc_cumulative) <- c("date", "temp_value")
	rep = 1930
	
	while (rep < 2013) {
		print(paste("On population number ", num_unique, ", getting temp data from ", rep, sep = ""))
		## read in gridded data in nc file for the file_index from berkeley earth and store data in R workspace 
		filename <- paste("./Berkeley_Tavg/Complete_TAVG_Daily_LatLong1_", rep, ".nc", sep = "")
		ncfile <- nc_open(filename)
		
		## create variables for things needed to use data
		date <- ncvar_get(ncfile, "date_number")
		arr.anom <-ncvar_get(ncfile, "temperature")
		arr.clim <- ncvar_get(ncfile, "climatology")
		
		nc_close(ncfile)
		
		## get clim and anom data for collection location of species 
		## NaN here if location does not have data 
		loc_long_index <- which.min(abs(long - unique_pairs$longitude[num_unique]))
		loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
		loc.anom <- arr.anom[loc_long_index,loc_lat_index,]
		loc.clim.365d <- arr.clim[loc_long_index,loc_lat_index,]
		
		## account for leap year - duplicate day index added on feb 28 in clim array (this seems to be how they dealt with it when calculating anom)
		index_59 <- loc.clim.365d[59]
		loc.clim.366d <- append(loc.clim.365d, index_59, after = 59)
		
		## repeat day list loc.clim.365d on normal years + loc.clim.366d on leap years 
		last_year = (rep + 10)
		loc.clim <- c()
		
		if(last_year == 2020) {
			last_year = 2019
		}
		
		while (rep < last_year) {
			if (rep == 2018) {
				loc.clim <- append(loc.clim, loc.clim.365d[1:151], after = length(loc.clim))
			}
			else if (rep %% 4 == 0){
				if (rep == 1900) { 
					loc.clim <- append(loc.clim, loc.clim.365d, after = length(loc.clim))
				}
				else {
					loc.clim <- append(loc.clim, loc.clim.366d, after = length(loc.clim))
				}
			}
			else {
				loc.clim <- append(loc.clim, loc.clim.365d, after = length(loc.clim))
			}
			rep = rep + 1
		}
		
		## create dataframe of actual temp values at location by adding anomaly and climatology values over the 10 years 
		temp_list <- c()
		max <- rep
		rep <- rep - 10
		d <- 1
		
		if (rep == 2009) {
			rep <- 2010
		}
		
		while (rep < max) {
			if (rep == 2018) {
				temps <- loc.anom[d:(d+150)] + loc.clim[d:(d+150)]
				temp_list <- append(temp_list, temps, after = length(temp_list))
				d = d + 150
			}
			else if (rep %% 4 == 0) {
				if (rep == 1900) {
					temps <- loc.anom[d:(d+364)] + loc.clim[d:(d+364)]
					temp_list <- append(temp_list, temps, after = length(temp_list))
					d = d + 365
				}
				else { 
					temps <- loc.anom[d:(d+365)] + loc.clim[d:(d+365)]
					temp_list <- append(temp_list, temps, after = length(temp_list))
					d = d + 366
				}
			}
			else {
				temps <- loc.anom[d:(d+364)] + loc.clim[d:(d+364)]
				temp_list <- append(temp_list, temps, after = length(temp_list))
				d = d + 365
			}
			rep = rep + 1
		}
		
		## make dataframe of date and corresponding temp values
		loc <- data.frame(date[], temp_list[])
		colnames(loc) <- c("date", "temp_value")
		
		## add loc to loc_cumulative so one data frame contains data from all 10 year datasets
		loc_cumulative <- rbind(loc_cumulative, loc)
	}
	
	pop_id <- unique_pairs$temp_id[num_unique]
	
	## add column for population to temperature data 
	if (num_unique == 1){
		temperature_data <- cbind(temperature_data, loc_cumulative)
		temperature_data <- temperature_data[,-1]
		colnames(temperature_data)[num_unique+1] <- pop_id
	}
	else {
		temperature_data <- cbind(temperature_data, loc_cumulative[,2])
		colnames(temperature_data)[num_unique+1] <- pop_id
	}
	
	num_unique = num_unique + 1;
	
}

terrestrial_temps <- temperature_data[-1,]
terrestrial_temps <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_terrestrial_tavg_popdynam.rds")

## check NA:
isNA <- terrestrial_temps %>%
	select(as.vector(which(colSums(is.na(terrestrial_temps)) == nrow(terrestrial_temps))))

## 19.5_-154.93333 - all are on Hawaii

write.csv(terrestrial_temps, "data-processed/intratherm-terrestrial-temps-tavg_popdynam.csv", row.names = FALSE)




####################################
##      GETTING MARINE TEMPS      ##
####################################
unique_pairs <- ol_locs_all %>%
	filter(realm_of_population == "Marine") %>%
	filter(!duplicated(temp_id))

info <- info("ncdcOisst21Agg_LonPM180")


## make latitude and longitude vectors based on NOAA format
## longitude: Uniform grid with centers from -179.875 to 179.875 by 0.25 degrees.
lon <- rep(-179.875, times = 1439)
n = 2
while (n < 1441) {
	lon[n] <- lon[n -1] + 0.25
	n = n+1
}
## latitude: Uniform grid with centers from -89.875 to 89.875 by 0.25 degrees.
lat <- rep(-89.875, times = 719)
n = 2
while (n < 721) {
	lat[n] <- lat[n -1] + 0.25
	n = n+1
}


grid_lat <- c()
grid_lon <- c()

## find closest lat lon grid cell to each population collection location 
num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
	loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
	
	grid_lon <- append(grid_lon, lon[loc_lon_index])
	grid_lat <- append(grid_lat, lat[loc_lat_index])
	
	num_unique = num_unique + 1
}

unique_pairs <- unique_pairs %>%
	mutate(grid_lon = grid_lon) %>%
	mutate(grid_lat = grid_lat) 


## create dataframe for temp data
## nValues for time attribute = 14096
temperature_data <- data.frame(matrix(nrow = 14096))
colnames(temperature_data) = c("date")

## loop through each population getting temp data for its grid cell and adding to temp data
num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
	print(paste("On population number", num_unique))
	time_series <- griddap(info,
						   time = c("1981-09-01", "2020-04-04"), 
						   latitude = c(unique_pairs$grid_lat[num_unique],unique_pairs$grid_lat[num_unique]),
						   longitude = c(unique_pairs$grid_lon[num_unique], unique_pairs$grid_lon[num_unique]),
						   url = "https://upwell.pfeg.noaa.gov/erddap/")
	temps <- time_series$data$sst
	print(paste("Successfully got time series of length", length(temps)))
	
	if (num_unique == 1) {
		times <- time_series$data$time
		temperature_data$date <- times
	}
	
	temperature_data$temp <- temps
	pop_id <- unique_pairs$temp_id[num_unique]
	colnames(temperature_data)[num_unique+1]<- pop_id
	
	print("Stored data in temperature_data and moving on to next population!")
	
	num_unique <- num_unique + 1
}

## save
marine_temps <- temperature_data
marine_temps <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_marine_temps_popdynam.rds")

## search freshwater for missing marine temps 
marine_missing <- unique_pairs[which(unique_pairs$temp_id %in% colnames(marine_temps)[which(is.na(marine_temps[1,]))]),]  %>%
	select(-grid_lat,-grid_lon)

marine_temps <- marine_temps[,-which(colnames(marine_temps) %in% marine_missing$temp_id)]

write.csv(marine_temps, "data-processed/intratherm-marine-temp-data_popdynam.csv", row.names = FALSE)


	


########################################
##      GETTING FRESHWATER TEMPS      ##
########################################
unique_pairs <- ol_locs_all %>%
	filter(realm_of_population == "Freshwater") %>%
	rbind(., marine_missing) %>%
	filter(!duplicated(temp_id))

## 0_0 - from Klicava Reservoir, coordinates should be 50.08, 13.9
unique_pairs$latitude[which(unique_pairs$temp_id == "0_0")]  <- 50.08
unique_pairs$longitude[which(unique_pairs$temp_id == "0_0")]  <- 13.9
unique_pairs$temp_id[which(unique_pairs$temp_id == "0_0")]  <- "50.08_13.9"

## open nc file and get lat lon and time vectors
filename <- paste("watertemperature_wfd_historical_1958-2001.nc", sep = "")
ncfile <- nc_open(filename)

lon <- ncvar_get(ncfile, "longitude") ## units: degrees - intervals of 0.5 (30')
lat <- ncvar_get(ncfile, "latitude") ## units: degrees - intervals of 0.5 (30')
time <- ncvar_get(ncfile, "time") ## units: hours since 1901-01-01 (first time is 1958-01-01) 

## close the file
nc_close(ncfile)

## temps:
temperature_data <- data.frame(matrix(nrow = length(time)))
colnames(temperature_data) = c("date")
temperature_data$date <- time

## for each population:
num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
	
	## find closest lat lon coordinates to population collection location
	loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
	
	## get waterTemp time series for closest lat lon coordinates 
	ncfile <- nc_open(filename)
	waterTemp <- ncvar_get(ncfile, "waterTemperature", start = c(loc_lon_index, loc_lat_index, 1), count = c(1, 1, -1))
	nc_close(ncfile)
	
	## add to column in temperature_data and rename after column's population_id with longitude added onto the end
	temperature_data$temp <- waterTemp
	colnames(temperature_data)[num_unique+1] <- unique_pairs$temp_id[num_unique]
	
	num_unique <- num_unique + 1
}

freshwater_temps <- temperature_data
freshwater_temps <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_temps_freshdaily_popdynam.rds")

## change realm_of_population for missing marine temps to freshwater:
ol_locs_all <- ol_locs_all %>%
	mutate(realm_of_population = ifelse(temp_id %in% colnames(freshwater_temps), "Freshwater", realm_of_population))

population_overlap <- population_overlap %>%
	mutate(realm_of_population = ifelse(temp_id %in% colnames(freshwater_temps), "Freshwater", realm_of_population))
	

## convert from degrees K to degrees C
converted <- freshwater_temps
converted[, 2:230] <- converted[, 2:230] - 273.15

## convert date 
## starts at 1958-01-01
year <- c(round(0.5/365, digits = 3))
leap_year <- c(round(0.5/366, digits = 3))

i = 1
while (i < 366) {
	if (i < 365) {
		year = append(year, round((i+0.5)/365, digits = 3))
	}
	leap_year = append(leap_year, round((i+0.5)/366, digits = 3))
	i = i+1
}

rep = 1958
last_year = 2002
date <- c()

while (rep < last_year) {
	if (rep %% 4 == 0){
		if (rep == 1900) { 
			date <- append(date, rep+year, after = length(date))
		}
		else {
			date <- append(date, rep+leap_year, after = length(date))
		}
	}
	else {
		date <- append(date, rep+year, after = length(date))
	}
	rep = rep + 1
}

## replace column for date
converted$date <- as.vector(date)
freshwater_temps <- converted

##  check na:
na_locs <- colnames(freshwater_temps)[which(is.na(freshwater_temps[1,]))]
na_locs	<- na_locs[!na_locs %in% marine_missing$temp_id]

## 52.782831_5.327911 - on coast of netherlands
## 65.5_-66 - Arctic Ocean
## 63.75_-166.88333 - Bering Sea
## -33.545_27.05139 - off coast of South Africa
##  52.5_5.25 - on coast of netherlands
## -50.6_127.4 - Indian Ocean 

## remove all na from freshwater temps, label realm_of_population as marine
freshwater_temps <- freshwater_temps[,-which(is.na(freshwater_temps[1,]))]

write.csv(freshwater_temps, "data-processed/intratherm-freshwater-temp-data-daily_popdynam.csv", row.names = FALSE)

ol_locs_all$realm_of_population[ol_locs_all$temp_id %in% na_locs] <- "Marine"
population_overlap$realm_of_population[population_overlap$temp_id %in% na_locs] <- "Marine"

## get marine temp data for these species:
unique_pairs <- ol_locs_all %>%
	filter(realm_of_population == "Marine" & temp_id %in% na_locs) 

grid_lat <- c()
grid_lon <- c()

## find closest lat lon grid cell to each population collection location 
num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
	loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
	
	grid_lon <- append(grid_lon, lon[loc_lon_index])
	grid_lat <- append(grid_lat, lat[loc_lat_index])
	
	num_unique = num_unique + 1
}

unique_pairs <- unique_pairs %>%
	mutate(grid_lon = grid_lon) %>%
	mutate(grid_lat = grid_lat) %>%
	filter(!duplicated(temp_id)) %>%
	filter(!temp_id %in% colnames(marine_temps))

temperature_data <- data.frame(matrix(nrow = 14096))
colnames(temperature_data) = c("date")

## loop through each population getting temp data for its grid cell and adding to temp data
num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
	print(paste("On population number", num_unique))
	time_series <- griddap(info,
						   time = c("1981-09-01", "2020-04-04"), 
						   latitude = c(unique_pairs$grid_lat[num_unique],unique_pairs$grid_lat[num_unique]),
						   longitude = c(unique_pairs$grid_lon[num_unique], unique_pairs$grid_lon[num_unique]),
						   url = "https://upwell.pfeg.noaa.gov/erddap/")
	temps <- time_series$data$sst
	print(paste("Successfully got time series of length", length(temps)))
	
	if (num_unique == 1) {
		times <- time_series$data$time
		temperature_data$date <- times
	}
	
	temperature_data$temp <- temps
	pop_id <- unique_pairs$temp_id[num_unique]
	colnames(temperature_data)[num_unique+1]<- pop_id
	
	print("Stored data in temperature_data and moving on to next population!")
	
	num_unique <- num_unique + 1
}

new_marine <- temperature_data
new_marine <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/new_marine_popdynam.rds") %>%
	select(-date)

## combine with other marine data
marine_temps <- read_csv("data-processed/intratherm-marine-temp-data_popdynam.csv") %>%
	cbind(., new_marine) 

write.csv(marine_temps, "data-processed/intratherm-marine-temp-data_popdynam.csv", row.names = FALSE)


## write out file of all unique populations that overlap 
population_overlap <- population_overlap %>%
	distinct()

write.csv(population_overlap, "data-processed/population-overlap.csv", row.names = FALSE)



#############################################################################################
## make array of overlapping population trend and temperature time series for each population 
## later add predicted CTmax time series to array
#############################################################################################
library(rlist)

population_overlap <- read.csv("data-processed/population-overlap.csv", stringsAsFactors = FALSE) %>%
	filter(population_source == "LPI") %>%
	select(genus_species, realm_of_population, temp_id)
lpi_ol <- read.csv("data-processed/lpi-intratherm-overlap_nikki.csv") %>%
	arrange(Binomial, ID, Location, year) %>% ## each goes from 1950-2015 and has NA for missing years 
	mutate(population_id = paste(genus_species, Latitude, Longitude, ID, sep = "_")) %>%
	mutate(temp_id = paste(Latitude, Longitude, sep = "_")) %>%
	left_join(., population_overlap)

## NOTE: units vary greatly
unique(lpi_ol$Units)

## start with lpi:
pops_by_realm <- lpi_ol %>%
	select(population_id, temp_id, year, abundance, realm_of_population, Units) %>%
	distinct() %>%
	split(., lpi_ol$realm_of_population) 

pops_new <- list()
i = 1 
while (i < length(pops_by_realm)+1) {
	pops <- pops_by_realm[[i]] %>%
		split(., .$population_id) 
	
	if(i == 1) {
		temps <- read_csv("data-processed/intratherm-freshwater-temp-data-daily_popdynam.csv") %>%
			separate(date, sep = 4, into = c("year"), remove = FALSE)  %>%
			mutate(year = as.integer(year))
		first <- first(which(temps$year == 1958))
		last <- first(which(temps$year == 2001)) - 1
	}
	else if (i == 2) {
		temps <- read_csv("data-processed/intratherm-marine-temp-data_popdynam.csv") %>%
			separate(date, sep = "-", into = c("year"), remove = FALSE) %>%
			mutate(year = as.integer(year))
		first <- first(which(temps$year == 1981))
		last <- first(which(temps$year == 2016)) - 1
	}
	else if (i == 3) {
		temps <- read_csv("data-processed/intratherm-terrestrial-temps-tavg_popdynam.csv") %>%
			separate(date, sep = 4, into = c("year"), remove = FALSE) %>%
			mutate(year = as.integer(year))
		first <- first(which(temps$year == 1950))
		last <- first(which(temps$year == 2016)) - 1
	}
	
	z = 1 + length(pops_new)
	x = 1
	while (x < length(pops) + 1) {
		pop <- as.data.frame(pops[x]) 
		colnames(pop) <- c("population_id", "temp_id", "year", "abundance", "realm_of_population", "units")
		pop <- left_join(pop, temps[,1:2], by = c("year")) %>%
			group_by(year) %>%
			mutate(abundance = replace(abundance, duplicated(abundance), NA)) %>%
			ungroup()
		
		if(i == 1) {
			pop <-	filter(pop, !is.na(date)) %>%
				filter(date >= 1958, date <= 2001)
		}
		else if(i == 2) {
			pop <-	filter(pop, !is.na(date)) %>%
				filter(date >= 1981)
		}
		
		pop$temperature <- unlist(temps[first:last,which(colnames(temps) ==
															  	pop$temp_id[1])])
		if(is.na(first(which(!is.na(pop$abundance))))) {
			x = x+1
		}
			
		else if(is.null(pop$temperature)) {
			x = x+1
		}
		else {
			pop <- pop %>%
				.[first(which(!is.na(.$abundance))):
				  	last(which(.$year == .$year[last(which(!is.na(.$abundance)))])),] 
			
			pop <- pop %>%
				select(population_id, date, abundance, units, temperature) 
			
			pops_new <- list.append(pops_new, pop)
			names(pops_new)[z] <- pop$population_id[1]
			
			z = z+1
			x = x+1	
		}
	}
	
	i = i+1
}

## now add populations from gpdd
population_overlap <- read.csv("data-processed/population-overlap.csv", stringsAsFactors = FALSE) %>%
	filter(population_source == "GPDD") %>%
	select(genus_species, realm_of_population, temp_id, LocationID)
gpdd_ol <- read.csv("data-processed/intratherm-gpdd_nikki.csv", stringsAsFactors = FALSE) %>%
	mutate(temp_id = paste(LatDD, LongDD, sep = "_")) %>%
	arrange(TaxonName, MainID, LocationID, SampleYear) %>%
	mutate(population_id = paste(TaxonName, LatDD, LongDD, MainID, sep = "_")) %>%
	left_join(., population_overlap, by = c("TaxonName" = "genus_species", "temp_id", "LocationID"))

unique(gpdd_ol$SamplingUnits)

pops_by_realm <- gpdd_ol %>%
	select(population_id, temp_id, SampleYear, Population, realm_of_population, DecimalYearBegin, SamplingUnits) %>%
	distinct() %>%
	split(., gpdd_ol$realm_of_population) 

i = 1 
while(i < length(pops_by_realm)+1) {
	pops <- pops_by_realm[[i]] %>%
		split(., .$population_id) 
	
	if(i == 1) {
		temps <- read_csv("data-processed/intratherm-freshwater-temp-data-daily_popdynam.csv") %>%
			separate(date, sep = 4, into = c("year"), remove = FALSE)  %>%
			mutate(year = as.integer(year))
		first = 1958
		last = 2001
	}
	else if (i == 2) {
		temps <- read_csv("data-processed/intratherm-marine-temp-data_popdynam.csv") %>%
			separate(date, sep = "-", into = c("year"), remove = FALSE) %>%
			mutate(year = as.integer(year))
		first = 1981
		last = 2015
	}
	else if (i == 3) {
		temps <- read_csv("data-processed/intratherm-terrestrial-temps-tavg_popdynam.csv") %>%
			separate(date, sep = 4, into = c("year"), remove = FALSE) %>%
			mutate(year = as.integer(year))
		first = 1950
		last = 2015
	}
	
	z = 1 + length(pops_new)
	x = 1
	while (x < length(pops) + 1) {
		pop <- as.data.frame(pops[x]) 
		colnames(pop) <- c("population_id", "temp_id", "year", "abundance", 
						   "realm_of_population", "decimal_year_start", "units")
		
		start <- min(pop$year)
		end <- max(pop$year)
		
		if (end < last) {
			## move on to next one
			x = x+1
		}
		
		else {
			## cut time series to match each other
			if (start < first | start == first) {
				start = first
				first_year <- first(which(temps$year == start))
				pop <- pop %>%
					filter(year >= start)
			}
			else if (start > first) {
				first_year = first(which(temps$year == start))
			}
			if (end > last | end == last) {
				end = last
				last_year = nrow(temps)
				pop <- pop %>%
					filter(year <= end)
			}
			else if (end < last) {
				last_year = first(which(temps$year == end+1)) - 1
			}
			
			## check if more than one sample per year:
			if (length(unique(pop$year)) != nrow(pop)) {
				vec <- c()
				for (l in pop$decimal_year_start) {
					vec = append(vec, temps$date[which.min(abs(l - temps$date))])
				}
				pop$date <- vec
				
				pop <- pop %>%
					right_join(temps[first_year:last_year,1:2], pop, by = c("date", "year")) %>%
					arrange(date) %>%
					fill(population_id, temp_id, realm_of_population, units, .direction = "up") %>%
					select(-decimal_year_start)
				
				pop$temperature <- unlist(temps[first_year:last_year,which(colnames(temps) ==
																		   	pop$temp_id[1])])
			}
			
			else {
				pop <- left_join(pop, temps[first_year:last_year,1:2], by = c("year")) %>%
					group_by(year) %>%
					mutate(abundance = replace(abundance, duplicated(abundance), NA)) %>%
					ungroup()
				
				pop$temperature <- unlist(temps[first_year:last_year,which(colnames(temps) ==
																		   	pop$temp_id[1])])
			}
			
			# if(is.na(first(which(!is.na(pop$abundance))))) {
			# 	x = x+1
			# }
			# 
			# else if(is.null(pop$temperature)) {
			# 	x = x+1
			# }
			# else {
			pop <- pop %>%
				.[first(which(!is.na(.$abundance))):
				  	last(which(.$year == .$year[last(which(!is.na(.$abundance)))])),] %>%
				select(population_id, date, abundance, units, temperature) 
			
			pops_new <- list.append(pops_new, pop)
			names(pops_new)[z] <- pop$population_id[1]
			
			z = z+1
			x = x+1	
		}
	
		# }
	}
	
	i = i+1
}

## try plotting one:
data <- pops_new$`Heterandria formosa_27.68333_-80.73333_4993`
ggplot(data = data, aes(x = date, y = temperature)) + geom_line() +
	geom_line(data = na.omit(data), colour = "red")

## save it as an rds
saveRDS(pops_new, "data-processed/population-dynamics-with-temp-ts.rds")


####################################################################################
## map populations that overlap  
####################################################################################
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

lpi_ol <- read.csv("data-processed/lpi-intratherm-overlap_nikki.csv", stringsAsFactors = FALSE)
gdpp_ol <- read.csv( "data-processed/intratherm-gpdd_nikki.csv", stringsAsFactors = FALSE)
intratherm <- read.csv( "data-processed/intratherm-with-elev.csv", stringsAsFactors = FALSE)

ol_locs_intra_gdpp <- gdpp_ol %>% 
	select(TaxonName, latitude, longitude, location_description) %>% 
	rename(genus_species = TaxonName) %>%
	unique() %>%
	mutate(population_source = "Intratherm")

ol_locs_gdpp <- gdpp_ol %>% 
	select(-LatDD, -LongDD, -ExactName, -Country) %>%
	left_join(., location, by = "LocationID") %>%
	mutate(location_description = paste(ExactName, Country, sep = " ")) %>%
	select(TaxonName, LatDD, LongDD, location_description) %>% 
	rename(genus_species = TaxonName, longitude = LongDD, latitude = LatDD) %>%
	unique() %>%
	mutate(population_source = "GDPP")

ol_locs_intra_lpi <- intratherm %>% 
	filter(genus_species %in% lpi_ol$genus_species) %>%
	select(genus_species, latitude, longitude, location_description) %>% 
	unique() %>%
	mutate(population_source = "Intratherm")

ol_locs_lpi <- lpi_ol %>% 
	select(genus_species, Latitude, Longitude, Location) %>% 
	rename(latitude = Latitude, longitude = Longitude, location_description = Location) %>% 
	unique() %>%
	mutate(population_source = "LPI")

ol_locs <- rbind(ol_locs_intra_gdpp, ol_locs_gdpp, ol_locs_intra_lpi, ol_locs_lpi) %>%
	arrange(genus_species, population_source)


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
#> [1] "sf" "data.frame"

map = ggplot(data = world) + 
	geom_sf(color = "dimgrey", fill = "white", size = 0.1) + 
	theme(panel.grid.major = element_line(colour = "light grey", size = 0.05),
		  panel.border = element_rect(colour = "transparent"), 
		  panel.background = element_rect(fill = "grey96"), legend.position = "none") + 
	coord_sf(expand = FALSE) +
	scale_x_continuous(breaks = c(-150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150)) + 
	scale_y_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90))  +
	geom_point(data = ol_locs, aes(y = latitude, x = longitude, 
								   col = genus_species), size = 0.5) 


## figure out how many CTmax we could potentially predict:
nrow(ol_locs[which(ol_locs$population_source != "Intratherm"),])
