## data cleaning ##

## load packages 
library(taxize)
library(tidyverse)

## note: taxize code first used on may 2020 version of database, again used on precleaning database
data <- read.csv("/Users/nikkimoore/Documents/intra-therm/data-raw/intratherm-may-2020-precleaning.csv")



## taxize all data so genus and species are correct
###################################################
## note on taxize synonyms: acceptedname with return value NA means name given was accepted name, do nothing 

taxa <- data$genus_species ## create dataframe of names to check
taxa<- data.frame(taxa)


syns <- unique(taxa)
tsn_search <- get_tsn(as.character(syns$taxa), accepted = FALSE) ## find tsn for each unique taxa
tsns <- data.frame(tsn_search)
tsns$taxa <- syns$taxa
syns <- tsns

found <- syns %>%
  subset(match == "found") 
  
report <- lapply(found$ids, itis_acceptname)
report_df <- data.frame(matrix(unlist(report), nrow=208, byrow=T),stringsAsFactors=FALSE)

found <- found %>%
  mutate(genus_species_corrected = report_df$X2)

## merge short unique list to long list of all taxa
merged_unique <- left_join(syns, found)
merged <- left_join(taxa, merged_unique)

  
## if names found are not accepted names, then change to accepted name
i = 1
while (i < length(merged$taxa)) {
  if (!is.na(merged$genus_species_corrected[i])) {
    merged$taxa[i] <- merged$genus_species_corrected[i]
  }
  i = i+1
}

## create new genus and species columns, correct original dataset
split <- str_split_fixed(merged$taxa, pattern = " ", n = 2)
merged$genus <- split[,1]
merged$species <- split[,2]

## update the database :)
test_data <- data %>%
  mutate(genus = merged$genus) %>%
  mutate(species = merged$species) %>%
  mutate(genus_species = merged$taxa) 

data <- test_data
data_protected <- data




## taxize all data to update higher taxonomy columns 
####################################################
higher_tax <- tax_name(as.character(unique(taxa$taxa)), get = c("phylum","class","order","family")) 
higher_tax <- higher_tax[, -1]
colnames(higher_tax)[2] <- "genus_species"

## get rid of old taxonomy columns 
data <- data %>%
  select(-phylum.x,-order,-order.x,-family,-family.x,-class,-class.x) 

## merge based on genus_species, leave all fields blank for taxa that were not found to be manually inputted
data <- left_join(data, higher_tax)

## move columns:
data <- data %>%
  select(intratherm_id, genus_species, genus, species, phylum, class, order, family, everything())  

## 
## for all taxa where higher taxonomy not found: data filled in manually using a google search 




## change 'original_compilation' for entries by Nikki with field missing into intratherm_team 
#############################################################################################
## flag entries input by Nikki 
data <- data %>%
  mutate(extractor) %>% 
  group_by(extractor) %>% 
  mutate(flag = grepl(extractor, pattern = "Nikki")) 

## if flagged as Nikki's and if original compilation is missing, write original compilation as intratherm_team
i = 1
while(i < length(data$original_compilation)) {
  if(data$flag[i] == TRUE && is.na(data$original_compilation[i])) {
    data$original_compilation[i] <- 'intratherm_team' 
  }
  i = i + 1
}

## remove flag
data <- data %>%
  select(-flag)

## check:
data %>%
  subset(extractor == "Nikki") %>% View






## clean realm_general2
#######################
## categories allowed: freshwater, terrestrial and marine based on the adult lifestage 
## investigate NA
unique(data$realm_general2)

## flag entries with value Aquatic & terrestrial
data_test <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "Aquatic & terrestrial")) 

  
## inspected all flagged, all are amphibians or missing, since spend adult life on land change to terrestrial 
## otherwise mark as missing taxonomy 
data_flagged <- data_test %>%
  filter(flag == TRUE) %>% 
  mutate(realm_general2 = ifelse(grepl(class, pattern = "Amphibia"), "Terrestrial", "missing higher taxonomy")) 

##change flagged data points to proper realm in database by removing flagged and readding 
data_test <- data_test %>%
  filter(flag == FALSE) 

data_test <- rbind(data_test, data_flagged)
data_test <- data_test %>%
  select(-flag)

##update the real data 
data <- data_test
data_protected <- data

unique(data$realm_general2)

## flag entries with value Aquatic
data_test <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "Aquatic")) 

## inspected all flagged, either Amphibia or Teleosti
data_flagged <- data_test %>%
  filter(flag == TRUE) 

## change Amphibia to Terrestrial 
data_amphibs <- data_flagged %>%
  filter(class == "Amphibia") %>% 
  mutate(realm_general2 = "Terrestrial") 

data_test <- data %>%
  mutate(flag = ((grepl(realm_general2, pattern = "Aquatic")) & (grepl(class, pattern = "Amphibia")))) %>% ##flag only amphibians
  filter(flag == FALSE) 

data_test <- rbind(data_test, data_amphibs)
data_test <- data_test %>% 
  select(-flag)

data <- data_test

## reflag Aquatic and inspect
data_test <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "Aquatic"))

data_flagged <- data_test %>%
  filter(flag == TRUE) 

## composed of family Cyprinidae and Zoarcidae, Cypr are freshwater and Zoarc are anadromous but spend most time in ocean 
unique(data_flagged$family)

## change Cyprinidae to freshwater 
data_cypr <- data_flagged %>%
  filter(family == "Cyprinidae") %>% 
  mutate(realm_general2 = "Freshwater") 

## remove Cpryn and reput with new realm
data_test <- data_test %>%
  mutate(flag = ((grepl(realm_general2, pattern = "Aquatic")) & (grepl(family, pattern = "Cyprinidae")))) %>% ##flag only Cypr
  filter(flag == FALSE) 

data_test <- rbind(data_test, data_cypr)
data_test <- data_test %>% 
  select(-flag)

data <- data_test

## change Zoarcidae to marine 
data_zoar <- data_flagged %>%
  filter(family == "Zoarcidae") %>% 
  mutate(realm_general2 = "Marine") 

data_test <- data_test %>%
  mutate(flag = ((grepl(realm_general2, pattern = "Aquatic")) & (grepl(family, pattern = "Zoarcidae")))) %>% ##flag only Zoar with aquatic
  filter(flag == FALSE) ## remove

data_test <- rbind(data_test, data_zoar)
data_test <- data_test %>%
  select(-flag)

data <- data_test

## make sure all is well and no aquatic are left: 
data %>%
  subset(realm_general2 == "Aquatic") %>% View 

data_protected <- data






## clean life_stage.x 
######################
unique(data$life_stage.x)

## fix adults
data_test <- data %>%
  mutate(is_adult = ifelse(is.na(life_stage.x), "FALSE", if_else(str_detect(life_stage.x, "juvenile|sub|Juvenile|Sub|juveniles"), 
                                                                 "FALSE", ifelse(str_detect(life_stage.x, "adult|Adult|adults"), 
                                                                                 "TRUE", "FALSE")))) 
data_adults <- data_test %>%
  filter(is_adult == TRUE) %>%
  mutate(life_stage.x = "Adult")

data_test <- data_test %>%
  filter(is_adult == FALSE) 

data_test <- rbind(data_test, data_adults)

data_test %>%
  select(-is_adult)

data <- data_test

unique(data_test$life_stage.x)

## fix juveniles
data_test <- data %>%
  mutate(is_juv = ifelse(is.na(life_stage.x), "FALSE", 
                         if_else(str_detect(life_stage.x, "sub-adult"), "TRUE", 
                         ifelse(str_detect(life_stage.x, "adult|Adult"), "FALSE", 
                         ifelse(str_detect(life_stage.x, "juvenile|Juvenile|sub-adult|fingerling|elver|fry"), "TRUE", "FALSE"))))) 
  

data_juv <- data_test %>%
  filter(is_juv == TRUE) %>%
  mutate(life_stage.x = "Juvenile")

data_test <- data_test %>%
  filter(is_juv == FALSE) 

data_test <- rbind(data_test, data_juv)

data_test %>%
  select(-is_juv)

data <- data_test

unique(data_test$life_stage.x)


## fix larvae
data_test <- data %>%
  mutate(is_larvae = ifelse(is.na(life_stage.x), "FALSE", 
                         if_else(str_detect(life_stage.x, "larva"), "TRUE", "FALSE"))) 

data_larvae <- data_test %>%
  filter(is_larvae == TRUE) %>%
  mutate(life_stage.x = "Larva")

data_test <- data_test %>%
  filter(is_larvae == FALSE) 

data_test <- rbind(data_test, data_larvae)

data_test %>%
  select(-is_larvae)

data <- data_test

unique(data_test$life_stage.x)


## fix juveniles and adults to "Juvenile and adult"
data_test <- data %>%
  mutate(is_mixed = ifelse(is.na(life_stage.x), "FALSE", 
                            if_else(str_detect(life_stage.x, "juveniles adult|juvenile and"), "TRUE", "FALSE"))) 

data_mixed <- data_test %>%
  filter(is_mixed == TRUE) %>%
  mutate(life_stage.x = "Juvenile and adult")

data_test <- data_test %>%
  filter(is_mixed == FALSE) 

data_test <- rbind(data_test, data_mixed)

data_test %>%
  select(-is_mixed)

data <- data_test

unique(data_test$life_stage.x)


## fix embryos
data_test <- data %>%
  mutate(is_emb = ifelse(is.na(life_stage.x), "FALSE", 
                           if_else(str_detect(life_stage.x, "embryo"), "TRUE", "FALSE"))) 

data_emb <- data_test %>%
  filter(is_emb == TRUE) %>%
  mutate(life_stage.x = "Embryo")

data_test <- data_test %>%
  filter(is_emb == FALSE) 

data_test <- rbind(data_test, data_emb)

data_test %>%
  select(-is_emb)

data <- data_test

unique(data_test$life_stage.x)


## fix unknown
data_test <- data %>%
  mutate(is_unk = ifelse(is.na(life_stage.x), "FALSE", 
                         if_else(str_detect(life_stage.x, "unknown"), "TRUE", "FALSE"))) 

data_unk <- data_test %>%
  filter(is_unk == TRUE) %>%
  mutate(life_stage.x = "Unknown")

data_test <- data_test %>%
  filter(is_unk == FALSE) 

data_test <- rbind(data_test, data_unk)

data_test %>%
  select(-is_unk)

data <- data_test

unique(data_test$life_stage.x)

data_protected <- data





## fix dispersal distance january 10 issue 
##########################################
unique(data$dispersal_distance_category)

data$dispersal_distance_category <- as.character(data$dispersal_distance_category)

data_test <- data %>%
  mutate(is_Jan10 = ifelse(is.na(dispersal_distance_category), "FALSE", 
                         ifelse(str_detect(dispersal_distance_category, "10-Jan"), "TRUE", "FALSE"))) 

data_jan10 <- data_test %>%
  filter(is_Jan10 == TRUE) %>%
  mutate(dispersal_distance_category = "1-10")

data_test <- data_test %>%
  filter(is_Jan10 == FALSE) 

data_test <- rbind(data_test, data_jan10)

data_test %>%
  select(-is_Jan10)

data <- data_test

data_protected <- data






## fix ranges in acclim_temp column by taking average of values
###############################################################
levels(data$acclim_temp)

data_sub <- data %>%
  mutate(has_hyphen = ifelse(str_detect(acclim_temp, "-"), "TRUE", "FALSE")) %>% ## detect ones with hyphen
  filter(has_hyphen == TRUE) ##subset to data with hyphen 

split <- str_split_fixed(data_sub$acclim_temp, pattern = "-", n = 2)
split <- data.frame(split)

split <- split %>%
  mutate(is_empty = ifelse(str_detect(X1, ""), (ifelse(str_detect(X2, ""), "FALSE", "TRUE")), "TRUE")) ## detect ones that are   empty to make sure no hypthens represented negative values 

data_sub$has_hyphen[which(split$is_empty == TRUE)] <- FALSE ##unflag ones with hyphen not surrounded by two things

split <- split %>% 
  filter(is_empty == FALSE) %>%
  mutate(avg = (as.numeric(as.character(X1)) + as.numeric(as.character(X2))) / 2) ## make new column representing average

data_sub <- data_sub %>%
  mutate(acclim_temp = ifelse((has_hyphen == TRUE), split$avg, as.numeric(as.character(data_sub$acclim_temp)))) %>%
  mutate(acclim_temp = as.factor(acclim_temp))

data_test <- data %>%
  mutate(has_hyphen = ifelse(str_detect(acclim_temp, "-"), "TRUE", "FALSE")) 

get_rid <- which(data_test$has_hyphen == TRUE)
data_test <- data_test[-get_rid,] ## remove rows where has_hyphen is TRUE

merged <- rbind(data_test, data_sub) ## add back hyphenated with new values 

data_test <- merged %>%
  select(-has_hyphen)

data <- data_test
data_protected <- data




## clean elevation of collection
unique(data$elevation_of_collection)
## appears that some are ranges and some may be in ft vs m - fix later if needed 

## fix 20-May
data$elevation_of_collection <- as.character(data$elevation_of_collection)

data_test <- data %>%
  mutate(is_May20 = ifelse(is.na(elevation_of_collection), "FALSE", 
                           ifelse(str_detect(elevation_of_collection, "20-May"), "TRUE", "FALSE"))) 

data_may20 <- data_test %>%
  filter(is_May20 == TRUE) %>%
  mutate(elevation_of_collection = "2005")

data_test <- data_test %>%
  filter(is_May20 == FALSE) 

data_test <- rbind(data_test, data_may20)

data_test %>%
  select(-is_May20)

data <- data_test

data_protected <- data




## update population id 
########################
## new population id: includes latitude AND elevation as unique identifier 
data_test <- data %>%
  mutate(population_id = paste(genus_species, "_", latitude, "_", elevation_of_collection)) 

data <- data_test

data_protected <- data




## remove all non-wild hatchery containing rows in loc description 
##################################################################
##subset to hatchery data 
data_hatch <- data %>%
  mutate(is_hatchery = ifelse(str_detect(location_description, "hatchery|Hatchery"), "TRUE", "FALSE")) %>%
  filter(is_hatchery == TRUE) %>%
  select(-is_hatchery)

## flag wild populations as keep in subset
data_hatch <- data_hatch %>%
  mutate(keep = ifelse(str_detect(location_description, "wild|Wild"), "TRUE", "FALSE")) %>% 
  filter(keep == TRUE) %>%
  select(-keep)

## remove all hatchery data from data, re-add ones flagged keep
hatchery <- which(str_detect(data$location_description, "hatchery|Hatchery"))
data_test <- data[-hatchery,]
merged <- rbind(data_test, data_hatch)

data <- merged

data_protected <- data



## round all latitudes and longitudes that were marked inferred?





## remove columns we do not need right now: 
bad_columns <- c("realm_general3", 
                 "realm_specific", 
                 "realm_general", 
                 "n_cat", 
                 "acclim_time_life_120", 
                 "log_acclim_time", 
                 "safety", 
                 "log_elevation", 
                 "elev_min", 
                 "iucn_2104risk", 
                 "rate_acclimation_c_day", 
                 "weight_g", 
                 "trophic_position", 
                 "source_logic_for_trophic_position", 
                 "dispersal_type_walking_may_need_to_be_reworded", 
                 "logic_source_for_dispersal_type",
                 "cold_season_dormancy",
                 "highland_specialist",
                 "source_logic_for_cold_season_dormancy", 
                 "source_logic_for_highland_specialist",
                 "no_acclimation", 
                 "life_stage",
                 "dispersal_distance2_category",
                 "logic_source_for_dispersal_distance2_category")

data_test <- data[, !(names(data) %in% bad_columns)]

## remove db column that was added accidnetally with higher taxonomy 
data_test <- data_test[,-70]


data <- data_test


write.csv(data, "/Users/nikkimoore/Documents/intra-therm/data-processed/intratherm-may-2020-squeaky-clean.csv", row.names = FALSE)



## then went and added all missing taxa manually to squeaky clean file, reimported to continue cleaning 
data <- read.csv("/Users/nikkimoore/Documents/intra-therm/data-processed/intratherm-may-2020-squeaky-clean.csv")


## make sure no missing realms for ones that had no higher taxonomy 
unique(data$realm_general2)

## change realm to terresrtrial since amphibian
## flag entries with value Aquatic
data_test <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "missing higher taxonomy")) 

## inspected all flagged, either Amphibia or Teleosti
data_flagged <- data_test %>%
  filter(flag == TRUE)

## change Amphibia to Terrestrial 
data_amphibs <- data_flagged %>%
  filter(class == "Amphibia") %>% 
  mutate(realm_general2 = "Terrestrial") 

data_test <- data_test %>%
  filter(flag == FALSE)

data_test <- rbind(data_test, data_amphibs)
data_test <- data_test %>% 
  select(-flag)

data <- data_test


## rewrite updated version to data file:
write.csv(data, "/Users/nikkimoore/Documents/intra-therm/data-processed/intratherm-may-2020-squeaky-clean.csv", row.names = FALSE)



