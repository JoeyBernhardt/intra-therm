install.packages("rfishbase",
                 repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"),
                 type="source")
install.packages("devtools")
devtools::install_github("ropensci/rfishbase")

library("rfishbase")
library("broom")
library("tidyverse")


LongevityWild
Length
LTypeMaxM
CommonLengthF

setwd("Dropbox/intra-therm/data-processed/traits_data")
Comte_data<-read.csv("Comte_traits_data.csv")

species(as.character(Comte_data$Species[1:5]))

main_traits<-species(as.character(Comte_data$Species), 
        fields=c("LongevityWild", "Length", "LTypeMaxM", "CommonLengthF", "Weight", 
                 "AnaCat", "BodyShapeI"))

species(as.character(Comte_data$Species[1:5]), fields=c("Fecundity"))

fecundity(as.character(Comte_data$Species[1:5]))

fec<-fecundity(species_list = as.character(Comte_data$Species), 
       fields = c("FecundityMin", "FecundityMax", "FecundityType"), query = NULL, limit = 200)
       
dev<-larvae(species_list = as.character(Comte_data$Species), 
  fields = c("PlaceofDevelopment", "LarvalDurationMin", "LarvalDurationMax"), query = NULL, limit = 200)

detailed_traits<-left_join(fec, dev)
comte_fishbase<-left_join(main_traits, detailed_traits)

#check for species coverrage, names using Taxise
#Trophic level
#adult habitat e.g. marine benthic, freshwater



write.csv(comte_fishbase, "comte_fishbase.csv")
