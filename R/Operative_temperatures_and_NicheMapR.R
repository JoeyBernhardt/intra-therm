######## Install nichemapR from github
require(devtools)
install_github("mrke/NicheMapR")
require(NicheMapR)
get.global.climate(folder="path") # download global climate database

######## Climate data with NicheMapR
loc <- c(x=-3.70256, y=40.4165) # define lon / lat

# NichemapR has maaany parameters, but for what we need, I think we just need to include: 
# location, minimum and maximum shade levels (%), and the reference height at which temp is simulated, which is related to the size of the animal (here 10 cm)
# I estimate max shade levels of the canopy using the LAI index (below), but just as an example this is for 100% shade:

micro <- micro_global(loc = loc, minshade = 0, maxshade = 100, Usrhyt = 0.10) 
micro_sun <- as.data.frame(micro$metout) # meteorological conditions in the sun 
micro_shade <- as.data.frame(micro$shadmet) # and in the shade
micro_soil <- as.data.frame(micro$soil) # if we want soil T (see operative temperatures below!)

head(micro_sun)
# most important here is: DOY, TIME (min), TALOC (Air temperature), RHLOC (relative humidity), VLOC (wing speed), and SOLR (radiation)

# this is the maximum temperature (air T in the sun, at noon, in the warmest month of the year)
max(micro_sun$TALOC) 

# This is how an average day of july looks like in Madrid
month = 7
day = unique(micro_sun$DOY)[month] # this is the day of the year corresponding to month 7

AIRT_sun <- subset(micro_sun, DOY == day)$TALOC
AIRT_shade <- subset(micro_shade, DOY == day)$TALOC

plot(AIRT_sun, ylab="Air temperature, ºC", xlab="Time, hour") # temperature in the sun (month 7)
points(AIRT_shade, pch=20) # temperature in the shade

############################################

######## More realistic shade levels
# We can either use max T in the sun (as above), or calculate "the actual level of shade" in that location using the LAI index,
# and then introduce it in NicheMapR as "maxshade".

require(ncdf4) 
require(raster)
require(rgdal)
require(GeoLight)
 
loc <- c(x=-3.70256, y=40.4165) # lon / lat

## Load LAI data 
nc_data <- nc_open("LAI_mean_monthly_1981-2015.nc4")
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
lai.array <- ncvar_get(nc_data, "LAI")

resolution = 1 # set resolution of the LAI layer to match with your data (e.g., 1º x 1º)

LAI_vector <- numeric(12)
for(month in 1:12){
  # extract LAI from layer
  if(month>=8){m=month-7}else{m=month+5} # 1: aug, 2:sept....
  lai.slice <- lai.array[, , m]
  r <- raster(t(lai.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  r <- aggregate(r, fact=resolution/res(r)[1])
  r <- flip(r, direction='y')
  LAI <- extract(r, cellFromXY(r, loc))
  
  LAI_vector[month] <- LAI 
}

LAI_vector # LAI values every month in our location

# Functions to calculate the proportion of light that makes it through the canopy (Campbell & Norman 1998, in Algar et al. 2018 GEB)
shade <- function(ac=0.8, z, LAI){
  Kbc <- sqrt((1 + tan(z*pi/180)^2) / 2.00132)
  omega_p <- exp(-sqrt(ac) * Kbc * LAI)
  return((1-omega_p)*100)
}

zenith <- function(sun, lon, lat){
  lon <- as.numeric(lon); lat <- as.numeric(lat)
  rad <- pi/180
  hourAngle <- sun$solarTime + lon - 180
  cosZenith <- (sin(rad * lat) * sun$sinSolarDec + cos(rad * 
                                                         lat) * sun$cosSolarDec * cos(rad * hourAngle))
  cosZenith[cosZenith > 1] <- 1
  cosZenith[cosZenith < -1] <- -1
  acos(cosZenith)/rad
}

shade_vector <- numeric(12)
for(month in 1:12){
  # compute zenith angle value
  s <- solar(as.POSIXct(paste0("2000-",month,"-15"," ",1:24,":","00")))
  zen <- zenith(s, lon=loc[1], lat=loc[2])
  zen[zen>90] <- 90 # hourly zenith angles
  
  # extract LAI from vector
  LAI <- LAI_vector[month]
  
  shade_vector[month] <- mean(shade(0.8, zen, LAI)) # daily average of shade levels
}

shade_vector # Monthly max shade levels (%) in our location

######## Introducing shade in NicheMapR
# Now we need to introduce the level of shade in the "maxshade" argument of micro_global. We need to run micro_global 12 times 
# (1 for each month) because one cannot introduce different shade levels for each month in NicheMapR

AIRT_sun <- AIRT_shade <- array(NA, dim=c(24, 12))
for(month in 1:12){
  maxshade <- shade_vector[month]
  micro <- micro_global(loc = loc, minshade = 0, maxshade = maxshade, Usrhyt = 0.10) 
  micro_sun <- as.data.frame(micro$metout)
  micro_shade <- as.data.frame(micro$shadmet)
  
  day = unique(micro_sun$DOY)[month] 
  
  AIRT_sun[,month] <- subset(micro_sun, DOY == day)$TALOC
  AIRT_shade[,month] <- subset(micro_shade, DOY == day)$TALOC
}

AIRT_sun # This array contains air temperatures in the sun in 12 columns (months) with 24 rows (hours)

month = 7

plot(AIRT_sun[,month], ylab="Air temperature, ºC", xlab="Time, hour") # temperature in the sun (month 7)
points(AIRT_shade[,month], pch=20) # temperature in the shade

# The temperature in the sun (0% of shade) looks exactly the same, but the temperaure in the shade is 
# quite higher than before (where we assumed 100% of shade)

# If we want the max T of the warmest month, taking canopy cover into account:
max(AIRT_shade)

############################################ OPERATIVE TEMPERATURES
# This is the function used by Algar et al. (2018) (based on Buckley 2007) to compute operative temperatures of lizards in the sun. 
# We can parameterize this function using NicheMapR (for air T, soil T, wind velocity, and solar radiation) and our body size data 
# (body length)

Te_function <- function(S,    # Solar radiation (Wm-2)
                        Ta,   # Air temperature (ºC)
                        Tg,   # Ground temperature (ºC)
                        v,    # Wind velocity (m/s)
                        d,    # Body length (m)
                        alpha_lw=0.965, # Skin absorbance (long wave) (Buckley 2007)
                        alpha_s=0.9,    # Skin absorbance (short wave) (Buckley 2007)
                        eps=0.965,      # Skin emissinvity (Buckley 2007)
                        Fa=0.5,         # View factor from sky (Algar et al. 2018)
                        Fg=0.5){        # View factor from the ground (Algar et al. 2018)
  
  ## Emission and absorption of long-wave radiation
  sigma = 5.67e-8 # W m-2 K-4, Stefan-Boltzmann constant
  eps_sky = 9.2e-6 * (Ta + 273)^2 # Clear sky emissivity of long wave radiation (Buckley 2007)
  La = eps_sky * sigma * (Ta + 273)^4 # long wave radiation from the sky
  
  eps_g = 0.965 # emisivity of the soil surface (Algar et al. 2018)
  Lg = eps_g * sigma * (Tg + 273)^4 # long wave radiation from the ground
  
  Rlw = alpha_lw * (Fa * La + Fg * Lg) # absorbed long-wave radiation
  
  ## Absorption of short-wave solar radiation
  # This is a simplification of Lauren's Buckley's model, which is focused on the geometry of a lizard. 
  # Here we just assume that the the upper half of the body received direct and scattered solar radiation
  Rsol = alpha_s * Fa * S 
  
  ## Operative temperature:
  cp = 29.3 # J mol-1 K-1, specific heat of the air
  Te = Ta + (Rsol + Rlw - eps * sigma * (Ta + 273)^4) / (4 * sigma * (Ta + 273)^3 + cp*(1.4 + 0.135*sqrt(v/d)))
  
  return(Te)
}

###### Using NicheMapR to compute body temperatures (from the data we simulated before):

month = 7
day = unique(micro_sun$DOY)[month]

AIRT <- subset(micro_sun, DOY == day)$TALOC # air T 
SOLR <- subset(micro_sun, DOY == day)$SOLR # solar radiation
SOILT <- subset(micro_soil, DOY == day)$D0 # solar radiation
VLOC <- subset(micro_sun, DOY == day)$VLOC  # wind speed

d = 0.05 # body length (m)
Te <- Te_function(S=SOLR, Ta=AIRT, Tg=SOILT, v=VLOC, d=d)

plot(Te, ylab="Operative temperature, ºC", xlab="Time, hour") # Operative temperatures in the sun (month 7)
points(AIRT, col="blue")

# The operative temperature is going to be higher than air T, especially when solar radiation is high. 


