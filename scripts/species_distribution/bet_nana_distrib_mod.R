#Load packages####
library(raster)
library(dismo)
library(maxnet)
library(rJava)
library(rgbif)
library(scrubr)
library(maps)
library(maptools)
library(rgeos)
library(raster)
library(rgdal)
library(dismo)
library(sf)
library(geodata)

#Data preparation####

#Import observations from GBIF
myspecies <- c("Betula nana L.")
gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 20000)
observation_data <- gbif_data$data[ , c("scientificName","decimalLongitude", "decimalLatitude", "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references")]
head(observation_data)

#round up the observations to 3 decimals
observation_data$decimalLongitude <- round(observation_data$decimalLongitude, 3)
observation_data$decimalLatitude <- round(observation_data$decimalLatitude, 3)


# load the world map
data("wrld_simpl")

plot(wrld_simpl)
points(observation_data$decimalLongitude, observation_data$decimalLatitude, col="red", pch=1, cex = 0.3) #check that the data makes sense

#remove duplicated data (this is to avoid the a bias of the data)

dups <- duplicated(observation_data[,c("decimalLatitude", "decimalLongitude")])
sum(dups) #this shows the number of duplicates in the df

observation_data <- observation_data[!dups,] #removes the duplicated data

#check again
dups <- duplicated(observation_data[,c("decimalLatitude", "decimalLongitude")])
sum(dups) #now it should be 0

#export the clean data
write.csv(observation_data, "data/distribution/bnanagbif_clean.csv", row.names = FALSE)

#load  data from worldclim

#climate data
climate <- getData('worldclim', download = T, var = 'bio', res = 2.5, path = "data/climate/")
plot(climate) #check the downloaded data

#elevation data
elevation <- elevation_global(res = 2.5, path = "data/distribution/")
plot(elevation) 

#extract data
#get the X and Y coordinates for observations
observation <- observation_data[, c("decimalLongitude", "decimalLatitude")]
head(observation)

obsclim <- extract(climate, observation) #extract info of climate from the observations
obselev <- extract(elevation, observation) #extract info of elevation from the observations

#prepare the climatic model####
bioclim.model <-  bioclim(obsclim)
pairs(bioclim.model, pa = 'p')


#Establecer los predictores####

predictors <- stack(climate$bio1, climate$bio2, climate$bio3, climate$bio4, climate$bio5, climate$bio6, climate$bio7, climate$bio8, climate$bio9, climate$bio10, climate$bio11, climate$bio12, climate$bio13, climate$bio14, climate$bio15, climate$bio16, climate$bio17, climate$bio18, climate$bio19, elevation$wc2.1_2.5m_elev)

predictions <- predict(predictors, bioclim.model)

plot(predictions, axes = T)

#Export the final raster

writeRaster(predictions, filename = "data/distribution/betulanana_distribution.tif", format = "GTiff")
