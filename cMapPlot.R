# script for plotting the output of cMap
# must be run after cMap.R and TRS-integerisation.R

# load the prerequisite packages
x <- c("ggplot2", "dplyr", "sp")
lapply(x, require, character.only = T)

# save geographic names to the cakes output
geonames <- read.csv("data/cakeMap/cars-raw.csv")
head(geonames)
geonames <- as.character(geonames[3:126,2])
geocakes <- cbind(geonames, cakes)
head(geocakes)

# load the geographic data
load("data/cakeMap/wards.RData")
wards <- spTransform(wards, CRSobj=CRS("+init=epsg:4326")) # transform CRS
??join

# prepare to join the geographic data with cake data
names(wards)
names(geocakes)[1] <- names(wards)[1] <- "id" # rename geocakes' geonames for join
head(geocakes)
head(wards@data)
head(join(wards@data, geocakes))
wards@data <- join(wards@data, geocakes)

# fortify the data for ggplot2
wardsF <- fortify(wards, region="id")
head(wardsF)
names(wards)
wardsF <- merge(wardsF, wards@data, by = "id")
head(wardsF) # see average cake consumption added

# map the result!
ggplot() + geom_polygon(data=wardsF, aes(long, lat, group=group, fill=avCake), color = "black", alpha=0.6) 
library(ggmap)
library(rgdal)
baseMap <- get_map(location=bbox(wards))
ggmap(baseMap) + 
  geom_polygon(data=wardsF, aes(long, lat, group=group, fill=avCake), color = "black", alpha=0.5) +
  scale_fill_continuous(low = "green", high = "red")

imd <- read.csv("data/cakeMap/inc-est-2001.csv")
head(imd)
head(join(wards@data, imd))
wards@data <- join(wards@data, imd)
plot(wards$Avinc, wards$avCake)
