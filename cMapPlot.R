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

# set up bounding box
bb <- b <- bbox(wards)
bb[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
bb[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.7 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.7 + mean(b[2, ])


# map the result!
ggplot() + 
  geom_polygon(data=wardsF, aes(long, lat, group=group, fill=avCake), color = "black", alpha=0.2)
library(ggmap)
library(rgdal)
baseMap <- get_map(location=bb, maptype="terrain")
# baseMap <- get_map(location=b, zoom=10, source='osm')
#  baseMap <- get_map(location=b, source='stamen')

ggmap(baseMap) + 
  geom_polygon(data=wardsF, aes(long, lat, group=group, fill=avCake), alpha=0.5) + 
  geom_path(data=wardsF, aes(long, lat, group=group), color="black", alpha=0.3) +
  scale_fill_continuous(low = "green", high = "red") + xlim(bb[1,]) + ylim(bb[2,]) + 
  theme_nothing()
## ggsave("figures/cakeMap.png")

# analysis
imd <- read.csv("data/cakeMap/inc-est-2001.csv")
summary(imd$NAME %in% wards$NAME)
summary(pmatch(wards$NAME, imd$NAME))
which(imd$NAME %in% wards$NAME) %in% pmatch(wards$NAME, imd$NAME)
head(join(wards@data, imd))
wards@data <- join(wards@data, imd)
plot(wards$Avinc, wards$avCake)
cor(wards$Avinc, wards$avCake, use='complete.obs')
