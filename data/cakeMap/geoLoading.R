## file to load and process the geographic data for cakeMap
library(rgdal)
wards <- readOGR("/media/SAMSUNG/geodata/UK2011boundaries/", "england_wa_2011_gen_clipped")
plot(wards)

geonames <- read.csv("data/cakeMap/cars-raw.csv")
head(geonames)
geonames <- as.character(geonames[3:126,2])
head(wards@data)
head(wards$CODE)
summary(wards$CODE %in% geonames) # the overlap works
wards <- wards[(wards$CODE %in% geonames),]
plot(wards)

object.size(wards)/1000000
library(rgeos)
wards <- SpatialPolygonsDataFrame(gSimplify(wards, tol=200), wards@data)
object.size(wards)/1000000
save(wards, file="data/cakeMap/wards.RData")
