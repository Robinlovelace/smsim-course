## file to load and process the geographic data for cakeMap
library(rgdal)
wards <- readOGR("/media/SAMSUNG/geodata/2011boundaries/", "england_wa_2011_gen_clipped")
