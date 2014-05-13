# this script turns the intall object (a list) into a data frame 

head(intall[[1]]) # top of 1st individuals
# intall is a list of data frames
# each element is a zone

# Let's join together the zones.
intall.df <- cbind(intall[[1]], zone = 1)
head(intall.df)
for(i in 2:10){ # run for all zones with 1:length(intall)
  intall.df <- rbind(intall.df, cbind(intall[[i]], zone = i))
}

summary(intall.df[ intall.df$zone == 3, ])
