### Proportional probabilities integerisation algorithm
### Run in current state after running IPF script in smsim-course repo - 
# e.g. cMap.R
# fw means 'final weights' and is used for the integerisation

# set up the objects we'll be using subsequently
intall <- ints <- as.list(1:nrow(cons)) # Names of integer indices (ints), and integer populations (intall) in ordered list
intagg <- cons * 0 # Aggregate area stats - set to 0 to avoid confusion
cakes <- data.frame(avCake = rep(0,nrow(cons)), sdCake = rep(0,nrow(cons)))

set.seed(0) # Include this line to ensure repeatable results

# Sample individuals based on their proportional probabilities
for (i in 1:nrow(cons)){
  ints[[i]] <- sample(which(fw[,i] > 0), size = sum(con1[i,]), 
                      prob=fw[,i], replace = T) 
  intall[[i]] <- ind[ints[[i]],] # Pulls all other data from index
  source("data/cakeMap/area.cat.R") # save the aggregate data
  intagg[i,] <- colSums(area.cat) 
  cakes$avCake[i] <- mean(intall[[i]]$avnumcakes)
  cakes$sdCake[i] <- sd(intall[[i]]$avnumcakes)
}

# Diagnostic checks (does not affect results)

summary(fw[,i]) # Summary statistics of all IPF weights for zone i (mean should be low)
summary(fw[ints[[i]],i]) # Summary statistics of all IPF weights for SELECTED INDIVIDUALS for zone i (mean should be higher)
summary(fw[ints[[i]][which(duplicated(ints[[i]]) == T)],i]) # Summary statistics of replicated weights in zone 1 (mean should be higher still)
object.size(intall) / 1000000 # size of the resulting individual level dataset
object.size(intagg)

# intagg.pp <- intagg # save this result for comparison with other methods of integerisation

# to reformat the intall dataset, uncomment the following code:
# intall.df <- cbind(intall[[1]], zone = 1)
# head(intall.df)
# for(i in 2:10){ # run for all zones with 1:length(intall)
#   intall.df <- rbind(intall.df, cbind(intall[[i]], zone = i))
# }
# summary(intall.df[ intall.df$zone == 3, ]) # test the output
