### Proportional probabilities integerisation algorithm
### Run in current state after running IPF script in smsim-course repo - 
# e.g. etsim.R, simple.R or cMap.R

intp <- floor(fw) # truncated final weights
ints <- as.list(1:nrow(cons)) # Names of integer results, in ordered list
intall <- list(1:nrow(cons))  # Row data, ready to be filled with integer weights
intcat <- ints
intagg <- cons * 0 # Aggregate area stats
prop.weights <- matrix(1, nrow=nrow(ind), ncol=nrow(cons))
pops <- popints <- rowSums(con1)
cakes <- data.frame(avCake = rep(0,nrow(cons)), sdCake = rep(0,nrow(cons)))

set.seed(0) # Include this line to ensure repeatable results

# Sample individuals based on their proportional probabilities
for (i in 1:nrow(cons)){
  ints[[i]] <- sample(which(fw[,i] > 0), size = sum(con1[i,]), 
                      prob=prop.weights[,i], replace = T) 
  popints[i] <- length(ints[[i]])
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
object.size(intall) / 1000000
object.size(intagg)

head(pops)
