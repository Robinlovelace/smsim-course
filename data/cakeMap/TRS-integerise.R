# truncate, replicate sample method

set.seed(0) # include this line to ensure repeatable results
for (i in 1:nrow(cons)){
  if(max(intp[,i]) ==0) intp[which.max(fw[,i]),i] <- 1 # ensures model will run in case max(i5.w5 < 1) thanks to Eveline van Leeuwen
  index <- cbind((which(intp[,i]>0)) # generates index
                 ,intp[which(intp[,i]>0),i]) # integer weights after rounding
  ints[[i]] <- index[rep(1:nrow(index),index[,2])] # clone individuals
  ints[[i]] <- # sample based on non-integer weight 
    c(ints[[i]], sample(which(fw[,i]> -1), size = abs((pops - length(ints[[i]]))), prob=fw[,i] - floor(fw[,i])))
  intall[[i]] <- ind[ints[[i]],] # pulls all other data from index
  source("data/cakeMap/area.cat.R")
  intagg[i,] <- colSums(area.cat) 
  cakes$avCake[i] <- mean(intall[[i]]$avnumcakes)
  cakes$sdCake[i] <- sd(intall[[i]]$avnumcakes)
}
