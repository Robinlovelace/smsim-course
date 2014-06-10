# loading the input data
# install.packages("gdata") # install gdata package if not already installed
library(gdata) # load package for reading data
ind <- read.xls("msim.xlsx", sheet="SAMPLING")
head(ind)

# load the constraints and check the totals add up
con1 <- read.xls("msim.xlsx", "GENDER")[-1]; sum(con1)
con2 <- read.xls("msim.xlsx", "AGE")[-1]; sum(con2)
con3 <- read.xls("msim.xlsx", "ETHNICITY")[-1]; sum(con3)
num.cons <- 3  # n. constraints - can set automatically: length(which(grepl("con[1-9]",ls()))))
cons <- cbind(con1, con2, con3)
cat.names <- names(cons)

# creating 0/1 matrix representation of ind. data
gender.cat <- model.matrix(~ind$GENDER -1 )
age.cat <- model.matrix(~ind$AGE -1)
eth.cat <- model.matrix(~ind$ETHNICITY - 1)
ind.cat <-  cbind(gender.cat, age.cat, eth.cat)
names(ind.cat) <- cat.names

# create weight matrix and initialise
weights <- array(dim=c(nrow(ind),nrow(cons),num.cons+1)) 
weights[,,1] <- 1 # sets initial weights to 1

# create aggregated smsim output matrix and add values from ind
ind.agg <- array(dim=c(nrow(cons),ncol(cons),num.cons+1))
for (i in 1:nrow(cons)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,1]}
ind.agg[,,1] # look at what we've created - individual level data comparable w. cons

############## The IPF part #############

# Re-weighting for constraint 1 via IPF 
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,2] <- con1[j,i] / ind.agg[j,i,1]}}
for (i in 1:nrow(cons)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2])}
# test results for first row (not necessary for model)
ind.agg[1,,2] - cons[1,] # should be zero for age/sex
cor(as.numeric(as.vector(ind.agg[,,2])), as.numeric(as.matrix(cons))) # how good is the correlation (fit)

# second constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con2) + ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,3] <- cons[j,i] / ind.agg[j,i,2]}}  
for (i in 1:nrow(cons)){ # convert con2 back into aggregate
  ind.agg[i,,3] <- colSums(ind.cat * weights[,i,1] * weights[,i,2] * weights[,i,3])}
ind.agg[1,,3] - cons[1,] # should be close to zero for new constraint
cor(as.numeric(as.vector(ind.agg[,,3])), as.numeric(as.matrix(cons))) # how good is the correlation (fit)

# third constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con3) + ncol(con1) + ncol(con2)){
    weights[which(ind.cat[,i] == 1),j,4] <- cons[j,i] / ind.agg[j,i,3]}}
for (i in 1:nrow(cons)){ # convert con3 back into aggregate
  ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2] * weights[,i,3] * weights[,i,4])}
ind.agg[1:3,,4] - cons[1:3,] # test the result

cor(as.numeric(as.vector(ind.agg[,,4])), as.numeric(as.matrix(cons))) # how good is the correlation (fit)
# you get a perfect fit between constraint data and results of model
# why? because of the final weights:
fw <- weights[,,1] * weights[,,2] * weights[,,3] * weights[,,4]
head(fw) # cols are zones, rows are individuals

############ Integerisation phase ###################

intall <- ints <- as.list(1:nrow(cons)) # Names of integer indices (ints), and integer populations (intall) in ordered list
intagg <- cons * 0 # Aggregate area stats - set to 0 to avoid confusion
f <- floor(fw) # truncated weights
d <- fw - f

set.seed(0) # Include this line to ensure repeatable results

# Sample individuals based on their proportional probabilities
for (i in 1:nrow(cons)){
  if(max(f[,i]) == 0) f[which.max(fw[,i]),i] <- 1 # ensures model will run in case max(i5.w5 < 1) thanks to Eveline van Leeuwen
  ints[[i]] <- rep(which(fw[,i] > 0), f[,i])
  s <- sample(which(fw[,i] >= 0), size = sum(con1[i,]) - sum(f[,i]) , # sample using decimal weights to 'top up' selection
              prob=d[,i], replace = F) 
  ints[[i]] <- c(ints[[i]], s) # add the sampled population to the one selected from truncated weights
  intall[[i]] <- ind[ints[[i]],] # Pulls all other data from index
  source("areaCat.R") # save the aggregate data
  intagg[i,] <- colSums(area.cat) 
}

intall

intall.df <- cbind(intall[[1]], zone = 1)
head(intall.df)
for(i in 2:nrow(cons)){ # run for all zones with 1:length(intall)
  intall.df <- rbind(intall.df, cbind(intall[[i]], zone = i))
}
summary(intall.df[ intall.df$zone == 1, ]) # test the output
summary(intall.df[ intall.df$zone == 3, ]) # test the output
summary(intall.df)

