############################################
#### From the IPF-performance-testing github repo  
#### https://github.com/Robinlovelace/IPF-performance-testing
############################################

ind <- read.csv("data/simple/ind.csv") # load the individual level data
cons <- read.csv("data/simple/cons.csv") # load aggregate constraints

con1 <- cons[,1:2]
con2 <- cons[,3:4]
# inspect the data we have loaded
ind
cons[1:2,]

source("data/simple/categorise.R") # categorise the individual level variable
ind.cat # take a look at the output

colSums(ind.cat)

weights <- array(1, dim=c(nrow(ind),nrow(cons))) 

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- matrix(rep(colSums(ind.cat), times = nrow(cons)), nrow = nrow(cons) )
sum(sqrt((ind.agg - cons)^2)) ## the total absolute error 

############## The IPF part #############

# Re-weighting for constraint 1 via IPF 
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con1)){
 weights[which(ind.cat[,i] == 1),j] <- cons[j,i] / ind.agg[j,i]}}

for (i in 1:nrow(cons)){ # convert con1 weights back into aggregates
  ind.agg[i,]   <- colSums(ind.cat * weights[,i])}

# test results for first row (not necessary for model)
ind.agg[1,] - cons[1,]
sum(sqrt((ind.agg - cons)^2)) ## the total absolute error 
weights2 <- weights

# Re-weighting for constraint 2 via IPF 
for (j in 1:nrow(cons)){
   for(i in 1:ncol(con2) + ncol(con1)){
 weights[which(ind.cat[,i] == 1),j] <- cons[j,i] / ind.agg[j,i]}}

for (i in 1:nrow(cons)){ # convert con1 weights back into aggregates
  ind.agg[i,]   <- colSums(ind.cat * weights2[,i] * weights[,i])}

ind.agg[1,] - cons[1,]
sum(sqrt((ind.agg - cons)^2)) ## the total absolute error 
