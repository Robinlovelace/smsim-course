############################################
#### IPFinR a script for IPF in R 
#### Robin Lovelace (2013)
############################################

num.its <- 3 # how many iterations will we run?

ind <- read.csv("data/simple/ind.csv")
con1 <- read.csv("data/simple/age.csv") # add the age constraint data 
con2 <- read.csv("data/simple/sex.csv") # add the sex constraint data
num.cons <- 2 # n. constraints 

# checking that totals add up, add more if need's be
sum(con1)
sum(con2) # check populations are equal
cons <- cbind(con1, con2)

# setting-up reweighting data
category.labels <- names(cons) # should be correct from cons.R

# set-up aggregate values - column for each category
source("data/simple/categorise.R") # this script must be customised to input data
ind.cat[1:3, 1:4] # take a look at the first 2 rows and 4 columns of ind.cat - as expected?
# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct?
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(cons), num.cons + 1)) 
weights[,,1] <- 1 # sets initial weights to 1
ini.ws <- weights[,,1]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(cons),ncol(cons),num.cons + 1))
for (i in 1:nrow(cons)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,1]}
ind.agg[,,1] # look at what we've created - n. individuals replicated throughout

############## The IPF part #############

# Re-weighting for constraint 1 via IPF 
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con1)){
 weights[which(ind.cat[,i] == 1),j,2] <- con1[j,i] / ind.agg[j,i,1]}}
for (i in 1:nrow(cons)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2])}
# test results for first row (not necessary for model)
ind.agg[1,,2] - cons[1,] # should be zero for 1st constraint

# second constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con2) + ncol(con1)){
  weights[which(ind.cat[,i] == 1),j,3] <- cons[j,i] / ind.agg[j,i,2]}}  
for (i in 1:nrow(cons)){ # convert con2 back into aggregate
ind.agg[i,,3] <- colSums(ind.cat * weights[,i,1] * weights[,i,2] * weights[,i,3])}
round(ind.agg[1,,3] - cons[1,], 5) # should be zero for 2nd constraint

# for multiple iterations
wf <- array(dim=c(dim(weights), num.its)) # array to store weight iterations
indf <- array(dim=c(dim(ind.agg), num.its))
wf[,,,1] <- weights 
indf[,,,1] <- ind.agg

############## The multiple iterations #############

# loop for multiple iterations (run e2.R repeatedly, saving each time)
for(it in 2:num.its){
source(file="data/simple/e2.R")
wf[,,,it] <- weights
indf[,,,it] <- ind.agg
}

############## The analysis part #############

a.v <- as.vector(as.matrix(cons)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,2]))
cor(a.v,g.v)

t1 <- data.frame(it = 1, corr = cor(a.v,g.v))
t1 <- t1[0,]
for(it in 1:num.its){
  for(con in 2:(num.cons + 1)){
    g.v <- as.vector(as.matrix(indf[,,con,it]))
    t1[nrow(t1)+1,] <- c(it+con/10,cor(a.v,g.v))
  }
}
t1
t1$numit <- 1:nrow(t1)

############## Plot the results #############

# plot the increasing fit, one interation to the next 
barplot(height=t1[,2], names.arg=t1[,1], ylim=c(t1[1,2],1), ylab=("Correlation (r)"))