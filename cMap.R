############################################
#### From the IPF-performance-testing github repo  
#### https://github.com/Robinlovelace/IPF-performance-testing
############################################

num.its <- 2 # how many iterations will we run?

# Loading the data: Ensure R is in the right working directory 
ind <- read.csv("data/cakeMap/ind.csv")
cons <- read.csv("data/cakeMap/cons.csv")

# load constraints separately - normally this would be first stage
con1 <- cons[1:12]
con2 <- cons[13:14]
con3 <- cons[15:24]

num.cons <- 3  # calculate n. constraints (can set manually)
category.labels <- names(cons) # should be correct from cons.R

# set-up aggregate values - column for each category
source("data/cakeMap/categorise.R") # this script must be customised to input data

# check constraint totals - should be true
sum(ind.cat[,1:ncol(con1)]) == nrow(ind) # is the number in each category correct?
sum(ind.cat[,ncol(con1)+1:ncol(con2)]) == nrow(ind) 

# create weights in 3D matrix (individuals, areas, iteration)
weights <- array(dim=c(nrow(ind),nrow(cons),num.cons+1)) 
weights[,,num.cons+1][] <- 1 # sets initial weights to 1
ini.ws <- weights[,,num.cons+1]

# convert survey data into aggregates to compare with census (3D matix)
ind.agg <- array(dim=c(nrow(cons),ncol(cons),num.cons+1))
for (i in 1:nrow(cons)){
  ind.agg[i,,1]   <- colSums(ind.cat) * weights[1,i,num.cons+1]}
ind.agg[1:5,1:10,1] # look at what we've created - n. individuals replicated throughout

############## The IPF part #############

# Re-weighting for constraint 1 via IPF 
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con1)){
 weights[which(ind.cat[,i] == 1),j,1] <- con1[j,i] /ind.agg[j,i,1]}}
for (i in 1:nrow(cons)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1])}
# test results for first row (not necessary for model)
ind.agg[1,1:2,2] - cons[1,1:2] # should be zero

# second constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con2) + ncol(con1)){
  weights[which(ind.cat[,i] == 1),j,2] <- cons[j,i] /ind.agg[j,i,2]}}  
for (i in 1:nrow(cons)){ # convert con2 back into aggregate
ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1] * weights[,i,2])}
ind.agg[1,,3] - cons[1,]

# third constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con3) + ncol(con1) + ncol(con2)){
    weights[which(ind.cat[,i] == 1),j,3] <- cons[j,i] /ind.agg[j,i,3]}}
for (i in 1:nrow(cons)){ # convert con3 back into aggregate
  ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,num.cons+1] * weights[,i,1] * weights[,i,2] * 
                               weights[,i,3])}
# test the result
ind.agg[1:3,,4] - cons[1:3,]

# for multiple iterations
wf <- array(dim=c(dim(weights), num.its, 1)) # array to store weights its, wei
indf <- array(dim=c(dim(ind.agg), num.its, 1))
wf[,,,1,1] <- weights 
indf[,,,1,1] <- ind.agg

############## The multiple iterations #############

# loop for multiple iterations (run e2.R repeatedly, saving each time)
for(it in 2:num.its){
source(file="data/cakeMap/e2.R")
wf[,,,it,1] <- weights
indf[,,,it,1] <- ind.agg
}

############## The analysis part #############
a.v <- as.vector(as.matrix(cons)) # constraints in long form, for cor
g.v <- as.vector(as.matrix(indf[,,1,2,1]))
cor(a.v,g.v)

t1 <- data.frame(it = 1, corr = cor(a.v,g.v))
t1 <- t1[0,]
for(it in 1:num.its){
  for(con in 2:(num.cons+1)){
    g.v <- as.vector(as.matrix(indf[,,con,it,1]))
    t1[nrow(t1)+1,] <- c(it+con/10,cor(a.v,g.v))
  }
}
t1
t1$numit<-1:nrow(t1)

############## Plot the results #############

# plot the increasing fit, one interation to the next 
barplot(height=t1[,2], names.arg=t1[,1], ylim=c(t1[1,2],1), ylab=("Correlation (r)"))