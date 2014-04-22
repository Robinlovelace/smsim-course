# script to categorise the 'simple' individual-level dataset

# create the category array
ind.cat <- array(0,dim=c(nrow(ind), ncol(cons)))

# re-categorise the variables
ind.cat[which(ind$age < 50),1] <- 1 # Age, "< 50"
ind.cat[which(ind$age >= 50),2] <- 1 # "50+"
ind.cat[which(ind$sex =="m"),3] <- 1 # Sex constraint: "m" 
ind.cat[which(ind$sex =="f"),4] <- 1 #"f"
sum(ind.cat) # Should be 10 - n. individuals

# Polishing up
ind.cat <- data.frame(ind.cat)
names(ind.cat) <- names(cons)
