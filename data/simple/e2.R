# e2 - for etsim iteration 2, and beyond
weights[,,1] <- weights[,,1] * weights[,,2] * weights[,,3] 
ind.agg[,,1] <- ind.agg[,,num.cons + 1]

# re-weighting for constraint 1 via IPF 
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,2] <- con1[j,i] / ind.agg[j,i,1]}}

# convert con1 weights back into aggregates
for (i in 1:nrow(cons)){
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2])}

# test results for first row - set values appropriate for analysis
ind.agg[1,1:ncol(con1),] - cons[1,]

# second constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con2) + ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,3] <- cons[j,i] /ind.agg[j,i,2]}}  

# convert con2 back into aggregate
for (i in 1:nrow(cons)){
  ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2] *
                               weights[,i,3])}
# test results for first row
ind.agg[5,ncol(con1)+1:ncol(con2),3] 
cons[5,ncol(con1)+1:ncol(con2)]