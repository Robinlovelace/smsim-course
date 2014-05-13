weights[,,1] <- weights[,,1] * weights[,,2] * weights[,,3] *  weights[,,4] 
ind.agg[,,1] <- ind.agg[,,4]

# re-weighting for constraint 1 via IPF 
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,2] <- con1[j,i] / ind.agg[j,i,1]}}
for (i in 1:nrow(cons)){ # convert con1 weights back into aggregates
  ind.agg[i,,2]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2])}
# test the result
ind.agg[1:3,1:15,2]
cons[1:3,1:15]

# second constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con2) + ncol(con1)){
    weights[which(ind.cat[,i] == 1),j,3] <- cons[j,i] / ind.agg[j,i,2]}}  
for (i in 1:nrow(cons)){ # convert con2 back into aggregate
  ind.agg[i,,3]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2] * weights[,i,3])}
ind.agg[1:3,1:15,3]
cons[1:3,1:15]
# third constraint
for (j in 1:nrow(cons)){
  for(i in 1:ncol(con3) + ncol(con1) + ncol(con2)){
    weights[which(ind.cat[,i] == 1),j,4] <- cons[j,i] / ind.agg[j,i,3]}}
for (i in 1:nrow(cons)){ # convert con3 back into aggregate
  ind.agg[i,,4]   <- colSums(ind.cat * weights[,i,1] * weights[,i,2] * weights[,i,3] * 
                               weights[,i,4])}
# test the result
ind.agg[1:3,,4]
cons[1:3,]
