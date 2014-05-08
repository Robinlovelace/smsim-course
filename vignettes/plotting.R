# analysis of cakemap data:
library(reshape2)
ind.agg.melted <- melt(ind.agg[,,1])
head(ind.agg.melted)
cons.melted <- melt(cons)
head(cons.melted)
class(ind.agg.melted[,2])
plot(cons.melted[,2], ind.agg.melted[,3])
