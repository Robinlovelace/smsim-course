# analysis of cakemap microsim. results
# run after running cMap.R

# test the plotting works:
library(reshape2) # load this package for reformating the data

ind.agg.melted <- melt(indf[,,3,1,1]) # this refers to [area,category,constraint,iteration,dummy]
head(ind.agg.melted) # take a look at our melted dataset

cons.melted <- melt(cons)
head(cons.melted)
names(cons.melted) <- c("Category", "Census")

class(ind.agg.melted[,2])
plot(cons.melted[,2], ind.agg.melted[,3])

# create long constraint variable (used for graphing)
con.long.names <- c(rep("Age/Sex", ncol(con1) * nrow(cons)),
  rep("N.Cars", ncol(con2) * nrow(cons)),
  rep("Class", ncol(con3) * nrow(cons))
  )

# look at the graph with these categories added
library(ggplot2) # for nice plots
qplot(cons.melted[,2], ind.agg.melted[,3], color = con.long.names, alpha = 0.5) + theme_bw()

# create data for entire iteration
iam <- melt(indf[,,,1,1]) # provide shortened names of those above
head(iam)
names(iam) <- c("Zone", "Category", "Cons", "Simulated")
nrow(ind.agg.melted) / length(con.long.names) # how many constraints?

cm4 <- rbind(cons.melted, cons.melted)
cm4 <- rbind(cm4, cm4)

iam <- cbind(iam, cm4)

iam <- cbind(iam, data.frame(Constraint = rep(con.long.names, 4)))
head(iam) # check if our final data frame makes sense

qplot(data = iam, x = Census, y = Simulated, color = Constraint) + 
  theme_bw() + facet_wrap( ~ Cons , ncol = 2) # plot the figure for all constraints

ggsave("figures/ipf-graph.png")
