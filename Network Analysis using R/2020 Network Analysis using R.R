##################################################################
#  Name:   Network Analysis using R.R                            #
#  Date:   December 1, 2020                                      #
#  Author: Bomi Lee                                              #
#  Purpose: Calculate statistics of network data.                #
#  Thanks to Elizabeth Menninga for some of this code.           #
##################################################################

# Install and load needed packages

#install.packages("igraph")
#install.packages("statnet")

library(igraph)
library(rio)

# Set Working Directory
getwd()
setwd("C:/Users/bomim/Documents/Network-Analysis-Workshops-2020/Network Analysis using R")

# Load and Plot Data

node <- import("nodeList.csv")
edge <- import("edgeList.csv")

net_igraph<-graph_from_data_frame(d=edge, v=node, directed=T)
net_igraph # Directed, unweighted

windows()
plot(net_igraph,
     edge.arrow.size=.6,
     vertex.size=5, 
     vertex.frame.color="gray", 
     vertex.label.cex=1.2,
     vertex.label.dist=1,
     vertex.label.color="black")

## Real Data: Defensive Alliance (ATOP)
atop <- import("atop_sample2.csv")

head(atop)
atop1997_dat <-subset(atop, year==1997, c(stateabb1, stateabb2))
atop1997 <- graph.data.frame(atop1997_dat)

atop1997und <- as.undirected(atop1997, mode='collapse')  # Undirected, unweighted

windows()
plot(atop1997und,
     vertex.label=NA,
     edge.width=1.5, 
     vertex.size=2.5, 
     vertex.frame.color="black", 
     vertex.color="blue",
     layout=layout_with_kk)

windows()
plot(atop1997und,
     #vertex.label=NA,
     vertex.label.cex=0.6, 
     vertex.label.color="black",
     vertex.label.dist=1,
     edge.width=1.5, 
     vertex.size=2.5, 
     vertex.frame.color="black", 
     vertex.color="blue",
     layout=layout_with_kk)

# Calculate Network Statistics

## Centrality
### Degree - Number of adjacent ties for a node
?degree

degree(net_igraph,
       mode = "in")

degree(net_igraph,
       mode = "out")

degree(net_igraph,
       mode = "total")

degree(atop1997und)


### Eigenvector (undirected)
eigen_centrality(atop1997und)$vector

### Betweenness (directed or undirected)
betweenness(net_igraph)
betweenness(atop1997und)

### Closeness (directed or undirected)
closeness(net_igraph)
closeness(atop1997und)


# ERGM using Statnet
detach(package:igraph)
library(statnet)

## Undirected Networks: flomarriage data
data(florentine) 
flomarriage # Let's look at the flomarriage data
?flomarriage
windows()
plot(flomarriage, displaylabels = TRUE) 

### Fit model
flomodel_1 <- ergm(flomarriage ~ edges) 
flomodel_1

summary(flomodel_1) 

### Interpretation?

### Let's add a term often thought to be a measure of "clustering" -- the number of completed triangles
set.seed(1664) #Seeding the RNG for demo purposes; otherwise, your output will vary
flomodel_2 <- ergm(flomarriage ~ edges + triangle)

summary(flomodel_2)

flomodel_2$coef 
flomodel_2$formula 

### Interpretation
coef1 = flomodel_2$coef[1]
coef2 = flomodel_2$coef[2]
logodds = coef1 + c(0,1,2) * coef2
expit = function(x) 1/(1+exp(-x))
ps = expit(logodds)
ps = round(ps, 3)
ps

### Check MCMC chains
pdf("diagnostics.pdf")
mcmc.diagnostics(flomodel_2)
dev.off()

### Check model fit
pdf("gof_2.pdf")
gof_2 = gof(flomodel_2)
plot(gof_2)
dev.off()
gof_2

### Including node attributes in the model
wealth <- flomarriage %v% 'wealth'
wealth 
windows()
plot(flomarriage, vertex.cex = wealth/20, usearrows = FALSE, displaylabels = TRUE) 

### Model including wealth:
flomodel_3 <- ergm(flomarriage ~ edges + nodecov('wealth'))
summary(flomodel_3)

### Can include both triangles and wealth

flomodel_4 <- ergm(flomarriage ~ edges + triangle + nodecov("wealth"))
summary(flomodel_4)

### http://statnet.org/Workshops/ergm_tutorial.html#the_statnet_project
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2443947/