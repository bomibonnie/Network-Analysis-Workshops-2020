##################################################################
#  Name:   Network Analysis using R.R                            #
#  Date:   October 23, 2019                                      #
#  Author: Bomi Lee                                              #
#  Purpose: Create plots and and calculate statistics            #
#           of network data.                                     #
#  Thanks to Elizabeth Menninga for some of this code.           #         #
##################################################################

# Install and load needed packages

#install.packages("statnet")
#install.packages("igraph")
library(statnet)
library(rio)

# Set Working Directory
getwd()
#setwd("C:/Users/bomim/Documents/Rworkshop/Networks")

# Load and plot data
## statnet
edge <- import("edgeList.csv")
head(edge)
View(edge)

?network
net<-network(edge, matrix.type="edgelist")
#matrix.type="adjacency"
#directed=T

net
class(net)

windows()
plot(net, displaylabels=T)

### Weights
netweighted<-network(edge, 
                     matrix.type="edgelist",
                     ignore.eval=F,
                     names.eval="weight")

#ignore.eval: logical; ignore edge values?
#names.eval: optionally, the name of the attribute in which edge values should be stored
  
windows()
plot(netweighted,
     displaylabels=T,
     edge.lwd=5*netweighted%e%"weight")

netweighted[,] #adjacency matrix without weight
as.sociomatrix.sna(netweighted,"weight")
#as.sociomatrix: Coerce One or More Networks to Sociomatrix Form

netweighted
netweighted %e% "weight"

### Attribute data
netweighted %v% "gender"<-c("M","F","F","M","M","M")
netweighted %v% "gender"

netweighted %v% "vertex.names"

netweighted %v% "age" <- 1:4
netweighted %v% "age" #what happened?

netweighted

## igraph
library(igraph)

node <- import("nodeList.csv")
net_igraph<-graph_from_data_frame(d=edge, v=node, directed=T)
# build an igraph object from the above matrix
class(net_igraph)

V(net_igraph)$gender <- c("M","F","F","M","M","M")
V(net_igraph)$gender

V(net_igraph)$age <- 1:6
V(net_igraph)$age

E(net_igraph)$Weight

net_igraph

windows()
plot(net_igraph,
     edge.width=2*E(net_igraph)$Weight,
     edge.arrow.size=.6,
     vertex.size=5, 
     vertex.frame.color="black", 
     vertex.color="purple",
     vertex.label.cex=1.5,
     vertex.label.dist=1,
     layout=layout.fruchterman.reingold)

## Real data
atop <- import("atop_sample.csv")

head(atop)
atop2002_dat <-subset(atop, year==2002, c(stateabb1, stateabb2))
atop2002 <- graph.data.frame(atop2002_dat)
# no isolates
atop2002g <- as.undirected(atop2002, mode='collapse')

windows()
plot(atop2002g,
     vertex.label=NA,
     edge.width=1.5, vertex.size=3.5, 
     vertex.frame.color="black", 
     vertex.color="blue",
     layout=layout.fruchterman.reingold)

plot(atop2002g,
     vertex.label=NA,
     edge.width=1.5, vertex.size=3.5, 
     vertex.frame.color="black", 
     vertex.color="blue",
     layout=layout.kamada.kawai)

### https://igraph.org/r/doc/


# Calculate Network Statistics

## Centrality
### Degree - Number of adjacent ties for a node
?degree

degree(atop2002g)
which.max(degree(atop2002g))
degree(atop2002g)[113]

degree(net_igraph,
       mode = "in")

degree(net_igraph,
       mode = "out")

degree(net_igraph,
       mode = "total")

### Eigenvector
#eigen_centrality(net_igraph)
eigen_centrality(atop2002g)$vector

### Betweenness
betweenness(net_igraph)
#betweenness(atop2002g)

### Closeness
closeness(net_igraph)
#closeness(atop2002g)

## Into a Dataset
degree <- degree(atop2002g)
eigen <- eigen_centrality(atop2002g)$vector

cent_mat <- cbind(degree, eigen)
cent_df <- data.frame(cent_mat)
cent_df$state <- rownames(cent_df)

View(cent_df)

## Dyad
windows()
plot(net_igraph)
summary(net_igraph)

?dyad.census
#mut: The number of pairs with mutual connections.
#asym: The number of pairs with non-mutual connections.
#null: The number of pairs with no connection between them.

dyad.census(net_igraph)

## Triads
?triad.census
triad.census(net_igraph)

### Using statnet
summary(net ~ edges)
atopnet<-network(atop2002_dat, matrix.type="edgelist")
summary(atopnet ~ triangles)