##################################################################
#  Name:   Network Visualization using R.R                       #
#  Date:   November 7, 2020                                      #
#  Author: Bomi Lee                                              #
#  Purpose: Create plots of network data.                        #
#  Thanks to Elizabeth Menninga for some of this code.           #      
##################################################################

# Install and load needed packages

#install.packages("statnet")
#install.packages("igraph")
#install.packages("rio")
library(statnet)
library(rio)

# Set Working Directory
getwd()
setwd("C:/Users/bomim/Documents/Rworkshop/Networks")

# Load and plot data
## statnet
edge <- import("edgeList.csv")
edge
class(edge)

?network
net<-network(edge, matrix.type="edgelist")
#matrix.type="adjacency"
#directed=F

net
class(net)
summary(net)
net %v% "vertex.names"

windows()
plot(net, 
     displaylabels=T)

### Weights
edge
netweighted<-network(edge, 
                     matrix.type="edgelist",
                     ignore.eval=F,
                     names.eval="weight")

#ignore.eval: logical; ignore edge values?
#names.eval: optionally, the name of the attribute in which edge values should be stored

windows()
plot(netweighted,
     displaylabels=T,
     edge.lwd=4*netweighted%e%"weight")

netweighted[,] #adjacency matrix without weight
as.sociomatrix.sna(netweighted,"weight")
#as.sociomatrix: Coerce One or More Networks to Sociomatrix Form

netweighted
netweighted %v% "vertex.names"
netweighted %e% "weight"

### Add node attributes
netweighted %v% "gender"<-c("M","F","F","M","M","M")
netweighted %v% "gender"

netweighted %v% "age" <- 1:4
netweighted %v% "age" #what happened?

### Change the color of vertices
netweighted %v% "color" <- ifelse(netweighted %v% "gender"=="M", "gray", "yellow")
netweighted %v% "color"

netweighted

windows()
plot(netweighted,
     displaylabels=F,
     edge.lwd=2*netweighted%e%"weight",
     vertex.cex=2*netweighted%v%"age",
     vertex.col=netweighted%v%"color")


## igraph
detach(package:statnet)
library(igraph)

node <- import("nodeList.csv")

?graph_from_data_frame
net_igraph <- graph_from_data_frame(d=edge, v=node, directed=T)
# build an igraph object from the above matrix
class(net_igraph)

V(net_igraph)$gender <- c("M","F","F","M","M","M")
V(net_igraph)$gender

V(net_igraph)$color <- ifelse(V(net_igraph)$gender=="M", "black", "yellow")
V(net_igraph)$color

V(net_igraph)$age <- 1:6
V(net_igraph)$age

E(net_igraph)$Weight

net_igraph

windows()
plot(net_igraph,
     edge.width=2*E(net_igraph)$Weight,
     edge.arrow.size=.6,
     vertex.size=2*V(net_igraph)$age, 
     vertex.frame.color="gray", 
     vertex.color=V(net_igraph)$color,
     vertex.label.cex=1.5,
     vertex.label.dist=1.5,
     vertex.label.color="black")

## Real data: alliance
atop <- import("atop_sample.csv")

head(atop)
atop1997_dat <-subset(atop, year==1997, c(stateabb1, stateabb2))
atop1997 <- graph.data.frame(atop1997_dat)
atop1997
# no isolates
?as.undirected
atop1997_und <- as.undirected(atop1997, mode='collapse')

atop1997_und

windows()
plot(atop1997_und,
     vertex.label=NA,
     edge.width=1.5, 
     vertex.size=3.5, 
     vertex.frame.color="black", 
     vertex.color="white",
     layout=layout.fruchterman.reingold)

windows()
plot(atop1997_und,
     edge.width=1.5, 
     vertex.size=3.5, 
     vertex.frame.color="black", 
     vertex.color="white",
     layout=layout.fruchterman.reingold)

## Create sub-networks
E(atop1997_und)
V(atop1997_und)

net1997_usa <- atop1997_und - c("CAN", "FRN", "AZE")
sub1997<-subcomponent(net1997_usa, "USA",mode="all")
sub1997

sub1997_usa<-induced.subgraph(net1997_usa, vids=sub1997)
E(sub1997_usa)
V(sub1997_usa)

windows()
plot(sub1997_usa,
     edge.color="gray",
     edge.width=2,
     vertex.size=4, 
     vertex.frame.color="gray", 
     vertex.color="white",
     vertex.label=V(sub1997_usa)$stateabb,
     vertex.label.cex=0.6, 
     vertex.label.color="black",
     vertex.label.dist=1,
     layout=layout_with_kk)

### layout: https://igraph.org/r/doc/layout_.html

### https://igraph.org/r/doc/
### https://github.com/statnet/Workshops/wiki
### https://kateto.net/