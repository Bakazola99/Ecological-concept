
##### To update R using R-studio
library(installr)

##### Libraries
library(cluster)
library(labdsv)
library(MASS)
library(MVA)
library(optpart)
library(picante)
library(stats)
library(vegan)

getwd()
setwd("C:\\Users\\dell\\OneDrive\\Documents\\R\\Machine Learning with R")

#### Load the data
comm <- read.csv("grassland.csv", header = T, row.names = 1)

#### Using Bray-Curtis Dissimilarity
comm.bc.dist <- vegdist(comm, method = "bray")

##### Plot the dendogram using the recommended average linkage
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis Dissimilarity", hang = -1)
comm.bc.clust.cut <- cutree(comm.bc.clust, k=2)
rect.hclust(comm.bc.clust, k=2, border =2)
plot(comm.bc.clust, labels = as.character(comm.bc.clust.cut), hang = -1)


gb.biol<- read.csv("GBbiol.csv", row.names = 1)

#Create a matrix of similarity values:
gb.beta <- betadiver(gb.biol, method = "w")

#Create a dendrogram via hierarchical clustering and plot it:
gb.clus <- hclust(gb.beta)
plot(gb.clus)

#If you want a little less compressed dendrogram:
plot(gb.clus, hang = -2)
