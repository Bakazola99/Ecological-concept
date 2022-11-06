#### Installing some packages

install.packages("BiodiversityR") 
install.packages("labdsv") #### good packge for multivariate analysis
install.packages("plyr")
library(cluster)
library(labdsv)
library(MASS)
library(MVA)
library(optpart)
library(picante) # For phylogenetic community analysis
library(stats)
library(vegan) ##currently the best all-round package for community ecology
library(BiodiversityR)
library(plyr) ## for manipulating datasheet

# We will be using a data called bryceveg.R it is a data from site x containing plant
# species abundance data from sites in Bryce Canyon National Park, Utah.

veg <- read.table("bryceveg.R", header = T)
View(veg)
names(veg)
row.names(veg)

bryce <- read.table("brycesite.R", header = T)

comm <- read.csv("grassland.csv", header = T, row.names = 1)
comm[1:5, 1:5]

## To find out how many species are present at each site, i.e species richness and Diversity index
species.richness<-specnumber(comm)
species.richness

richness <- apply(comm>0,1,sum)
richness

shannon <- diversity(comm,"shannon")
shannon

simpson <- diversity(comm, "simpson")
simpson

### Load datafile that contains the environmental data of the comm data
environ <- read.csv("plot.metadata.csv", header = T, row.names = 1)

# To know the distribution of the species in term of the environmental data
boxplot(species.richness~environ$habitat, ylab = "species abundance", xlab = "habitat")

# obs: it was observed that there is a difference in the species abundance between the two stations.
# Meanwhile there is still overlap. therefore, we can test the hypothesis if the difference is significant.
# First: let's test the normality and homogeneity

species.habitat <-data.frame(environ, species.richness)

attach(species.habitat)
fescue <-species.habitat[habitat=="Fescue","species.richness"]
mixedgrass <-species.habitat[habitat=="Mixedgrass","species.richness"]

#shapiro test for normality
shapiro.test(fescue)
shapiro.test(mixedgrass) ## data is normally distributed

## Homogeneity
var.test(x= fescue, y=mixedgrass) # equal variance

t.test(x=fescue, y=mixedgrass, var.equal = T, alternative = "two.sided") # there difference is statistically significant.
# another way to do it 
t.test(species.richness~environ$habitat) ## significant difference i.e p<0.05.

### dissimilarity matrix
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.dist

### To explain the species dissimilarity among the site by environmental variables
adonis2(comm.bc.dist~habitat, data = environ) # there is significant differnce among the dissimilar site in terms of habitat.

## Note, since wew have multiple sites, several species and several environmental variables, we need to perform an ordination analysis, 
## which is the analysis to simplifies the multivariate data into two dimensions. The common one is NMDS- non-metric multidimensional scaling.
comm.bc.mds <- metaMDS(comm, distance = "bray")

## plot the result
mds.fig <- ordiplot(comm.bc.mds, type = "none")
points(mds.fig, "sites", pch = 19, col="green", select = environ$habitat=="Fescue")
points(mds.fig, "sites", pch = 19, col="blue", select = environ$habitat=="Mixedgrass")
ordiellipse(comm.bc.mds, environ$habitat, conf = 0.95, label = T)

### To plot the relationship of species in terms of sites and environmental variables
ordiplot(comm.bc.mds, display = "sites", type = "points")
ordiplot(comm.bc.mds, display = "species", type = "points")
ordiplot(comm.bc.mds)
plot(envfit(comm.bc.mds, environ[,3:6]))
legend("topleft", c("sites", "species"),col=c('black', 'red'), pch=c(1,3))


