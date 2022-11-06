                        ### Clustering Analysis ####
data <- read.csv("utilities.csv", header = T)
str(data)
data$Company <- as.factor(data$Company)

library(psych)
pairs.panels(data[2:9])

plot(Fuel_Cost~Sales, data=data, col="red")
with(data,text(data$Fuel_Cost~data$Sales, labels=data$Company, 
               pos=4, 
               cex=.7,
               col="green"))

nor <- data[,-c(1,1)]
means <- apply(nor, 2, mean)
std <- apply(nor, 2, sd)
normalised_data <- scale(nor,center = means, scale=std)

distance =dist(normalised_data)

hier <- hclust(distance, method = "complete")
plot(hier, labels = data$Company, main = "Hierarchical Clustering", hang = -1)
rect.hclust(hier, k=3, border =2)

member <- cutree(hier, 3)
member

aggregate(normalised_data, list(member), mean)
aggregate(data[,-c(1,1)], list(member), mean)

library(cluster)
plot(silhouette(cutree(hier, 3), distance))

set.seed(123)
kv <- kmeans(normalised_data,4)
clusplot(data,
         kv$cluster,
         color = T,
         shade = T,
         labels = 2,
         lines=0)
