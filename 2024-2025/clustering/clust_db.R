################################################################################
#                                 Clustering                ,                  #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################

#caricamento libraries

library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(fpc)
library(dbscan)

#caricamento dati 

data(iris)
head(iris)

# Prepare the data by removing the species column
iris_data <- iris[, -5]

summary(iris_data)

GGally::ggcorr(
  data = iris_data,
  low = "red3",
  high = "blue3",
  mid = "white",
  label = T,
  label_round = 2
)

#visualizzazione dati 

pr.out <- prcomp(iris_data[,1:4], scale = TRUE)
par(mfrow = c(1, 1))
plot(pr.out$x[, 1:2], col = iris$Species, pch = 19,
     xlab = "Z1", ylab = "Z2")

# cluster gerarchico

#distanza euclidea

hc.complete <- hclust(dist(iris_data), method="complete")

hc.average <- hclust(dist(iris_data), method="average")

hc.single <- hclust(dist(iris_data), method="single")

par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage",
       xlab = "", sub = "", cex = .9)
plot(hc.average , main = "Average Linkage",
       xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage",
       xlab = "", sub = "", cex = .9)

#tagliare dendogramma
hc.clusters.com <- cutree(hc.complete, 2)
hc.clusters.average <- cutree(hc.average, 2)
hc.clusters.single <- cutree(hc.single, 2)

table(hc.clusters.com , iris$Species)
table(hc.clusters.average , iris$Species)
table(hc.clusters.single , iris$Species)

# dbscan

db <- dbscan(iris_data, eps = 0.5, minPts = 8)
print(db)
# minPts è generalmente fissato come 2*numero features
# eps raggio di un cerchio che verrà utilizzato per trovare i vicini da un punto

# Visualize the clusters
iris_data$cluster <- factor(db$cluster)
ggplot(iris_data, aes(x = Sepal.Length, y = Sepal.Width, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "DBSCAN Clustering of Iris Data") +
  theme_minimal()

# Identify outliers
outliers <- iris_data[db$cluster == -1, ]
print(outliers)

# proviamo a vedere cosa accade con diversi valori di epsilon

# esercizio: proviamo ad applicare k-means

# esercizio 2: proviamo diversi metodi di clusters su dati genomici

library(ISLR2)

nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)
table(nci.labs)