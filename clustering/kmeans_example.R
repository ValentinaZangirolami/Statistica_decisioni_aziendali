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
# install.packages("fpc") 

#caricamento dati 
airbnb_data <- read.csv("data/listings_istanbul.csv")
airbnb_data <- airbnb_data[c(1:1000),]

nrow(airbnb_data)
head(airbnb_data, n = 10)
summary(airbnb_data)

#analizziamo le relazioni tra le variabili

airbnb_data |>
  select(price, minimum_nights, number_of_reviews, calculated_host_listings_count, availability_365) |>
  cor(use = "pairwise.complete.obs") |>
  round(2)

ggplot(airbnb_data, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")

# normalizzazione

airbnb_data = scale(airbnb_data[, c("price", "number_of_reviews", "minimum_nights", "calculated_host_listings_count", "availability_365")])

# kmeans

set.seed(123)
km.out <- kmeans(airbnb_data, centers = 3, nstart = 20)
km.out

# solo 34.8% della totale varianza è spiegata dal clustering --> non è un buon risultato

#proviamo con diversi valori di cluster

n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters-1)
silhouette_avg <- numeric(n_clusters-1)
calinski <- numeric(n_clusters-1)

set.seed(123)

# Look over 2 to n possible clusters
for (i in c(2:n_clusters)) {
  # Fit the model: km.out
  km.out <- kmeans(airbnb_data, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i-1] <- km.out$tot.withinss
  # Compute silhouette scores
  sil <- silhouette(km.out$cluster, dist(airbnb_data))
  silhouette_avg[i-1] <- mean(sil[, 3])
  # Calinski-Harabasz Index
  calinski[i-1] <- fpc::calinhara(airbnb_data, km.out$cluster)
}

results <- tibble(
  clusters = 2:n_clusters,
  wss = wss,
  silhouette = silhouette_avg,
  calinski = calinski
)

wss_plot <- ggplot(results, 
                   aes(x = clusters, y = wss)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Scree Plot (Elbow Method)", 
       x = "Number of Clusters (k)", 
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  scale_x_continuous(breaks = 1:n_clusters) +
  theme_minimal()

print(wss_plot)

sil_plot <- ggplot(results, aes(x = clusters, y = silhouette)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Average Silhouette Width",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  scale_x_continuous(breaks = 1:n_clusters) +
  theme_minimal()

print(sil_plot)

ggplot(results, aes(x = clusters, y = calinski)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Calinski-Harabasz Index",
       x = "Number of Clusters (k)",
       y = "CH Index") +
  scale_x_continuous(breaks = 1:n_clusters) +
  theme_minimal()


#visualizzazione del cluster 

set.seed(123)
km.out <- kmeans(airbnb_data, centers = 6, nstart = 20)
km.out

fviz_cluster(km.out, airbnb_data, repel = TRUE)
