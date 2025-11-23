# Hierarchical clustering
library(factoextra)
library(cluster)

# --- Create the dataset ---
car_model <- c("perodua_myvi", "perodua_bezza", "proton_saga",
               "perodua_axia", "honda_city", "toyota_vios")

fuel_efficiency <- c(21.1, 22.0, 14.9, 27.4, 18.5, 19.2)
horsepower <- c(102, 94, 95, 67, 119, 106)

mtcars <- data.frame(car_model, fuel_efficiency, horsepower)
mtcars

# Use only numeric variables
mtcars_num <- mtcars[, c("fuel_efficiency", "horsepower")]


#Find distance
dist_matrix <- dist(mtcars_num)
dist_matrix

#Perform Hierarchical clustering
hcl <- hclust(dist_matrix, method = "single")

#Plot dendogram
plot(hcl,
     labels = mtcars$car_model,
     main = "Hierarchical Clustering (Single Linkage)",
     xlab = "",
     sub = "",
     cex = 0.9)

#Create a cluster
clusters <- cutree(hcl, k = 2)
mtcars$cluster <- as.factor(clusters)
mtcars

#Create a heatmap
heatmap(as.matrix(mtcars_num),
        Rowv = as.dendrogram(hcl),
        Colv = NA,
        scale = "none",
        labRow = mtcars$car_model,
        cexRow = 0.6,
        cexCol = 0.8,
        col = colorRampPalette(c("blue", "white", "red"))(50),
        main = "Heatmap of Cars")

#Plot cluster
fviz_cluster(
  list(data = mtcars_num, cluster = clusters),
  geom = "point",
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(),
  labelsize = 10,
  main = "Cluster Plot of Cars"
)
