install.packages("factoextra")
library(factoextra)
#loading sales product dataset
data = read.csv(choose.files())
str(data)

#first convert it to as.factor then as.numeric

data$Product_Description = as.numeric(data$Product_Description)
data$Product_Category = as.numeric(data$Product_Category)
data$Product_Line = as.numeric(data$Product_Line)
data$Raw_Material = as.numeric(data$Raw_Material)
data$Region = as.numeric(data$Region)
str(data)
df = scale(data)
head(df, nrow = 6)

# Compute the dissimilarity matrix
# df = the standardized data
res.dist <- dist(df, method = "euclidean")
#euclidean dist for scale dataset gives more accurte result
as.matrix(res.dist)[1:6, 1:6]

kmeans(df, centers=2, iter.max = 10, nstart = 25) #getting 17%
kmeans(df, centers=3, iter.max = 10, nstart = 25) #getting 29.3%
kmeans(df, centers=4, iter.max = 10, nstart = 25) #39%
kmeans(df, centers=5, iter.max = 10, nstart = 25) #45%

#show number of optimum cluster
fviz_nbclust(df, kmeans, method = "wss")+ geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res <- kmeans(df, 4, nstart = 25) #We are getting 39.2%
# Print the results 
print(km.res)

#To find mean of the each variable
aggregate(df, by=list(cluster=km.res$cluster), mean)
dd <- cbind(df, cluster = km.res$cluster) 
head(dd)
table(km.res$cluster)

km.res$size


#Hierarchical clustering
res.hc <- hclust(d = res.dist, method = "ward.D2")
print(res.hc)
#cex: label size
fviz_dend(res.hc, cex = 0.5)

fviz_cluster(km.res, data = df, palette = c
             ("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid",  star.plot = TRUE,  
             repel = TRUE,  ggtheme = theme_minimal() )

fviz_cluster(km.res, data = df, palette = c("set2"), 
             ellipse.type = "euclid",  star.plot = TRUE,  
             repel = TRUE,  ggtheme = theme_minimal())      

fviz_dend(km.res, cex = 0.5)
