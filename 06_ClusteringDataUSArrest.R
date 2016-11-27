##################################################################################################
# Beisiel Clustering
# source: http://www.sthda.com/english/wiki/cluster-analysis-in-r-unsupervised-machine-learning
# demo data: pre-built
##################################################################################################

# Load data
data("USArrests")
my_data <- USArrests
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
my_data <- scale(my_data)
# View the firt 3 rows
head(my_data, n = 3)


# Install factoextra
#install.packages("factoextra")
# Install cluster package
#install.packages("cluster")
library("cluster")
library("factoextra")


#It’s simple to compute and visualize distance matrix using the functions get_dist() and fviz_dist() in factoextra R package:
#get_dist(): for computing a distance matrix between the rows of a data matrix. Compared to the standard dist() function, it supports correlation-based distance measures including “pearson”, “kendall” and “spearman” methods.
#fviz_dist(): for visualizing a distance matrix
res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res.dist,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#Bestimmung der optimalen Anzahl der Cluster

#Beispiel A
wss <- (nrow(my_data)-1)*sum(apply(my_data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(my_data, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Beispiel B
library("factoextra")
fviz_nbclust(my_data, kmeans, method = "gap_stat")


# Anwendung k-means Algorithmus anhand zuvor bestimmter Anzahl k (!zuvor ermittelt)
km.res <- kmeans(my_data, 4, nstart = 25)
# Visualisierung 
library("factoextra")
fviz_cluster(km.res, data = my_data, frame.type = "convex")+ theme_minimal()


# Alternativ zu k-means: PAM
# Compute PAM
library("cluster")
pam.res <- pam(my_data, 4)
# Visualize
fviz_cluster(pam.res)





