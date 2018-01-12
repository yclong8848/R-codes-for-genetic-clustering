

data <- read.csv(file="VariationRateMatrix.csv", header = T)
View(data)
L = dim(data)[1] # 46
M = dim(data)[2] # 26
vect <- t(as.matrix(data[1:L, 2:M]))
View(vect)
dim(vect) # 25 46

par(mfrow=c(4, 2))



###################### Euclidean metricDistance ######################
DistM = as.matrix(dist(vect, method = "euclidean")) 
fit <- cmdscale(DistM, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-0.03, 0.03), ylim=c(-0.01, 0.01), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "Euclidean metric (Minkowski metric with p = 2)")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


###################### Minkowski metricDistance p = 1.25 ############# 
DistM = as.matrix(dist(vect, method = "minkowski", p = 1.25)) 
fit <- cmdscale(DistM, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-0.075, 0.075), ylim=c(-0.025, 0.025), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "Minkowski metric with p = 1.25")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


###################### manhattan Distance ######################
DistM = as.matrix(dist(vect, method = "manhattan")) 
fit <- cmdscale(DistM, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-0.2, 0.2), ylim=c(-0.05, 0.05), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "Manhattan metric (Minkowski metric with p = 1)")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


###################### Minkowski metricDistance p = 1.5 ############# 
DistM = as.matrix(dist(vect, method = "minkowski", p = 1.5)) 
fit <- cmdscale(DistM, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-0.05, 0.05), ylim=c(-0.02, 0.02), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "Minkowski metric with p = 1.5")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


###################### canberra Distance ######################
DistM = as.matrix(dist(vect, method = "canberra")) 
fit <- cmdscale(DistM, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-15, 20), ylim=c(-7, 10), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "Canberra metric")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


###################### Minkowski metricDistance p = 1.75 ############# 
DistM = as.matrix(dist(vect, method = "minkowski", p = 1.75)) 
fit <- cmdscale(DistM, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-0.03, 0.03), ylim=c(-0.01, 0.01), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "Minkowski metric with p = 1.75")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


###################### Chebyshev Distance   ##################### 
DistM = as.matrix(dist(vect, method = "maximum")) 
fit <- cmdscale(DistM, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-0.01, 0.01), ylim=c(-0.01, 0.01), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "Chebyshev metric")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")



######################    NTV Distance    ######################

NTV <- function(x, y) {
  xy <- c()
  for (j in 1:length(x)){
    xy[j] <- max(x[j], y[j])
  }
  sum(abs(x-y))/sum(xy)
}

DistM2 = matrix(nrow=dim(vect)[1], ncol=dim(vect)[1]) 
for (i in 1:dim(vect)[1]){
  for (j in 1:dim(vect)[1]){
    DistM2[i, j] = NTV(vect[i,], vect[j,])
  }
}

fit <- cmdscale(DistM2, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x[1:10], y[1:10], xlim=c(-0.35, 0.5), ylim=c(-0.2, 0.4), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28, main = "NTV metric")
points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


# ######################   Cosine Distance  ######################
# library("lsa")
# DistM = as.matrix(1- cosine(vect))
# fit <- cmdscale(DistM, eig = TRUE, k = 2)
# x <- fit$points[, 1]
# y <- fit$points[, 2]
# plot(x[1:10], y[1:10], xlim=c(-0.1, 0.15), ylim=c(-0.1, 0.05), pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 28)
# points(x[11:15], y[11:15], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = 34)
# points(x[16:20], y[16:20], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "cyan4")
# points(x[21:25], y[21:25], pch = 19, cex = 1.5, xlab="MDS Coordinate 1", ylab="MDS Coordinate 2", col = "coral4")


###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
#####################   Clustering Tree   #####################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################




library(dendextend)
groupCodes <- c(rep("MAD",10), rep("MAC",5), rep("AUD",5), rep("AUC",5))
colorCodes <- c(MAD="blue", MAC="red", AUD="cyan4", AUC="coral4")

par(mfrow=c(4, 2))




###################### Euclidean metricDistance ######################
DistM = dist(vect, method = "euclidean")
hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend, xlab="", sub = "", ylab="Distance", main = "Euclidean metric (Minkowski metric with p = 2)")


###################### Minkowski metricDistance p = 1.25 ############# 
DistM = dist(vect, method = "minkowski", p = 1.25)
hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend, xlab="", sub = "", ylab="Distance", main = "Minkowski metric with p = 1.25")


###################### manhattan Distance ######################
DistM = dist(vect, method = "manhattan")
hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend, xlab="", sub = "", ylab="Distance", main = "Manhattan metric (Minkowski metric with p = 1)")


###################### Minkowski metricDistance p = 1.5 ############# 
DistM = dist(vect, method = "minkowski", p = 1.5) 
hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend, xlab="", sub = "", ylab="Distance", main = "Minkowski metric with p = 1.5")


###################### canberra Distance ######################
DistM = dist(vect, method = "canberra") 
hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend, xlab="", sub = "", ylab="Distance", main = "Canberra metric")


###################### Minkowski metricDistance p = 1.75 ############# 
DistM = dist(vect, method = "minkowski", p = 1.75) 
hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend, xlab="", sub = "", ylab="Distance", main = "Minkowski metric with p = 1.75")


###################### Chebyshev Distance   ##################### 
DistM = dist(vect, method = "maximum") 
hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend, xlab="", sub = "", ylab="Distance", main = "Chebyshev metric")



######################    NTV Distance    ######################
NTV <- function(x, y) {
  xy <- c()
  for (j in 1:length(x)){
    xy[j] <- max(x[j], y[j])
  }
  sum(abs(x-y))/sum(xy)
}
DistM2 = matrix(nrow=dim(vect)[1], ncol=dim(vect)[1]) 
labels <- colnames(data)[2:26]
rownames(DistM2)<- labels
colnames(DistM2)<- labels


for (i in 1:dim(vect)[1]){
  for (j in 1:dim(vect)[1]){
    DistM2[i, j] = NTV(vect[i,], vect[j,])
  }
}
DistM = as.dist(DistM2)

hc <- hclust(DistM, method = "ward.D2")
dend <- as.dendrogram(hc)
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
lab = read.csv(file="VariationRateMatrix.csv", header = F)[1, 2:26]
plot(dend, xlab="", sub = "", ylab="Distance", main = "NTV metric")




################################################################
################################################################
################################################################
################################################################
#######################   AU/BP   ##############################
################################################################
################################################################
################################################################
################################################################



data <- read.csv(file="VariationRateMatrix2.csv", header = T)
View(data)
L = dim(data)[1] # 46
M = dim(data)[2] # 26
vect <- t(as.matrix(data[1:L, 2:M]))
View(vect)
dim(vect) # 25 46


library(pvclust)

re <- pvclust(t(vect), method.hclust="ward.D2",
        method.dist="canberra", use.cor="pairwise.complete.obs",
        nboot=5000, parallel=FALSE, r=seq(.5,1.4,by=.1),
        store=FALSE, weight=FALSE, iseed=NULL, quiet=FALSE)


plot(re, xlab="", sub = "", ylab="Canberra Distance", main = "Cluster tree with au/bp values (%) on Ward's method")
 










