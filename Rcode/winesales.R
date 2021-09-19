library("readxl")
library("dplyr")
library("corrplot")
library("car")
library("Rtsne")
library("cluster")

setwd("/Users/harini-mac/Desktop/Northwestern University/MSDS-411/Week10/FinalProject/")
mydata <- read_excel("WineSales.xlsx")

# check the structure, dimensions
str(mydata)
head(mydata)
dim(mydata)

# remove any records with missing values
mydata <- na.omit(mydata)
dim(mydata)


#Perform EDA on the data 
wine.df <- mydata %>% select(-INDEX)
dim(wine.df)
summary(wine.df)
par(mfrow=c(2,2))
range(wine.df$FixedAcidity)
hist(wine.df$FixedAcidity, main="Histogram for Fixed Acidity",xlab="FixedAcidity", border="blue",col="green",xlim=c(-18,32.5),las=1,breaks=100)
boxplot(wine.df$FixedAcidity, main = "Box plot of Fixed Acidity", xlab = "FixedAcidity", ylab = "",col = "gold",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$VolatileAcidity)
hist(wine.df$VolatileAcidity, main="Histogram for Volatile Acidity",xlab="VolatileAcidity", border="blue",col="red",xlim=c(-2.75,3.68),las=1,breaks=100)
boxplot(wine.df$VolatileAcidity, main = "Box plot of Volatile Acidity", xlab = "VolatileAcidity", ylab = "",col = "green",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$CitricAcid)
hist(wine.df$CitricAcid, main="Histogram for Citric Acid",xlab="CitricAcid", border="blue",col="orange",xlim=c(-2.75,3.68),las=1,breaks=100)
boxplot(wine.df$CitricAcid, main = "Box plot of Citric Acid", xlab = "CitricAcid", ylab = "",col = "blue",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$ResidualSugar)
hist(wine.df$ResidualSugar, main="Histogram for Residual Sugar",xlab="ResidualSugar", border="blue",col="pink",xlim=c(-127.80,140.65),las=1,breaks=100)
boxplot(wine.df$ResidualSugar, main = "Box plot of Residual Sugar", xlab = "ResidualSugar", ylab = "",col = "pink",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$Chlorides)
hist(wine.df$Chlorides, main="Histogram for Chlorides",xlab="Chlorides", border="blue",col="green",xlim=c(-1.171,1.27),las=1,breaks=100)
boxplot(wine.df$Chlorides, main = "Box plot of Chlorides", xlab = "Chlorides", ylab = "",col = "yellow",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$FreeSulfurDioxide)
hist(wine.df$FreeSulfurDioxide, main="Histogram for Free Sulfur Dioxide",xlab="FreeSulfurDioxide", border="blue",col="brown",xlim=c(-555,622),las=1,breaks=100)
boxplot(wine.df$FreeSulfurDioxide, main = "Box plot of Free Sulfur Dioxide", xlab = "FreeSulfurDioxide", ylab = "",col = "red",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$TotalSulfurDioxide)
hist(wine.df$TotalSulfurDioxide, main="Histogram for TotalSulfurDioxide",xlab="TotalSulfurDioxide", border="blue",col="red",xlim=c(-793,1057),las=1,breaks=100)
boxplot(wine.df$TotalSulfurDioxide, main = "Box plot of TotalSulfurDioxide", xlab = "TotalSulfurDioxide", ylab = "",col = "green",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$Density)
hist(wine.df$Density, main="Histogram for Density",xlab="Density", border="blue",col="violet",xlim=c(-0.8881,1.0992),las=1,breaks=100)
boxplot(wine.df$Density, main = "Box plot of Density", xlab = "Density", ylab = "",col = "gold",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$pH)
hist(wine.df$pH, main="Histogram for pH",xlab="pH", border="blue",col="red",xlim=c(-0.480,5.94),las=1,breaks=100)
boxplot(wine.df$pH, main = "Box plot of pH", xlab = "pH", ylab = "",col = "green",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$Sulphates)
hist(wine.df$Sulphates, main="Histogram for Sulphates",xlab="Sulphates", border="blue",col="violet",xlim=c(-3.13,4.11),las=1,breaks=100)
boxplot(wine.df$Sulphates, main = "Box plot of Sulphates", xlab = "Sulphates", ylab = "",col = "gold",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$Alcohol)
hist(wine.df$Alcohol, main="Histogram for Alcohol",xlab="Alcohol", border="blue",col="blue",xlim=c(-4.50,26.50),las=1,breaks=100)
boxplot(wine.df$Alcohol, main = "Box plot of Alcohol", xlab = "Alcohol", ylab = "",col = "green",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$LabelAppeal)
hist(wine.df$LabelAppeal, main="Histogram for LabelAppeal",xlab="LabelAppeal", border="blue",col="green",xlim=c(-2,2),las=1,breaks=100)
boxplot(wine.df$LabelAppeal, main = "Box plot of LabelAppeal", xlab = "LabelAppeal", ylab = "",col = "orange",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$AcidIndex)
hist(wine.df$AcidIndex, main="Histogram for AcidIndex",xlab="AcidIndex", border="blue",col="pink",xlim=c(-4,17),las=1,breaks=100)
boxplot(wine.df$AcidIndex, main = "Box plot of AcidIndex", xlab = "AcidIndex", ylab = "",col = "pink",border = "brown",horizontal = TRUE,notch = TRUE)

summary(wine.df$STARS)
hist(wine.df$STARS, main="Histogram for STARS",xlab="STARS", border="blue",col="green",xlim=c(1,4),las=1,breaks=100)
boxplot(wine.df$STARS, main = "Box plot of STARS", xlab = "STARS", ylab = "",col = "yellow",border = "brown",horizontal = TRUE,notch = TRUE)

# Obtain a correlation plot 
par(mfrow=c(1,1))
cor.df <- cor(wine.df)
corrplot(cor.df,method="circle")

# Exploratory data analysis using models
model.1 <- lm(TARGET ~ LabelAppeal + STARS, data=wine.df)
summary(model.1)
vif(model.1)

model.2 <- lm(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, data=wine.df)
summary(model.2)
vif(model.2)

# Dimension reduction using PCA

# compute the principal components using the reduced data
returns.pca <- princomp(x=wine.df[,2:15],cor=TRUE)
names(returns.pca)
# See the output components returned by princomp();
names(returns.pca)
pc.1 <- returns.pca$loadings[,1];
pc.2 <- returns.pca$loadings[,2];
names(pc.1)

colors = rainbow(length(unique(pc.1)),start=0.1,end=0.9)
names(colors) = unique(pc.1)
# create a plot of the principal components pc.1 vs pc.2
plot(-20,20,type='p',xlim=c(-0.80,0.40),ylim=c(-0.40,0.65),xlab='PC 1',ylab='PC 2')
text(pc.1,pc.2,labels=names(pc.1),cex=1.00,col = colors)
title("plot of pc.1 vs pc.2")

biplot(returns.pca)

#2D visualization using t-SNE

tsne_data <- wine.df

str(tsne_data)
head(tsne_data)
names(tsne_data)

colors = rainbow(length(unique(wine.df$TARGET)),start=0.1,end=0.5)
names(colors) = unique(wine.df$TARGET)

tsne_plot <- function(perpl=30,iterations=500,learning=200){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(tsne_data %>% select(-TARGET), dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning)
  plot(tsne$Y, t='n', main = print(paste0("perplexity = ",perpl, ", max_iter = ",iterations, ", learning rate = ",learning)), xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=1, cex.lab=1.5)
  text(tsne$Y, labels = tsne_data$TARGET, col = colors[tsne_data$TARGET],cex=1.35)
}

# Test with different perplexity values 
par(mfrow=c(3,2))
perplexity_values <- c(2,5,30,50,65,80)
sapply(perplexity_values, function(i){tsne_plot(perpl=i)})

# Test with different learning rate values
learning_values <- c(20,100,200,1000,2000)
sapply(learning_values,function(i){tsne_plot(learning=i)})

# Test with different max iteration values
max_iters_values <- c(250, 500, 1000, 2000, 3000, 4000)
sapply(max_iters_values, function(i){tsne_plot(iterations=i)})


reduced.set<- wine.df %>% select(-TARGET)
#.rowNamesDF(reduced.set,make.names=TRUE) <-wine.df$TARGET
#row.names(reduced.set)
#names(reduced.set)
d1 <- dist(reduced.set) # euclidean distances between the rows
fit <- cmdscale(d1, eig=TRUE, k=2)
fit
par(mfrow=c(1,1))
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",main="Metric MDS",type="n")
text(x, y, labels = row.names(reduced.set), cex=.7,col='blue')

library(MASS)
d2 <- dist(reduced.set) # euclidean distances between the rows
fit <- isoMDS(d2, k=2) # k is the number of dim
fit # view results
# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(reduced.set), cex=.7,col='red')

# kmeans clustering with k=4 clusters on PCA data
pc.cor.df <- data.frame(pc1=pc.1, pc2=pc.2)
clusterresults <- kmeans(pc.cor.df,4)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults$tot.withinss
clusterresults$betweenss

# cluster plots for kmeans for k=4
par(mfrow=c(1,1))
clusplot(pc.cor.df, clusterresults$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=0)

## K means clustering
subdat <- pc.cor.df
wssplot <- function(subdat, nc=10, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(subdat, centers=i)$withinss)}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")}
par(mfrow=c(1,2))
wssplot(subdat)

model.3 <- lm(TARGET ~ AcidIndex + TotalSulfurDioxide +Sulphates+ LabelAppeal, data=wine.df)
summary(model.3)
vif(model.3)
