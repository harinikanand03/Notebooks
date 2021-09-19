library(cluster)
require(ggplot2)

setwd("/Users/harini-mac/Desktop/Northwestern University/MSDS-411/Week7/Assignment04/")
# read the European Employment file
my.data <- read.csv("EuropeanEmployment.csv")

# Check the structure and the data set
str(my.data)
head(my.data)
dim(my.data)

# remove any rows with missing values
my.data <-  na.omit(my.data)
dim(my.data)

# distribution table for Group
par(mfrow=c(1,1))
barplot(table(my.data$Group), col=c("red","blue","green","purple"), main="Barplot for the Group variable")

par(mfrow=c(4,2))
# EDA for AGR
summary(my.data$AGR)
hist(my.data$AGR,main="Histogram for Agriculture variable",xlab="AGR",border="blue",col="orange",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$AGR,main = "Box plot of Agriculture variable", xlab = "AGR",ylab = "",col = "orange",border = "brown",horizontal = TRUE,notch = FALSE)

# EDA for MIN
summary(my.data$MIN)
hist(my.data$MIN,main="Histogram for Mining variable",xlab="MIN",border="blue",col="green",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$MIN,main = "Box plot of Mining variable", xlab = "MIN",ylab = "",col="green",border = "brown",horizontal = TRUE,notch = FALSE)

# EDA for MAN
summary(my.data$MAN)
hist(my.data$MAN,main="Histogram for Manufacturing variable",xlab="MAN",border="blue",col="blue",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$MIN,main = "Box plot of Manufacturing variable", xlab = "MAN",ylab = "",col="blue",border = "brown",horizontal = TRUE,notch = FALSE)

# EDA for PS
summary(my.data$PS)
hist(my.data$PS,main="Histogram for Power and water supply variable",xlab="PS",border="blue",col="aquamarine",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$PS,main = "Box plot of Power and water supply variable", xlab = "PS",ylab = "",col="aquamarine",border = "brown",horizontal = TRUE,notch = FALSE)

par(mfrow=c(5,2))
# EDA for CON
summary(my.data$CON)
hist(my.data$CON,main="Histogram for Construction variable",xlab="CON",border="blue",col="purple",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$CON,main = "Box plot of Construction variable", xlab = "CON",ylab = "",col = "purple",border = "brown",horizontal = TRUE,notch = FALSE)

# EDA for SER
summary(my.data$SER)
hist(my.data$SER,main="Histogram for Services variable",xlab="SER",border="blue",col="darkgreen",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$SER,main = "Box plot of Services variable", xlab = "SER",ylab = "",col="darkgreen",border = "brown",horizontal = TRUE,notch = FALSE)

# EDA for FIN
summary(my.data$FIN)
hist(my.data$FIN,main="Histogram for Finance variable",xlab="FIN",border="blue",col="red",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$FIN,main = "Box plot of Finance variable", xlab = "FIN",ylab = "",col="red",border = "brown",horizontal = TRUE,notch = FALSE)

# EDA for SPS
summary(my.data$SPS)
hist(my.data$SPS,main="Histogram for Social and personal services variable",xlab="SPS",border="violet",col="lightblue",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$SPS,main = "Box plot of Social and personal services variable", xlab = "SPS",ylab = "",col="lightblue",border = "brown",horizontal = TRUE,notch = FALSE)

# EDA for TC
summary(my.data$TC)
hist(my.data$TC,main="Histogram for Transport and communications variable",xlab="TC",border="violet",col="pink",xlim=c(0,100),las=1,breaks=10)
boxplot(my.data$TC,main = "Box plot of Transport and communications variable", xlab = "TC",ylab = "",col="pink",border = "brown",horizontal = TRUE,notch = FALSE)

#Pairwise scatterplot
pairs(my.data[,-c(1,2)],col="red")
#pairs(my.data[,-c(1,2)],pch=19, col=as.numeric(my.data$Group)+1)

# FIN vs SER scatterplot
ggplot(my.data, aes(x=SER, y=FIN, colour = Group, label= Country)) +geom_point() +
  geom_text(aes(label=Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot Financial vs Services") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# FIN vs MAN scatterplot
ggplot(my.data, aes(x=MAN, y=FIN, colour = Group, label= Country)) +
  geom_point() + geom_text(aes(label=Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot Financial vs Manufacturing") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Principal component analysis
# PCA with No standardization - compositional data 
apply(my.data[,-c(1,2)],MARGIN=1,FUN=sum)
pca.out <- princomp(x=my.data[,-c(1,2)],cor=FALSE);
names(pca.out)
pc.1 <- pca.out$scores[,1];
pc.2 <- pca.out$scores[,2];
str(pc.1)
pcdf = data.frame(pc1=pc.1, pc2=pc.2)
pcdf1 = cbind(pcdf,my.data$Country)
pcdf2 = cbind(pcdf1,my.data$Group)
str(pcdf2)
ggplot(pcdf2, aes(x=pc1, y=pc2, colour = my.data$Group, label= 
                    my.data$Country)) +
  geom_point() + geom_text(aes(label=my.data$Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# PCA with Correlation matrix
pca.cor<- princomp(x=my.data[,-c(1,2)],cor=TRUE);
names(pca.cor)
pc.cor.1 <- pca.cor$scores[,1];
pc.cor.2 <- pca.cor$scores[,2];
str(pc.cor.1)
pc.cor.df= data.frame(pc1=pc.cor.1, pc2=pc.cor.2)
pc.cor.df1 = cbind(pc.cor.df,my.data$Country)
pc.cor.df2 = cbind(pc.cor.df1,my.data$Group)
str(pc.cor.df2)
ggplot(pc.cor.df2, aes(x=pc.cor.1, y=pc.cor.2, colour = my.data$Group, label= 
                    my.data$Country)) +
  geom_point() + geom_text(aes(label=my.data$Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#############################################
# Hirerarchical clustering - using raw data##
#############################################
hier.dist = dist(my.data[,-c(1,2)])
require(maptree)
hclustmodel <- hclust(hier.dist, method = 'complete')
par(mfrow=c(1,1))
plot(hclustmodel,labels=my.data$Country)
# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
head(cut.3)
cut.3
df3 <- cbind(my.data,cut.3)
df3
# cross tab of clusters vs Group
table(df3$Group,df3$cut.3)

# accuracy for k=3 - Between % ss
subdat <- my.data[,-c(1,2)]
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete3 <- cutree(hclust(hier.dist),3)
WSS <- cluster.stats(hier.dist,complete3, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# choose the number of clusters k = 6
cut.6 <- cutree(hclustmodel, k=6)
head(cut.6)
cut.6
df6 <- cbind(my.data,cut.6)
df6
# cross tab of clusters vs Group
table(df6$Group,df6$cut.6)

# accuracy for k=6 - Between % ss
subdat <- my.data[,-c(1,2)]
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
complete6 <- cutree(hclust(hier.dist),6)
WSS <- cluster.stats(hier.dist,complete6, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

##################################################################
# Hirerarchical clustering - using PCA from covariance matrix    #
#                                                                #
##################################################################
hier.dist = dist(pcdf)
require(maptree)
hclustmodel <- hclust(hier.dist, method = 'complete')
par(mfrow=c(1,1))
plot(hclustmodel,labels=my.data$Country)
# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
head(cut.3)
cut.3
df3 <- cbind(pcdf2,cut.3)
df3
colnames(df3) <- c("pc1","pc2","Country","Group","cut.3")
# cross tab of clusters vs Group
table(df3$Group,df3$cut.3)

# accuracy for k=3 - Between % ss
subdat <- pcdf
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete3 <- cutree(hclust(hier.dist),3)
WSS <- cluster.stats(hier.dist,complete3, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# choose the number of clusters k = 6
cut.6 <- cutree(hclustmodel, k=6)
head(cut.6)
cut.6
df6 <- cbind(pcdf2,cut.6)
df6
colnames(df6) <- c("pc1","pc2","Country","Group","cut.6")
# cross tab of clusters vs Group
table(df6$Group,df6$cut.6)

# accuracy for k=6 - Between % ss
subdat <- pcdf
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete6 <- cutree(hclust(hier.dist),6)
WSS <- cluster.stats(hier.dist,complete6, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

##################################################################
# k-Means clustering                                             #
#                                                                #
##################################################################
addLabel <- function(x) {
  label <- paste(x['Country'],x['Group'],sep="_")
  return(label)
}
label <- apply(my.data,1,function(x) addLabel(x))

#l1 <- 1:nrow(my.data)
#l2 <- my.data$Group
#label1 <- paste(l1, l2, sep="_")

rownames(my.data) <- label
# kmeans clustering with k=3 clusters
clusterresults.3 <- kmeans(my.data[,-c(1,2)],3)
names(clusterresults.3)
BetSSPer <- clusterresults.3$betweenss/clusterresults.3$totss
BetSSPer
clusterresults.3$totss
clusterresults.3$tot.withinss
clusterresults.3$betweenss

# cluster plots for kmeans (k=3)
clusplot(my.data[,-c(1,2)], clusterresults.3$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=1)

# kmeans clustering with k=6 clusters
clusterresults.6 <- kmeans(my.data[,-c(1,2)],6)
names(clusterresults.6)
BetSSPer <- clusterresults.6$betweenss/clusterresults.6$totss
BetSSPer
clusterresults.6$totss
clusterresults.6$tot.withinss
clusterresults.6$betweenss
#plot(clusterresults, data=my.data[,-c(1,2)])

# cluster plots for kmeans
library(cluster)
clusplot(my.data[,-c(1,2)], clusterresults.6$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=1)

#############################################################
#            K-Means clustering with PCA data               #
#############################################################
rownames(pcdf) <- label

# kmeans clustering with k=3 clusters
clusterresults.pca.3 <- kmeans(as.matrix(pcdf),3)
names(clusterresults.pca.3)
BetSSPer <- clusterresults.pca.3$betweenss/clusterresults.pca.3$totss
BetSSPer
clusterresults.pca.3$totss
clusterresults.pca.3$tot.withinss
clusterresults.pca.3$betweenss

# cluster plots for kmeans
clusplot(pcdf, clusterresults.pca.3$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=1)

# kmeans clustering with k=6 clusters
clusterresults.pca.6 <- kmeans(as.matrix(pcdf),6)
names(clusterresults.pca.6)
BetSSPer <- clusterresults.pca.6$betweenss/clusterresults.pca.6$totss
BetSSPer
clusterresults.pca.6$totss
clusterresults.pca.6$tot.withinss
clusterresults.pca.6$betweenss
#plot(clusterresults, data=my.data[,-c(1,2)])

# cluster plots for kmeans
#clusplot(pcdf,  clusterresults.pca.6$cluster, color=TRUE, 
#         shade=TRUE,
#         labels=2, lines=0)

clusplot(pcdf, clusterresults.pca.6$cluster, color=TRUE, 
         shade=TRUE,
        labels=2, lines=1)

#############################################################
# Computing the ‘Optimal’ Number of Clusters by Brute Force #
#############################################################
# Internal validation
## K means clustering
subdat <- my.data[, -c(1,2)]
wssplot <- function(subdat, nc=20, seed=1234) {
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

subdat <- pcdf
wssplot <- function(subdat, nc=20, seed=1234) {
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

## Hierarchical clustering
subdat <- my.data[, -c(1,2)]
wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    require(fpc)
    set.seed(seed)
    hier.dist <- dist(subdat)
    complete3 <- cutree(hclust(hier.dist),i)
    wss[i] <- cluster.stats(hier.dist,complete3, 
                            alt.clustering=NULL)$within.cluster.ss}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")
  return(wss)}
par(mfrow=c(1,2))
wssplot(subdat)

subdat <- pcdf
wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    require(fpc)
    set.seed(seed)
    hier.dist <- dist(subdat)
    complete3 <- cutree(hclust(hier.dist),i)
    wss[i] <- cluster.stats(hier.dist,complete3, 
                            alt.clustering=NULL)$within.cluster.ss}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")
  return(wss)}
par(mfrow=c(1,2))
wssplot(subdat)

#############################################################
# On Your Own Modeling 1  - CLUSTER SOLUTION USSTATES DS    #
#############################################################
library(readxl)
setwd("/Users/harini-mac/Desktop/Northwestern University/MSDS-411/Week7/Assignment04/")
# read the European Employment file
my.df <- read_excel("USStates.xlsx")

# Check the structure and the data set
str(my.df)
head(my.df)
dim(my.df)

# remove any rows with missing values
my.df <-  na.omit(my.df)
dim(my.df)

# Scatter plot of USSTATES data 
pairs(my.df[,-c(1,2)], main = "Pairwise scatter plot of USSTATES", pch = 19, col="green")

#############################################
# Hirerarchical clustering - using raw data##
#############################################
# Extract the continuous variables into a matrix
usstates.mat <- as.matrix(my.df[,-c(1,2)])
# Perform center scaling to mean 0 and sd 1
usstates.mat <- scale(usstates.mat, center=TRUE, scale=TRUE)
# Compute the Eucliean distance on the matrix
hier.dist = dist(usstates.mat)
require(maptree)
hclustmodel <- hclust(hier.dist, method = 'complete')
par(mfrow=c(1,1))
plot(hclustmodel,labels=my.df$State)
# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
head(cut.3)
cut.3
df3 <- cbind(my.df,cut.3)
df3
# cross tab of clusters vs Group
table(df3$Region,df3$cut.3)

# accuracy for k=3 - Between % ss
subdat <- as.data.frame(usstates.mat)
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete3 <- cutree(hclust(hier.dist),3)
WSS <- cluster.stats(hier.dist,complete3, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# choose the number of clusters k = 6
cut.6 <- cutree(hclustmodel, k=6)
head(cut.6)
cut.6
df6 <- cbind(my.df,cut.6)
df6
# cross tab of clusters vs Group
table(df6$Region,df6$cut.6)

# accuracy for k=6 - Between % ss
subdat <- as.data.frame(usstates.mat)
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
complete6 <- cutree(hclust(hier.dist),6)
WSS <- cluster.stats(hier.dist,complete6, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# choose the number of clusters k = 6
cut.6 <- cutree(hclustmodel, k=6)
head(cut.6)
cut.6
df6 <- cbind(my.df,cut.6)
df6
# cross tab of clusters vs Group
table(df6$Region,df6$cut.6)

# accuracy for k=6 - Between % ss
subdat <- as.data.frame(usstates.mat)
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
complete6 <- cutree(hclust(hier.dist),6)
WSS <- cluster.stats(hier.dist,complete6, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# choose the number of clusters k = 14
cut.14 <- cutree(hclustmodel, k=14)
head(cut.14)
cut.14
df14 <- cbind(my.df,cut.14)
df14
# cross tab of clusters vs Group
table(df14$Region,df14$cut.14)

# accuracy for k=14 - Between % ss
subdat <- as.data.frame(usstates.mat)
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
complete14 <- cutree(hclust(hier.dist),14)
WSS <- cluster.stats(hier.dist,complete14, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

######################################
## Hierarchical clustering
subdat <- scale(as.matrix(my.df[, -c(1,2)]))
wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    require(fpc)
    set.seed(seed)
    hier.dist <- dist(subdat)
    complete3 <- cutree(hclust(hier.dist),i)
    wss[i] <- cluster.stats(hier.dist,complete3, 
                            alt.clustering=NULL)$within.cluster.ss}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")
  return(wss)}
par(mfrow=c(1,2))
wssplot(subdat)

# PCA with Correlation matrix
pca.cor<- princomp(x=my.df[,-c(1,2)],cor=TRUE);
names(pca.cor)
pc.cor.1 <- pca.cor$scores[,1];
pc.cor.2 <- pca.cor$scores[,2];
str(pc.cor.1)
pc.cor.df= data.frame(pc1=pc.cor.1, pc2=pc.cor.2)
pc.cor.df1 = cbind(pc.cor.df,my.df$State)
pc.cor.df2 = cbind(pc.cor.df1,my.df$Region)
str(pc.cor.df2)
ggplot(pc.cor.df2, aes(x=pc.cor.1, y=pc.cor.2, colour = my.df$Region, label= 
                         my.df$State)) +
  geom_point() + geom_text(aes(label=my.df$State),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

hier.dist = dist(pc.cor.df)
require(maptree)
hclustmodel <- hclust(hier.dist, method = 'complete')
par(mfrow=c(1,1))
plot(hclustmodel,labels=my.df$State)
# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
head(cut.3)
cut.3
df3 <- cbind(pc.cor.df2,cut.3)
df3
colnames(df3) <- c("pc1","pc2","State","Region","cut.3")
# cross tab of clusters vs Group
table(df3$Region,df3$cut.3)

# accuracy for k=3 - Between % ss
subdat <- pc.cor.df
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete3 <- cutree(hclust(hier.dist),3)
WSS <- cluster.stats(hier.dist,complete3, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# choose the number of clusters k = 6
cut.6 <- cutree(hclustmodel, k=6)
head(cut.6)
cut.6
df6 <- cbind(pc.cor.df2,cut.6)
df6
colnames(df6) <- c("pc1","pc2","State","Region","cut.6")
# cross tab of clusters vs Group
table(df6$Region,df6$cut.6)

# accuracy for k=6 - Between % ss
subdat <- pc.cor.df
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete6 <- cutree(hclust(hier.dist),6)
WSS <- cluster.stats(hier.dist,complete6, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# choose the number of clusters k = 8
cut.8 <- cutree(hclustmodel, k=8)
head(cut.8)
cut.8
df8 <- cbind(pc.cor.df2,cut.8)
df8
colnames(df8) <- c("pc1","pc2","State","Region","cut.8")
# cross tab of clusters vs Group
table(df8$Region,df8$cut.8)

# accuracy for k=8 - Between % ss
subdat <- pc.cor.df
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete8 <- cutree(hclust(hier.dist),8)
WSS <- cluster.stats(hier.dist,complete8, 
                     alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

## Hierarchical clustering
subdat <- pc.cor.df
wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    require(fpc)
    set.seed(seed)
    hier.dist <- dist(subdat)
    complete3 <- cutree(hclust(hier.dist),i)
    wss[i] <- cluster.stats(hier.dist,complete3, 
                            alt.clustering=NULL)$within.cluster.ss}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")
  return(wss)}
par(mfrow=c(1,2))
wssplot(subdat)
#############################################################
# On Your Own Modeling 2  - CLUSTER SOLUTION RECIDIVSIM DS    #
#############################################################
library(readxl)
setwd("/Users/harini-mac/Desktop/Northwestern University/MSDS-411/Week7/Assignment04/")
# read the recidivism file
my.data <- read_excel("recidivism.xlsx")

# Check the structure and the data set
str(my.data)
head(my.data)
dim(my.data)

# remove any rows with missing values
recidivism.df <- na.omit(my.data)
# check the dimensions again 
dim(recidivism.df)
names(recidivism.df)

# Apply scale - standardization with mean 0 and sd 1
recidivism.mat <- scale(as.matrix(recidivism.df),center=TRUE,scale=TRUE)
recidivism.scaled.df <- as.data.frame(recidivism.mat)

## K means clustering
subdat <- recidivism.mat
wssplot <- function(subdat, nc=30, seed=1234) {
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


# kmeans clustering with k=11 clusters
clusterresults <- kmeans(recidivism.mat,11)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults$tot.withinss
clusterresults$betweenss
  
# cluster plots for kmeans
par(mfrow=c(1,1))
library(cluster)
clusplot(recidivism.scaled.df, clusterresults$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=0)

# kmeans clustering with k=20 clusters
clusterresults <- kmeans(recidivism.mat,20)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults$tot.withinss
clusterresults$betweenss

# cluster plots for kmeans
par(mfrow=c(1,1))
library(cluster)
clusplot(recidivism.scaled.df, clusterresults$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=0)

# PCA with Correlation matrix
pca.cor<- princomp(x=recidivism.mat,cor=TRUE);
names(pca.cor)
pc.cor.1 <- pca.cor$scores[,1];
pc.cor.2 <- pca.cor$scores[,2];
str(pc.cor.1)
pc.cor.df= data.frame(pc1=pc.cor.1, pc2=pc.cor.2)
par(mfrow=c(1,1))
plot(pc.cor.1, pc.cor.2,col="maroon",main="plot of PC1 vs PC2 scores")

## K means clustering
subdat <- pc.cor.df
wssplot <- function(subdat, nc=20, seed=1234) {
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

# kmeans clustering with k=5 clusters on PCA data
clusterresults <- kmeans(pc.cor.df,5)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults$tot.withinss
clusterresults$betweenss

# cluster plots for kmeans for k=5
par(mfrow=c(1,1))
clusplot(pc.cor.df, clusterresults$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=0)

# kmeans clustering with k=7 clusters on PCA data
clusterresults <- kmeans(pc.cor.df,7)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults$tot.withinss
clusterresults$betweenss

# cluster plots for kmeans for k=7
par(mfrow=c(1,1))
clusplot(pc.cor.df, clusterresults$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=0)

# kmeans clustering with k=9 clusters on PCA data
clusterresults <- kmeans(pc.cor.df,9)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults$tot.withinss
clusterresults$betweenss

# cluster plots for kmeans for k=9
par(mfrow=c(1,1))
clusplot(pc.cor.df, clusterresults$cluster, color=TRUE, 
         shade=TRUE,
         labels=2, lines=0)