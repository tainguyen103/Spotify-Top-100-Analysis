install.packages('dplyr')
library(dplyr)

install.packages('MASS')
library(MASS)

install.packages('ggplot2')
library(ggplot2)

install.packages('tibble')
library(tibble)

data <- read.csv('C:\\Users\\nguyenky\\Downloads\\SpotifyTop100.csv')
head(data)

getwd()

summary(data)

#VISUALIZATIONS

boxplot(data$year.released, main="Year of Release", col="Gray")
boxplot(data$bpm, main="Beats Per Minute", col="Gray")
boxplot(data$nrgy, main="Energy Level ", col="Gray")
boxplot(data$dnce, main="Danceability", col="Gray")
boxplot(data$dB, main="Decibel/Loudness", col="Gray")
boxplot(data$live, main="Live", col="Gray")
boxplot(data$val, main="Positivity/Mood of Song", col="Gray")
boxplot(data$dur, main="Duration", col="Gray")
boxplot(data$acous, main="Acoustic", col="Gray")
boxplot(data$spch, main="Concentration Level on Spoken Words", col="Gray")
boxplot(data$pop, main="Popularity", col="Gray")


data_numvar<-dplyr::select(data,bpm,nrgy,dnce,dB,live,val,dur,acous,spch,pop)
head(data_numvar)

data_numvar<-na.omit(data_numvar)

data_cluster<-kmeans(data_numvar,5)
data_cluster

par(mfrow=c(3,3))

plot(pop~bpm,data_numvar,col=data_cluster$cluster)
plot(pop~nrgy,data_numvar,col=data_cluster$cluster)
plot(pop~dnce,data_numvar,col=data_cluster$cluster)
plot(pop~dB,data_numvar,col=data_cluster$cluster)
plot(pop~live,data_numvar,col=data_cluster$cluster)
plot(pop~val,data_numvar,col=data_cluster$cluster)
plot(pop~dur,data_numvar,col=data_cluster$cluster)
plot(pop~acous,data_numvar,col=data_cluster$cluster)
plot(pop~spch,data_numvar,col=data_cluster$cluster)

#REGRESSION
data_Atype <- as.factor(data$artist.type)
data_Atype <- model.matrix(~0 + data_Atype)
data1 <- cbind(data_numvar,data_Atype)
head(data1)

data_model <- lm(pop~., data=data1)
data_model
options(scipen = 200)
summary(data_model)


data_topgen <- as.factor(data$top.genre)
data_topgen <- model.matrix(~0 + data_topgen)
data2 <- cbind(data_numvar,data_Atype,data_topgen)
head(data2)

data_model2 <- lm(pop~., data=data2)
data_model2
options(scipen = 200)
summary(data_model2)

#CLASSIFICATION1
data3 <- dplyr::select(data,-title,-artist,-top.genre,-year.released,-added,-top.year,-artist.type)
data3

training <- sample(1:nrow(data3), 0.8*nrow(data3))
trainingset <- data3[training,]

validation <- setdiff(1:nrow(data3), training)
validationset <- data3[validation,]

newdata <- dplyr::mutate(data3,class = ifelse(data$pop > 74, 1, 0))
newdata

library(class)

K_list <- 1:20
MAE <- 0
for (k in K_list){
  knn_pred <- knn(trainingset[,-which(colnames(trainingset) == "pop")],
                  validationset[,-which(colnames(validationset) == "pop")],
                  trainingset[,"pop"],
                  k=k)
  MAE<-cbind(MAE, mean(abs(as.numeric(as.character(knn_pred)) - validationset$pop)))
}

plot(K_list, MAE[-1],
     xlab = "K", ylab = "MAE")

#CLASSIFICATION2

newdata <- newdata[,-10]
newdata
newdata <- na.omit(newdata)
newdata
training <- sample(1:nrow(newdata), 0.8*nrow(newdata))
trainingset <- newdata[training,]

validation <- setdiff(1:nrow(newdata), training)
validationset <- newdata[validation,]

knn_pred <- knn(trainingset,
                validationset,
                trainingset$class,
                k=5)

summary(knn_pred)

table(knn_pred)

knn_pred

