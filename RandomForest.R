
if(!require(randomForest)){
  install.packages("randomForest")
  library(randomForest)
}

pulsar_stars <- read.csv("pulsar_stars.csv")

colnames(pulsar_stars) <- c(
  "mean",
  "stand_deviation",
  "kurtosis",
  "skewness",
  "mean_DM.SNR",
  "stand_deviation_DM.SNR",
  "kurtosis_DM.SNR",
  "skewness_DM.SNR",
  "class"
)


set.seed(123)

pulsar_stars2 <- subset(pulsar_stars, select = -c(kurtosis_DM.SNR, skewness_DM.SNR))
pulsar_stars2$class <- as.factor(pulsar_stars2$class)

if(!require(caTools)){
  install.packages("caTools")
  library(caTools)
}

sample <- sample.split(pulsar_stars2,SplitRatio = 0.75)
train1 <- subset(pulsar_stars2,sample ==TRUE)
test1 <- subset(pulsar_stars2,sample ==FALSE)

train1$class <- as.factor(train1$class)
test1$class <- as.factor(test1$class)


las_losowy <- randomForest(class ~ ., data = train1, proximity = TRUE)

las_losowy

if(!require(InformationValue)){
  install.packages("InformationValue")
  library(InformationValue)
}

#krzywa ROC
las_losowy_test <- randomForest(class ~ ., data = test1, proximity = TRUE)
plotROC(test1$class, las_losowy_test$votes[,2])

#Gini
rf.roc<-roc(test1$class, las_losowy_test$votes[,2])
gini = 2*auc(rf.roc)-1
gini

if(!require(caret)){
  install.packages("caret")
  library(caret)
}

#install.packages('e1071', dependencies=TRUE)
prediction <- predict(las_losowy, test1)
confusionMatrix(prediction, test1$class, positive = "1")

precision(test1$class, prediction)

