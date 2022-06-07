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



model1 <- glm(formula = class ~ mean + stand_deviation + kurtosis +
      skewness + mean_DM.SNR + stand_deviation_DM.SNR +  kurtosis_DM.SNR + skewness_DM.SNR, data = pulsar_stars, family = binomial())

summary(model1)

if(!require(car)){
  install.packages("car")
  library(car)
}
vif(model1)

pulsar_stars2 <- subset(pulsar_stars, select = -c(kurtosis_DM.SNR, skewness_DM.SNR))

pulsar_stars2$class <- as.factor(pulsar_stars2$class)

if(!require(caTools)){
  install.packages("caTools")
  library(caTools)
}

sample = sample.split(pulsar_stars2,SplitRatio = 0.75)
train1 =subset(pulsar_stars2,sample ==TRUE)
test1 =subset(pulsar_stars2,sample ==FALSE)


model2 <- glm(formula = class ~ mean + stand_deviation + kurtosis +
                skewness + mean_DM.SNR + stand_deviation_DM.SNR, data = train1, family = binomial())


vif(model2)

summary(model2)

if(!require(corrplot)){
  install.packages("corrplot")
  library(corrplot)
}

library(pcaPP)
base_kor <- pulsar_stars[,-9]
kendall2 <-cor.fk (base_kor)
kendall2 <- round(kendall2, 2)
kendall2[kendall2 < 0.5 & kendall2 > -0.5] <- "not correlated"

source("http://michael.hahsler.net/SMU/EMIS7332/R/copytable.R") 
copytable(kendall2)

correlations <- cor(pulsar_stars2[,1:6])
corrplot(correlations, method="circle", tl.cex = 0.5)


model2$coefficients


install.packages("caret")
library(caret)

test1$class <- as.factor(test1$class)
predicted2 <- predict(model2, test1)

if(!require(InformationValue)){
  install.packages("InformationValue")
  library(InformationValue)
}

optCutOff2 <- optimalCutoff(test1$class, predicted2)


glm.pred <- ifelse(predicted2 > 0.1, 1, 0)
glm.pred <- as.factor(glm.pred)
misClassError(test1, predicted2, threshold = optCutOff2)

library(caret)
caret::confusionMatrix(glm.pred, test1$class, positive = "1")


plotROC(test1$class, predicted2)

#Gini
rf.roc<-roc(test1$class, predicted2)
gini = 2*auc(rf.roc)-1
gini 
#precision(test1$class, predicted2)
      