#libraries used in the project
library(tidyverse)
library(caret)
library(car)
library(caTools)
library(corrplot)
library(InformationValue)
library(randomForest)
library(DescTools)
library(leaps)
library(MLmetrics)
library(pcaPP)
library(pROC)
library(MASS) #it masks 'select' from dplyr - beware of that
library(DMwR)
library(reshape2)
library(moments)

#changing the scipen to 999 to avoid scientific notation
options(scipen = 999)
#changing language to english
Sys.setenv(LANG = "en")

#loading dataset
pulsar_stars <- read.csv("pulsar_stars.csv")

#changing the names of the columns
colnames(pulsar_stars) <- c(
  "profile.mean", 
  "profile.st_dev", 
  "profile.kurtosis", 
  "profile.skewness", 
  "DM_SNR.mean", 
  "DM_SNR.st_dev", 
  "DM_SNR.kurtosis", 
  "DM_SNR.skewness", 
  "target")

####Data statistics and preparation####
summary(pulsar_stars)
str(pulsar_stars) 
Desc(pulsar_stars)

#I will change the labels for the target variable from 0 and 1 to 'no' and 'yes'
pulsar_stars$target <- as.character(pulsar_stars$target)
pulsar_stars$target <- ifelse(pulsar_stars$target == "0", "no", "yes")
pulsar_stars$target <- as.factor(pulsar_stars$target)

#checking for NA values - fortunately there are none
colSums(is.na(pulsar_stars)) %>% 
  sort()

#distribution of the target class - negative observations are much more common
table(pulsar_stars$target)

#plot with the distribution of target variable
ggplot(pulsar_stars, aes(target, fill = target)) +
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank()) +
  stat_count(geom = "text", colour = "white", size = 5,
             aes(label = ..count..),position=position_stack(vjust=0.5))


###data split###
#I am creating a data split with stratification (making sure that the distribution
#of the target variable is similiar in both training and testing datasets)
#with a ratio of 0.75 to 0.25

#setting random seed in order to reproduce the results later - it can be any number
set.seed(123)
stars_to_train <- createDataPartition(pulsar_stars$target, 
                                      p = 0.75, 
                                      list = FALSE) 

#subsetting the dataset with rows randomly chosen above for the train data
stars_train <- pulsar_stars[stars_to_train,]
#and rows not present in the train data in order to produce test dataset
stars_test <- pulsar_stars[-stars_to_train,]

#checking if training and testing datasets have similar proportion of positive 
#and negative class observations - they do
nrow(stars_train[stars_train$target == "yes",])/nrow(stars_train[stars_train$target == "no",])
nrow(stars_test[stars_test$target == "yes",])/nrow(stars_test[stars_test$target == "no",])

#Checking for correlation and multicollinearity
#ommitting the last column - it's the target variable and it's a factor - 
#corrplot needs numerical values
stars_cor <- cor(stars_train[,1:8])

#vizualization of the correlation matrix - check the documentation for all the 
#plotting options
corrplot.mixed(stars_cor, upper = "circle", lower = "number", 
               tl.pos = "lt", tl.col = "black")

#Independent variables are not required to have a normal distribution, 
#however sometimes normality (especially no skewness) can give better results. 
#Let's inspect the distribution of all the features. 
#It will also enable us to spot possible outliers. 

melt(stars_train[, 1:8]) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(fill = "lightblue") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal()

#now I will check the skewness of variables (function comes from the package moments)
#there are some variables with pretty high skewness, like profile.skewness and profile.kurtosis
lapply(pulsar_stars[, 1:8], skewness)

#Now I will try to normalize distribution of all the independent variables with 
#a Yeo-Johnson transformation. First I need to create an object containing results
yeojohnson_transformation <- preProcess(stars_train, method = "YeoJohnson", na.remove = F)

#and then used that object to recreate transformed dataset with normalized values
stars_train.normalized <- predict(yeojohnson_transformation, stars_train, na.action = na.pass)

#plots with distributions of normalized variables - as can be seen they resembles 
#normal distribution a little bit more. 
melt(stars_train.normalized[, 1:8]) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(fill = "lightblue") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal()

#repeating the same transformation but for test data
yeojohnson_transformation_test <- preProcess(stars_test, method = "YeoJohnson", na.remove = F)
stars_test.normalized <- predict(yeojohnson_transformation, stars_test, na.action = na.pass)


####Logistic regression model####

#I will create a function with summary statistics that will be used for train control
fiveStats <- function(...) c(twoClassSummary(...), 
                             defaultSummary(...))

#five fold cross validation
ctrl_cv5 <- trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE,
                         summaryFunction = fiveStats)

#creating model using caret package - 'glm' method and family = 'binomial'
#for logistic regression. Also setting random seed since there are some random
#elements in the algorithm
set.seed(123)
glm_model <- train(target~., 
                   data = stars_train.normalized,
                   method = "glm",
                   family = "binomial",
                   trControl = ctrl_cv5)

#predicted values for the test dataset using the model created above - 
#results will be stored in a new column
stars_test.normalized$glm <- predict(glm_model, 
                                     newdata = stars_test.normalized)


#confusion matrix with the results on the test data - data that was not used for 
#building the model
caret::confusionMatrix(stars_test.normalized$glm,
                stars_test.normalized$target, 
                positive = "yes")


#As mentioned above, data is characterized by a high imbalance of the predicted class. 
#I will use SMOTE method from DRwM package in order to perform rebalancing.
#it can be done by just changing sampling parameter of the train_control object to 'smote'
ctrl_cv5$sampling <- "smote"

set.seed(123)
glm_model_smote <- train(target~., 
                         data = stars_train.normalized,
                         method = "glm",
                         family = "binomial",
                         trControl = ctrl_cv5)

#creating column with predicted values in the test dataset
stars_test.normalized$glm_smote <- predict(glm_model_smote, 
                                           newdata = stars_test.normalized)

#confusion matrix with results for rebalanced model on the test dataset
caret::confusionMatrix(data = stars_test.normalized$glm_smote,
                reference = stars_test.normalized$target,
                positive = "yes")


#####Model tests#####
#For testing purposes I will also create a model with just an intercept and 
#a model with all the variables
null_model <- glm(formula = target ~ 1, data=stars_train.normalized, family=binomial())
max_model <- glm(formula = target ~ ., data=stars_train.normalized, family=binomial())

#Multicollinearity check
#there seems to be a moderate multicollinearity, since there are variables
#for which the variance inflation factor is higher than 5, however since 
#we only care about the prediction accuracy, it can be ignored
vif(glm_model_smote$finalModel) #model results can be accessed via finalModel attribute

#Hosmer Lemeshow test function - here it will be better described
hosmerlem = function(y, yhat, g=20) {
  cutyhat = cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE)  
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)  
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)  
  chisq = sum((obs - expect)^2/expect)  
  P = pchisq(chisq, g - 2)  
  return(list(chisq=chisq,p.value=P))
  hr=P
}

stars_train.normalized <- stars_train.normalized %>% 
  mutate(target_num = ifelse(target == "yes", 1, 0))

predicted <- predict(glm_model_smote, 
        newdata = stars_train.normalized, type = "prob")

predicted <- predicted$yes

hosmerlem(y=stars_train.normalized$target_num, yhat=predicted,g=10)
hosmerlem(y=stars_train.normalized$target_num, yhat=predicted,g=7)
hosmerlem(y=stars_train.normalized$target_num, yhat=predicted,g=8)
hosmerlem(y=stars_train.normalized$target_num, yhat=predicted,g=9)


####Roc plot
stars_test.normalized <- stars_test.normalized %>% 
  mutate(target_num = ifelse(target == "yes", 1, 0))

predicted <- predict(glm_model_smote, 
                     newdata = stars_test.normalized, type = "prob")

plotROC(stars_test.normalized$target_num, predicted$yes)

library(pROC)
set.seed(123)
rf.roc<-roc(stars_test.normalized$target_num, predicted$yes)
auc(rf.roc)

#####Random Forest####