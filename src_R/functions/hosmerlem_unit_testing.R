source("./src_R/functions/hosmer_lemeshow.R")

library(titanic)
library(dplyr)
library(testthat)

data("titanic_train")

#creating a very basic logistic regression model
titanic_train <- titanic_train %>%
 na.omit() %>%
 dplyr::select(Survived, Pclass, Sex, Age, Parch)

set.seed(123)
model <- glm(Survived ~.,family=binomial(link='logit'),data=titanic_train)

predicted <- predict(model,
                    newdata = titanic_train, type = "response")

hosmerlem(y=titanic_train$Survived, yhat=predicted, g=10)


testthat::test_that('hosmerlem general test', {
  testthat::expect_equal(hosmerlem(titanic_train$Survived, 
                                   yhat=predicted, 
                                   g=10), 
                         list(chisq = 32.04523, 
                              p.value = 0.9999))
})

testthat::test_that('hosmerlem not numeric', {
  testthat::expect_error(hosmerlem(y = as.character(titanic_train$Survived), 
                                   yhat = as.character(predicted), 
                                   g=10))
  testthat::expect_error(hosmerlem(y = as.character(titanic_train$Survived), 
                                   yhat = predicted, 
                                   g=10))
  testthat::expect_error(hosmerlem(titanic_train$Survived, 
                                   yhat = as.character(predicted), 
                                   g=10))
})

#I will change the values of the y from 0 and 1 to 2 and 3 and supply those 
#values to the function
titanic_train <- titanic_train %>% 
  mutate(Survived_test = ifelse(Survived == 1, 2, 3))

testthat::test_that('hosmerlem y of 0s and 1s', {
  testthat::expect_error(hosmerlem(y = titanic_train$Survived_test, 
                                   yhat = predicted, 
                                   g=10))
})

#I will add 1 to every element of the vector so that the values will be outside 
#of the 0-1 range.
predicted_test <- predicted + 1

testthat::test_that('hosmerlem predicted values outside of 0-1', {
  testthat::expect_error(hosmerlem(y = titanic_train$Survived, 
                                   yhat = predicted_test, 
                                   g=10))
})

testthat::test_that('not equal length of y and fitted y', {
  testthat::expect_error(hosmerlem(y = append(titanic_train$Survived, 1), #adding a single value to the vector
                                   yhat = predicted_test, 
                                   g=10))
})

testthat::test_that('g too large', {
  testthat::expect_error(hosmerlem(y = titanic_train$Survived, 
                                   yhat = predicted_test, 
                                   g=10000))
})
