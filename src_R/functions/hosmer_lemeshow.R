#Hosmer Lemeshow test function

#' @title hosmerlem
#' @description Returns chi-square test statistic and corresponding p-value of 
#' the Hosmer-Lemeshow goodness of fit test. Used for binary response variables.
#' Null hypothesis states that the model is well fitted, therefore p-values above 
#' 0.05 are desirable.
#' @param y the target variable in a numeric format, where 1 stands for a positive class,
#' and 0 for negative class.
#' @param yhat fitted probability values of the logistic regression model (between 0 and 1).
#' @param g Arbitrary number of subgroups to use in the calculation of the test. Should be higher 
#' than the number of degrees of freedom. The largest accepted g is 120. 
#' @return  Chi square test statistic and corresponding p-value of 
#' the Hosmer-Lemeshow goodness of fit test
#' @examples
#'library(titanic)
#'library(dplyr)
#'
#'data("titanic_train")
#'
#'titanic_train <- titanic_train %>% 
#'  na.omit() %>% 
#'  dplyr::select(Survived, Pclass, Sex, Age, Parch)
#'
#'model <- glm(Survived ~.,family=binomial(link='logit'), data=titanic_train)
#'
#'set.seed(123)
#'predicted <- predict(model, 
#'                     newdata = titanic_train, type = "response")
#'
#'hosmerlem(y=titanic_train$Survived, yhat=predicted, g=10)

hosmerlem = function(y, yhat, g=20) {
  #first, check if y and yhat are numeric
  if(!is.numeric(y) | !is.numeric(yhat)){
    stop(paste("y and yhat must be numeric vectors. One of them or both are not. Stopping."))
  }
  
  #check if y consists of 0s and 1s
  if(!(length(unique(y)) == 2 & is.element(c(0), unique(y)) & is.element(c(1), unique(y)))){
    stop(paste("y must consists of numeric values of 0 and 1"))
  }
  
  #check if yhat are in the range of 0 to 1
  if(max(yhat) > 1 | min(yhat) < 0){
    stop(paste("yhat must be fitted probabilities in a range of 0 to 1"))
  }
  
  #check if y and yhat are the same length
  if(length(y) != length(yhat)){
    stop(paste("y and yhat must be the same length"))
  }
  
  #check if y and yhat are the same length
  if(g > 120){
    stop(paste("g is too large."))
  }
  
  #sorting the fitted values and dividing them in g number of groups
  cutyhat = cut(yhat, breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE) 
  #creating a contingency table from cross-classifying factors, first for actual values
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)  
  #and for fitted values
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)  
  #calculating chi-square test statistic
  chisq = sum((obs - expect)^2/expect) 
  #calculating p-value
  P = round(pchisq(chisq, g - 2), 4)  
  #returning test statistic and p-value
  return(list(chisq=chisq,p.value=P))
}
