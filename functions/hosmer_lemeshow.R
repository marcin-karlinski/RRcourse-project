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
#' than the number of degrees of freedom. 
#' @return  Chi square test statistic and corresponding p-value of 
#' the Hosmer-Lemeshow goodness of fit test
#' @examples
#' hosmerlem(data$actual_values, data_predicted_values, 10)

hosmerlem = function(y, yhat, g=20) {
  #sorting the fitted values and dividing them in g number of groups
  cutyhat = cut(yhat, breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE) 
  #creating a contingency table from cross-classifying factors, first for actual values
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)  
  #and for fitted values
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)  
  #calculating chi-square test statistic
  chisq = sum((obs - expect)^2/expect) 
  #calculating p-value
  P = pchisq(chisq, g - 2)  
  #returning test statistic and p-value
  return(list(chisq=chisq,p.value=P))
}