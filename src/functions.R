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