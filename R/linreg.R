
linreg<-function(formula,data){
  dependent_variable_name<-all.vars(formula)[1]
  Y<-data[[dependent_variable_name]] # vector of dependent variable
  X<-model.matrix(formula,data) # matric of independent variables
  
  beta<-solve((t(X) %*% X)) %*% t(X) %*% Y
  
  #degree of freedom
  
  n <- dim(data)[1]
  p <- dim(data)[2]-1
  df<- n-p
  
  #residual variance
  
  res_var <- (t(res) %*% res)/df
  
  #variance of the regression coefficients
  
  reg_var <- res_var*solve((t(X) %*% X))
  
  #t-values for each coefficient
  
  t_values <- beta / sqrt(reg_var)
  
  #p-value ????
  
  p_value <- pt(beta,df)
  
  #first graph
  plot(fitted_values,resids, ylab="Residuals",xlab="Fitted values of lm")
  
  
  
  #f<-Petal.Length ~ Sepal.Width + Sepal.Length
  return(X)
}
