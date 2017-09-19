
linreg<-function(formula,data){
  
  
  # data<-iris
  # Petal.Lenght <- data$Petal.Length
  # Sepal.Width  <- data$Sepal.Width
  # Sepal.Length <- data$Sepal.Length
  #ex : formula<-data$Petal.Length ~ data$Sepal.Width + data$Sepal.Length
  
  
  dependent_variable_name<-all.vars(formula)[1]
  Y<-data[[dependent_variable_name]] # vector of dependent variable
  X<-model.matrix(formula,data) # matric of independent variables
  
  ## calculation using QR decomposition
  qr<-qr(X)
  beta_hat<-qr.coef(qr,Y)
  fitted<-qr.fitted(qr,Y)
  res<-qr.resid(qr,Y)
  
  #degree of freedom
  
  n <- dim(data)[1]
  p <- dim(data)[2]-2
  df<- n-p
  
  #residual variance
  
  res_var <- (t(res) %*% res)/df
  
  
  #variance of the regression coefficients
  
  reg_var <- (as.numeric(res_var) * solve((t(X) %*% X)))
  diag(reg_var)
  
  #t-values for each coefficient
  
  t_values <- beta_hat / sqrt(diag(reg_var))
  
  #p-value ????
  
  p_value <- pt(fitted,df)
  
  #f<-Petal.Length ~ Sepal.Width + Sepal.Length
  return(X)
  
  #first graph
  plot(fitted,res, ylab="Residuals",xlab="Fitted values of lm", main="Residuals vs Fitted")
  
  stand_res <- abs((res-mean(res))/sqrt(var(res)))
  ggplot(fitted, stand_res)
  

  ## RC class tryout 
  linreg<-setRefClass("linreg", 
  fields=list(beta_hat="numeric",y_hat="numeric"))
  return(beta)

}
