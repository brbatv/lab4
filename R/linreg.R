
linreg<-function(formula,data){
  #ex : f<-Petal.Length ~ Sepal.Width + Sepal.Length
  
  dependent_variable_name<-all.vars(formula)[1]
  Y<-data[[dependent_variable_name]] # vector of dependent variable
  X<-model.matrix(formula,data) # matric of independent variables
  X_n<-model.matrix(formula,data)[,-1] # same without intercept column
  
  
  ## calculation using QR decomposition
  qr<-qr(X)
  beta_hat<-qr.coef(qr,Y)
  
  
  
  ## RC class tryout 
  linreg<-setRefClass("linreg", 
  fields=list(beta_hat="numeric",y_hat="numeric"))
  return(beta)
}
