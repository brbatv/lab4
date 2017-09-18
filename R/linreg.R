
linreg<-function(formula,data){
  dependent_variable_name<-all.vars(formula)[1]
  Y<-data[[dependent_variable_name]] # vector of dependent variable
  X<-model.matrix(formula,data) # matric of independent variables
  X_n<-model.matrix(formula,data)[,-1] # same without intercept column
  #f<-Petal.Length ~ Sepal.Width + Sepal.Length
  return(X)
}
