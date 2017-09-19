#'@export
linreg<-function(formula,data){
  #ex : f<-Petal.Length ~ Sepal.Width + Sepal.Length
  
  dependent_variable_name<-all.vars(formula)[1]
  Y<-data[[dependent_variable_name]] # vector of dependent variable
  X<-model.matrix(formula,data) # matric of independent variables
  
  
  ## calculation using QR decomposition
  qr<-qr(X)
  beta_hat<-qr.coef(qr,Y) # regression coefficients
  fitted<-qr.fitted(qr,Y) # fitted values
  res<-qr.resid(qr,Y) # residuals
  
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
  
  p_values <- pt(beta,df)
  
  return(beta)
}

#'@exportClass linreg
#'@export linreg
#'@import methods

linreg<-setRefClass("linreg", 
                    fields=list(formula="formula",data="data.frame"),
                    methods=list(
                      resid = function(){
                        return(123456)
                      }
                      
                    ))