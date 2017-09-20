#'A RC class for multiple linear regression
#'
#'@field formula The formula object containing depedent and independent variables
#'@field data A data frame object to apply the multiple linear regression to
#'
#'
#'@exportClass linreg
#'@export linreg
#formula<-Petal.Length ~ Sepal.Width + Sepal.Length
#a<-linreg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris)


linreg<-setRefClass("linreg", 
                    fields=list(formula="formula",data="data.frame",beta_hat="numeric",res="numeric",fitted="numeric",name_of_data_input="character"),
                    methods=list(
                      initialize = function(formula,data){ # this function is using $new <3
                        formula<<-formula
                        name_of_data_input<<-deparse(substitute(data)) 
                        data<<-data
                        
                        dependent_variable_name<-all.vars(formula)[1]
                        Y<-data[[dependent_variable_name]] # vector of dependent variable
                        X<-model.matrix(formula,data) # matric of independent variables
                        qr<-qr(X)
                        beta_hat<<-qr.coef(qr,Y) # regression coefficients
                        fitted<<-qr.fitted(qr,Y) #the fitted values
                        res<<-qr.resid(qr,Y) # residuals
                        
                        #degrees of freedom
                        n <- dim(data)[1]
                        p <- dim(data)[2]-2
                        df<- n-p
                        
                        res_var <- (t(res) %*% res)/df # residual variance
                        
                        #variance of the regression coefficients
                        reg_var <- diag(as.numeric(res_var) * solve((t(X) %*% X)))
                        
                        #t-values for each coefficient
                        t_values <- beta_hat / sqrt(reg_var)
                        
                        #p-value for each coefficient
                        p_value<-2*pt(abs(t_values),df,lower.tail = FALSE)
                        
                        
                      },
                      resid = function(){
                        "Returns residual vector"
                        return(res) #done
                      },
                      pred = function(){
                        "Returns fitted values"
                        return(fitted) #done
                        
                      },
                      coef=function(){
                        "Returns linear regression coefficients"
                        return(beta_hat) #done
                        
                      },
                      summary=function(){
                        "Returns a summary of the linear regression"
                        
                      },
                      plot=function(){
                        #first graph ---> missing red line
                        ggplot(data.frame(fitted,res),aes(y=res,x=fitted))+geom_point(shape=1)
                        
                        #second graph -----> missing red line
                        
                        #stand_res <- sqrt(abs((res-mean(res))/sqrt(var(res))))
                        #plot(fitted, stand_res, ylab=expression(sqrt(abs("Standardized residuals"))),xlab="Fitted values of lm", main="Scale-Location")
                        
                        
                      },
                      print=function(){
                        "Prints the input and the coefficients in a user-friedly way"
                      cat("Call: \n ")
                      cat(paste0("linreg(formula = ",format(formula)," data = ",name_of_data_input,")\n\n"))
                      #cat(paste0(" ",names(beta_hat)))
                      cat("Coefficients: \n")
                      beta_hat
                      
                      
                      }
                      
                    ))



pop<-function(data)
{
  return("data")
}

