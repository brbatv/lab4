#'@exportClass linreg
#'@export linreg


linreg<-setRefClass("linreg", 
                    fields=list(formula="formula",data="data.frame",beta_hat="numeric",res="numeric",fitted="numeric"),
                    methods=list(
                      initialize = function(formula,data){ # this function is using $new <3
                        formula<<-formula
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
                        reg_var <- (as.numeric(res_var) * solve((t(X) %*% X)))
                        reg_var<-diag(reg_var)
                        
                        #t-values for each coefficient
                        t_values <- beta_hat / sqrt(diag(reg_var))
                        
                        #p-value ????
                        p_value <- pt(fitted,df)
                        
                        
                      },
                      resid = function(){
                        
                        return(res) #done
                      },
                      pred = function(){
                        return(pred) #done
                        
                      },
                      coef=function(){
                        return(beta_hat) #done
                        
                      },
                      summary=function(){
                        
                      },
                      plot=function(){
                        #first graph
                        plot(fitted,res, ylab="Residuals",xlab="Fitted values of lm", main="Residuals vs Fitted")
                        
                        stand_res <- abs((res-mean(res))/sqrt(var(res)))
                        ggplot(fitted, stand_res)
                        
                      },
                      print=function(){
                        
                      }
                      
                    ))

#dependent_variable_name<-all.vars(formula)[1]

#Y<-data[[dependent_variable_name]] # vector of dependent variable
#X<-model.matrix(formula,data) # matric of independent variables
#qr<-qr(X)
#print(formula)
#beta_hat<<-qr.coef(qr,Y) # regression coefficients