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
#a<-linreg$new(formula=Petal.Length ~ Species,data=iris)
#plot(lm(iris$Petal.Length ~ iris$Sepal.Width + iris$Sepal.Length))



linreg<-setRefClass("linreg", 
                    fields=list(formula="formula",data="data.frame",beta_hat="numeric",res="numeric",fitted="numeric",name_of_data_input="character",df="numeric",res_std_error="numeric",p_values="numeric",t_values="numeric",reg_var="numeric"),
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
                        df<<- n-p
                        
                        res_var <- (t(res) %*% res)/df # residual variance
                        res_std_error<<-sqrt(as.numeric(res_var)) 
                        
                        #variance of the regression coefficients
                        reg_var <<- diag(as.numeric(res_var) * solve((t(X) %*% X)))
                        
                        #t-values for each coefficient
                        t_values <<- beta_hat / sqrt(reg_var)
                        
                        #p-value for each coefficient
                        p_values<<-2*pt(abs(t_values),df,lower.tail = FALSE)
                        
                        
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

                        svar<-data.frame("Variable"=as.character(names(beta_hat)),"Estimate"=round(beta_hat,3),"Std Error"=round(sqrt(reg_var),3),"T"=round(t_values,3),"P"=round(p_values,5),stringsAsFactors = FALSE)
                        
                        cat("Call: \n")
                        cat(paste0("linreg(formula = ",format(formula),", data = ",name_of_data_input,")\n\n"))
              
                        cat(names(svar),sep="  ","\n")
                        for(i in 1:nrow(svar)){
                          cat(paste(svar[i,],collapse = " "),sep="",collapse=" ***\n")
                        }
                        cat("",sep="\n")
                        cat(paste0("Residual standard error: ",round(res_std_error,5) ," on " ,df, " degrees of freedom"))
                      },
                      plot=function(){
                        
                        phras<- paste("ln(",format(formula),")")
                        #first graph
                        theme_update(plot.title = element_text(hjust = 0.5))
                        first<-ggplot(data.frame(fitted,res),aes_string(y=res,x=fitted))+geom_point(shape=1,size=3)+xlab(paste("Fitted values", phras, sep = "\n"))+ ylab("Residuals")+ ggtitle("Residuals vs Fitted") +geom_hline(yintercept=0, linetype="dashed")+geom_smooth(span = 1.5,colour="red",method="loess",se=FALSE)
                        
                        #second graph : scale-location
                        stand_res <- sqrt(abs((res-mean(res))/sqrt(var(res))))
                        second<-ggplot(data.frame(fitted,stand_res),aes(y=stand_res,x=fitted))+geom_point(shape=1)+xlab(paste("Fitted values",phras, sep = "\n"))+ ylab(expression(sqrt(abs("Standardized residuals")))) + ggtitle("Scale-Location")+geom_smooth(span = 1.5,colour="red",method="loess",se=FALSE)
                        return(list(first,second)) # needed to show both graphs
                        
                      },
                      print=function(){
                        "Prints the input and the coefficients in a user-friedly way"
                      cat("Call: \n")
                      cat(paste0("linreg(formula = ",format(formula),", data = ",name_of_data_input,")\n\n"))
                      
                      cat("Coefficients: \n")
                      cat(" ")
                      cat(names(beta_hat))
                      cat(" ")
                      cat(sep="\n")
                      cat(sep="      ",beta_hat)
                      }
                      
                    ))

