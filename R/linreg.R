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
                        require(ggplot2)
                        require(magick)
                        require(grid)
                       
                        
                        logo <- image_read('http://www.ida.liu.se/mall11/images/logo-sv.png')
                        g_pic<- rasterGrob(logo,interpolate=TRUE)
                        
                        phras<- paste("ln(",format(formula),")")
                        
                        #first graph
                        x_max<-max(fitted)
                        y_max<-max(res)
                        first<-ggplot(data.frame(fitted,res),aes(y=res,x=fitted))+geom_point(shape=1,size=3)+xlab(paste("Fitted values", phras, sep = "\n"))+ ylab("Residuals")+ ggtitle("Residuals vs Fitted") +geom_hline(yintercept=0, linetype="dashed")+geom_smooth(span = 1.5,colour="turquoise4",method="loess",se=FALSE)+theme_LIU()+annotation_custom(g_pic, xmin=x_max-2, xmax=x_max-1, ymin=y_max-6, ymax=y_max-1)

                        #second graph : scale-location
                        stand_res <- sqrt(abs((res-mean(res))/sqrt(var(res))))
                        x_max<-max(fitted)
                        y_max<-max(stand_res)
                        second<-ggplot(data.frame(fitted,stand_res),aes(y=stand_res,x=fitted))+geom_point(shape=1)+xlab(paste("Fitted values",phras, sep = "\n"))+ ylab(expression(sqrt(abs("Standardized residuals")))) + ggtitle("Scale-Location")+geom_smooth(span = 1.5,colour="turquoise4",method="loess",se=FALSE)+theme_LIU()+annotation_custom(g_pic, xmin=x_max-2, xmax=x_max-1, ymin=y_max-6, ymax=y_max-1)
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


theme_LIU <- function (base_size = 11, base_family = "sans") 
{  
  half_line <- base_size/2
  theme(
    line = element_line(colour = "black", size = 0.5, 
                        linetype = 1, lineend = "butt"), 
    rect = element_rect(fill = "white", colour = "black",
                        size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        colour = "white", size = base_size,
                        lineheight = 0.9,  hjust = 0.5,
                        vjust = 0.5, angle = 0, 
                        margin = margin(), debug = FALSE), 
    axis.line = element_blank(), 
    axis.text = element_text(size = rel(1), colour = "grey30"),
    axis.text.x = element_text(margin = margin(t = 0.8*half_line/2), 
                               vjust = 1), 
    axis.text.y = element_text(margin = margin(r = 0.8*half_line/2),
                               hjust = 1),
    axis.ticks = element_line(colour = "grey20"),
    axis.ticks.length = unit(half_line/2, "pt"), 
    axis.title.x = element_text(size = rel(1.2),margin = margin(t = half_line,
                                                                b = half_line*1.5)),
    axis.title.y = element_text(size = rel(1.2), angle = 90, 
                                margin = margin(r = half_line*1.5,
                                                l = half_line)),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    
    legend.background = element_rect(colour = NA), 
    legend.margin = margin(6,6,6,6),
    legend.key = element_rect(fill = "grey95", colour = "white"),
    legend.key.size = unit(1.2, "lines"), 
    legend.key.height = NULL,
    legend.key.width = NULL, 
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0), 
    legend.title.align = NULL, 
    legend.position = "right", 
    legend.direction = NULL,
    legend.justification = "center", 
    legend.box = NULL, 
    legend.spacing = NULL,
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.box.margin = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(), 
    panel.grid.major = element_line(colour = "grey95"), 
    panel.grid.minor = element_line(colour = "grey95", size = 0.25), 
    #panel.margin = unit(half_line, "pt"), 
    panel.margin.x = NULL, 
    panel.margin.y = NULL, panel.ontop = FALSE, 
    panel.spacing = element_blank(), 
    panel.spacing.x = element_blank(), 
    panel.spacing.y = element_blank(), 
    plot.subtitle = element_blank(), 
    plot.caption = element_blank(), 
    
    
    strip.background = element_rect(fill = "grey85", colour = NA),
    strip.text = element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x = element_text(size=1), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = half_line, 
                                                r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    strip.placement = "outside" ,
    
    plot.background = element_rect(fill = "mediumturquoise"), 
    plot.title = element_text(size = rel(2), 
                              margin = margin(b = half_line*2.5, l=half_line)),
    plot.margin = margin(0.8,0.8,0.8,0.8,"cm"),
    complete = TRUE)
}

