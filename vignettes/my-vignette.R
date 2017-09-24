## ------------------------------------------------------------------------
data <- iris
formula <- Petal.Length ~ Sepal.Width + Sepal.Length

#my_linear_reg <- linreg(formula, data)

#residuals <- my_linear_reg$res

#residuals 

#TAKE OFF THE COMMENT SIGN WHEN IT WORKS IN A PROPER WAY

## ------------------------------------------------------------------------
data <- iris
formula <- Petal.Length ~ Sepal.Width + Sepal.Length

#my_linear_reg <- linreg(formula, data)

#print(my_linear_reg)

#TAKE OFF THE COMMENT SIGN WHEN IT WORKS IN A PROPER WAY

## ---- fig.show='hold'----------------------------------------------------

#plot(my_linear_reg)
#plot(data$Petal.Length ~ data$Sepal.Width + data$Sepal.Length)


## ------------------------------------------------------------------------
#summary(my_linear_reg)

#TAKE OFF THE COMMENT SIGN WHEN IT WORKS IN A PROPER WAY

## ---- fig.show='hold',fig.cal="Residuals vs Fitted values"---------------
plot(1:10)
plot(10:1)

## ------------------------------------------------------------------------
data <- iris[1:10,]

formula <- Petal.Length ~ Sepal.Width + Sepal.Length

#my_linear_reg <- linreg(formula, data)


## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(iris,10))

## ---- echo=FALSE, results='asis'-----------------------------------------
#knitr::kable(head(coef(my_linear_reg)))

## ------------------------------------------------------------------------


