## ------------------------------------------------------------------------
data <- iris
formula <- Petal.Length ~ Sepal.Width + Sepal.Length

my_linear_reg <- linreg$new(formula, data)

residuals <- my_linear_reg$res

residuals 


## ------------------------------------------------------------------------
data <- iris

formula <- Petal.Length ~ Sepal.Width + Sepal.Length

my_linear_reg <- linreg$new(formula, data)

my_linear_reg$print()


## ----message=FALSE, fig.align="center",fig.height = 5, fig.width = 9-----
my_linear_reg$plot()


## ----message=FALSE-------------------------------------------------------

my_linear_reg$summary()


## ------------------------------------------------------------------------
data <- iris

formula <- Petal.Length ~ Species

my_linear_reg <- linreg$new(formula, data)


## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(iris,10))

## ------------------------------------------------------------------------

my_linear_reg$print()


## ------------------------------------------------------------------------

my_linear_reg$summary()


