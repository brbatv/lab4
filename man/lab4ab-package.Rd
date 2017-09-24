\name{lab4ab-package}
\alias{lab4ab-package}
\alias{lab4ab}
\docType{package}
\title{
\packageTitle{lab4ab}
}
\description{
\packageDescription{lab4ab}

`linreg$new()` helps to calculate all the most significative elements of a linear regression in just one step just giving the observations (contained in a dataset) and the formula.

The package also provides a printing method ($print()), a summary ($summary()) and two plots ($plot()).

The printing shows the coefficients found in the linear regression and their names; the summary acts just as the one of the lm() function; the two plots represent the the relationship between the fitted value and the residuals (the actual value and the square of the standardized value).

}
\details{



}
\author{
Alessia De Biase, Bruno Barakat

Maintainer: Bruno Barakat
}
\references{

}
\keyword{ package }
\seealso{

}
\examples{

data <- iris

formula <- Petal.Length ~ Sepal.Width + Sepal.Length

my_linear_reg <- linreg$new(formula, data)

my_linear_reg$print()
my_linear_reg$plot()
my_linear_reg$summary()


}
