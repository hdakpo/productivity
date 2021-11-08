\name{usagri}
\alias{usagri}
\docType{data}
\title{Price indices and implicit quantities of USA farm outputs and inputs by State, 1995-2004}
\description{
This data set from the United States Department of Agriculture (USDA) and its 
Economic Research Service department contains USA agriculture's input and output 
quantities along with their respective price indices for 48 States.

All quantities are expressed in thousand US$1996 and prices are relative to 
Alabama 1996 = 1.}

\usage{usagri}

\format{
  A data frame with 480 observations on the following 17 variables:
  \tabular{ll}{
    - \code{p.livestock}: \tab Livestock and animal products' relative price (reference is 1 = Alabama 1996).\cr
    - \code{States}: \tab 48 States of the USA identified with two capital letters.\cr
    - \code{States.num}: \tab State number.\cr
    - \code{Years}: \tab Year.\cr
    - \code{q.livestock}: \tab Livestock and animal products' quantity, in thousand US$1996.\cr
    - \code{q.crop}: \tab Crops' quantity, in thousand US$1996.\cr
    - \code{q.other}: \tab Other farm-related productions' quantity, in thousand US$1996.\cr
    - \code{q.capital}: \tab Capital services' quantity, in thousand US$1996.\cr
    - \code{q.land}: \tab Land services' quantity, in thousand US$1996.\cr
    - \code{q.labor}: \tab Labor services' quantity, in thousand US$1996.\cr
    - \code{q.materials}: \tab Total intermediate input quantity, in thousand US$1996.\cr
    - \code{p.livestock}: \tab Livestock and animal products' relative price (reference is 1 = Alabama 1996).\cr
    - \code{p.crop}: \tab Crops' relative price (reference is 1 = Alabama 1996).\cr
    - \code{p.other}: \tab Other farm-related productions' relative price (reference is 1 = Alabama 1996).\cr
    - \code{p.capital}: \tab Capital services' relative price (reference is 1 = Alabama 1996).\cr
    - \code{p.land}: \tab Land service flows' relative price (reference is 1 = Alabama 1996).\cr
    - \code{p.labor}: \tab Labor services' relative price (reference is 1 = Alabama 1996).\cr
    - \code{p.materials}: \tab Total intermediate inputs' relative price (reference is 1 = Alabama 1996).}
}

\details{
Further details on the data and the different variables can be found in the references.}
\source{
\url{http://www.ers.usda.gov/data-products/agricultural-productivity-in-the-us.aspx}
}

\references{
Ball V.E., Gollop F.M., Kelly-Hawke A., and Swinand G.P. (1999), Patterns of state 
productivity growth in the US farm sector: Linking state and aggregate models. 
\emph{American Journal of Agricultural Economics}, \bold{81}(1], 164--179.
\url{https://doi.org/10.2307/1244458}

Ball V.E., Hallahan C., and Nehring R. (2004), Convergence of productivity: An 
analysis of the catch-up hypothesis within a panel of states. \emph{American Journal 
of Agricultural Economics}, \bold{86}(5), 1315--1321.
\url{https://doi.org/10.1111/j.0002-9092.2004.00683.x}
}

\examples{
head(usagri)
str(usagri)
summary(usagri)
}

\keyword{datasets}
