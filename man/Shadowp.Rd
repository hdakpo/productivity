\name{Shadowp}
\alias{Shadowp}

\title{Shadow prices used in productivity and profitability computations}

\description{From any object created by either \code{\link{fareprim}}, \code{\link{fisher}}, 
\code{\link{hicksmoorsteen}}, \code{\link{laspeyres}}, \code{\link{lowe}}, or \code{\link{paasche}} 
function, and provided that the argument \code{shadow} (or \code{components} in the case of \code{\link{hicksmoorsteen}}) 
is set to \code{TRUE} in the function's call, this function extracts the deflated cost input and revenue output 
shadow prices.}

\usage{Shadowp(object, \dots)}

\arguments{
  \item{object}{Object of class \verb{'FarePrimont'}, \verb{'Fisher'}, \verb{'HicksMoorsteen'}, 
  \verb{'Laspeyres'}, \verb{'Lowe'}, or \verb{'Paasche'}.}
  \item{\dots}{Currently not used.}
}

\details{
Shadow prices are derived from dual input- and output-oriented DEA models. 

In the case of Fisher, Hicks-Moorsteen, Laspeyres, Lowe and Paasche indices, deflated input and output shadow 
prices \bold{for each observation} are returned (if the argument \code{shadow = TRUE} in the function's call).

In the case of Hicks-Moorsteen index, shadow prices used to compute Malmquist-\emph{hs} and Malmquist-\emph{it} 
\bold{for each observation} are returned (if the argument \code{components = TRUE} in the function's call).

In the case of Färe-Primont index, the deflated input and output shadow prices \bold{of the representative observation} 
(i.e. the sample means of quantities and prices) are returned (if the argument \code{shadow = TRUE} in the function's call).

\itemize{
\item An object of class \verb{'FarePrimont'} is a result of a call to \code{\link{fareprim}}.
\item An object of class \verb{'Fisher'} is a result of a call to \code{\link{fisher}}.
\item An object of class \verb{'HicksMoorsteen'} is a result of a call to \code{\link{hicksmoorsteen}}.
\item An object of class \verb{'Laspeyres'} is a result of a call to \code{\link{laspeyres}}.
\item An object of class \verb{'Lowe'} is a result of a call to \code{\link{lowe}}.
\item An object of class \verb{'Paasche'} is a result of a call to \code{\link{paasche}}.}
}

\value{
\itemize{
  \item In the case of Färe-Primont, Fisher, Laspeyres, Lowe and Paasche indices, the function returns 
    a \bold{data frame} containing all the elements and observations included in the \code{"Shadowp"} component of \code{object}.
  \item In the case of Hicks-Moorsteen index, the function returns a \bold{list} of two data frames containing, for each observation, 
    input and output deflated shadow prices related to Malmquist-\emph{hs} and Malmquist-\emph{it} indices (and therefore 
    not Hicks-Moorsteen shadow prices per se).
      \describe{
        \item{\code{*} MalmquistHS:}{A data frame containing, for each observation, input and output shadow prices of the Malmquist-\emph{hs} index.}
        \item{\code{*} MalmquistIT:}{A data frame containing, for each observation, input and output shadow prices of the Malmquist-\emph{it} index.}
      }
    }
}

\section{Warning}{
For extreme efficient observations, the problem of multiple solutions may arise and the values of 
shadow prices may differ depending on the linear programming solver used (here \pkg{lpSolveAPI}).}

\author{Yann Desjeux, K Hervé Dakpo, Laure Latruffe}

\seealso{
- \code{\link{Changes}} for productivity and profitability change indices; and \cr
- \code{\link{Levels}} for productivity and profitability levels.}

\examples{
\dontrun{
  FISHER <- fisher(data = usagri, id.var = "States", time.var = "Years", x.vars = c(7:10), 
  y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13), orientation = "out", shadow = TRUE)
  Fisher.shadowprices <- Shadowp(FISHER)
  head(Fisher.shadowprices)
}
}

\keyword{manip}
