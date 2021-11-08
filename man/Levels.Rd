\name{Levels}
\alias{Levels}

\title{Productivity and profitability levels}

\description{This function extracts individual productivity and profitability (when available) 
levels from any object created by either \code{\link{fareprim}}, \code{\link{fisher}}, 
\code{\link{hicksmoorsteen}}, \code{\link{laspeyres}}, \code{\link{lowe}}, \code{\link{malm}}, 
 or \code{\link{paasche}} function.
}

\usage{Levels(object, \dots)}

\arguments{
  \item{object}{Object of class \verb{'FarePrimont'}, \verb{'Fisher'}, \verb{'HicksMoorsteen'}, 
  \verb{'Laspeyres'}, \verb{'Lowe'}, \verb{'Malmquist'}, or \verb{'Paasche'}.}
  \item{\dots}{Currently not used.}
}

\details{
\itemize{
\item An object of class \verb{'FarePrimont'} is a result of a call to \code{\link{fareprim}}.
\item An object of class \verb{'Fisher'} is a result of a call to \code{\link{fisher}}.
\item An object of class \verb{'HicksMoorsteen'} is a result of a call to \code{\link{hicksmoorsteen}}.
\item An object of class \verb{'Laspeyres'} is a result of a call to \code{\link{laspeyres}}.
\item An object of class \verb{'Lowe'} is a result of a call to \code{\link{lowe}}.
\item An object of class \verb{'Malmquist'} is a result of a call to \code{\link{malm}}.
\item An object of class \verb{'Paasche'} is a result of a call to \code{\link{paasche}}.}
}

\value{
\itemize{
  \item In the case of Färe-Primont, Fisher, Laspeyres, Lowe, Malmquist, and Paasche indices, the function returns 
    a \bold{data frame} containing all the elements and observations included in the \code{"Levels"} component of \code{object}.
  \item In the case of Hicks-Moorsteen index:
    \itemize{
      \item When \code{components = FALSE} (default) in the call to \code{\link{hicksmoorsteen}}, the function returns a \bold{data frame} 
        containing all the elements and observations included in the \code{"Levels"} component of the object of class \verb{'HicksMoorsteen'}.
      \item When \code{components = TRUE} in the call to \code{\link{hicksmoorsteen}}, the function returns a \bold{list} of three data frames:
        \describe{
          \item{\code{*} HicksMoorsteen:}{A data frame containing all the elements and observations related to \code{"Levels"} component of the Hicks-Moorsteen index.}
          \item{\code{*} MalmquistHS:}{A data frame containing all the elements and observations related to \code{"Levels"} component of the Malmquist-\emph{hs} index.}
          \item{\code{*} MalmquistIT:}{A data frame containing all the elements and observations related to \code{"Levels"} component of the Malmquist-\emph{it} index.}
        }
    }
}
}

\author{Yann Desjeux, K Hervé Dakpo, Laure Latruffe}

\seealso{For details and information on returned values, see \code{\link{fareprim}}, 
\code{\link{fisher}}, \code{\link{hicksmoorsteen}}, \code{\link{laspeyres}}, \code{\link{lowe}}, 
\code{\link{malm}}, or \code{\link{paasche}}.\cr

See also:\cr
- \code{\link{Changes}} for productivity and profitability change indices; and \cr
- \code{\link{Shadowp}} for shadow prices.}

\examples{
\dontrun{
  LOWE <- lowe(data = usagri, id.var = "States", time.var = "Years", x.vars = c(7:10), 
  y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13))
  Lowe.levels <- Levels(LOWE)
  head(Lowe.levels)
}
}

\keyword{manip}
