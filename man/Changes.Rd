\name{Changes}
\alias{Changes}

\title{Productivity and profitability change indices}

\description{This function extracts individual productivity and profitability (when available) change 
indices from any object created by either \code{\link{fareprim}}, \code{\link{fisher}}, 
\code{\link{hicksmoorsteen}}, \code{\link{laspeyres}}, \code{\link{lowe}}, \code{\link{malm}}, 
 or \code{\link{paasche}} function.
}

\usage{Changes(object, \dots)}

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
    a \bold{data frame} containing all the elements and observations included in the \code{"Changes"} component of \code{object}.
  \item In the case of Hicks-Moorsteen index:
    \itemize{
      \item When \code{components = FALSE} (default) in the call to \code{\link{hicksmoorsteen}}, the function returns a \bold{data frame} 
        containing all the elements and observations included in the \code{"Changes"} component of the object of class \verb{'HicksMoorsteen'}.
      \item When \code{components = TRUE} in the call to \code{\link{hicksmoorsteen}}, the function returns a \bold{list} of three data frames:
        \describe{
          \item{\code{*} HicksMoorsteen:}{A data frame containing all the elements and observations related to \code{"Changes"} component of the Hicks-Moorsteen index.}
          \item{\code{*} MalmquistHS:}{A data frame containing all the elements and observations related to \code{"Changes"} component of the Malmquist-\emph{hs} index.}
          \item{\code{*} MalmquistIT:}{A data frame containing all the elements and observations related to \code{"Changes"} component of the Malmquist-\emph{it} index.}
        }
    }
}
}

\author{Yann Desjeux, K Hervé Dakpo, Laure Latruffe}

\seealso{For details and information on returned values, see \code{\link{fareprim}}, 
\code{\link{fisher}}, \code{\link{hicksmoorsteen}}, \code{\link{laspeyres}}, \code{\link{lowe}}, 
\code{\link{malm}}, or \code{\link{paasche}}.\cr

See also:\cr
- \code{\link{Levels}} for productivity and profitability levels; and \cr
- \code{\link{Shadowp}} for shadow prices.}

\examples{
\dontrun{
  FAREPRIM <- fareprim(data = usagri, id.var = "States", time.var = "Years", 
  x.vars = c("q.capital", "q.land","q.labor","q.materials"), y.vars = c("q.livestock", 
  "q.crop", "q.other"), w.vars = c("p.capital", "p.land", "p.labor", "p.materials"), 
  p.vars = c("p.livestock", "p.crop", "p.other"))
  Fareprim.change <- Changes(FAREPRIM)
  head(Fareprim.change)
}
}

\keyword{manip}
