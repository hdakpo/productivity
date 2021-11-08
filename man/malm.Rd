\name{malm}
\alias{malm}
\alias{print.Malmquist}
\title{Malmquist productivity index}
\description{Using Data Envelopment Analysis (DEA), this function measures 
productivity with Malmquist index.}

\usage{malm(data, id.var, time.var, x.vars, y.vars, tech.reg = TRUE, rts = c("vrs", "crs", 
  "nirs", "ndrs"), orientation = c("out", "in"), parallel = FALSE, cores = max(1, 
  detectCores() - 1), scaled = TRUE)

\method{print}{Malmquist}(x, digits = NULL, \dots)}

\arguments{
  \item{data}{A dataframe containing the required information for measuring productivity.}
  \item{id.var}{Firms' ID variable. Can be an integer or a text string.}
  \item{time.var}{Time period variable. Can be an integer or a text string.}
  \item{x.vars}{Input quantity variables. Can be a vector of text strings or integers.}
  \item{y.vars}{Output quantity variables. Can be a vector of text strings or integers.}
  \item{tech.reg}{Logical. If \code{TRUE} (default), the model allows for negative technological change 
  (i.e. technological regress). If \code{FALSE}, only positive technological change (i.e. technological progress) 
  is allowed. See also the \code{Details} section.}
  \item{rts}{Character string specifying the returns to scale assumption. 
  The default value is \code{"vrs"} (variable returns to scale). Other possible options 
  are \code{"crs"} (constant returns to scale), \code{"nirs"} (non-increasing returns to scale), 
  or \code{"ndrs"} (non-decreasing returns to scale).}
  \item{orientation}{Character string specifying the orientation. The default value is \code{"out"} 
  (output-orientation). The other possible option is \code{"in"} (input-orientation).}
  \item{parallel}{Logical. Allows parallel computation. If \code{FALSE} (default) the estimation is conducted 
  in sequential mode. If \code{TRUE}, parallel mode is activated using the number of cores specified in \code{cores}. 
  When the sample size is small, it is recommended to keep the \code{parallel} option to its default value (\code{FALSE}).}
  \item{cores}{Integer. Used only if \code{parallel = TRUE}. It specifies the number of cores to be used 
  for parallel computation. By default, \code{cores = max(1, detectCores() - 1)}.}
   \item{scaled}{Logical. If \code{TRUE} (default), input and output quantities are rescaled. If \code{FALSE}, a warning message 
  is displayed when very large (>1e5) and/or very small (<1e-4) values are present in the input and output quantity variables.}
  \item{x}{An object of class \verb{'Malmquist'}.}
  \item{digits}{The minimum number of significant digits to be printed in values.
  Default = \code{max(3, getOption("digits") - 3)}.} 
  \item{\dots}{Currently not used.} 
}

\details{
Distance functions required for computing the Malmquist index are radial measures which verify the translation 
invariance property. Hence, unless very large or very small values are present, the Malmquist index is insensitive 
to the rescaling option (\code{scaled}).
}

\value{
\code{malm()} returns a list of class \verb{'Malmquist'} for which a summary of productivity measures 
in levels and changes is printed. 

This list contains the following items:

\item{Levels}{It contains the Shephard distance function estimates, useful to compute and decompose 
  the Malmquist productivity index. These distance functions use input and output quantities 
  for period \code{1} and period \code{0}.

  In addition to the \code{id.var} variable and periods \code{1} and \code{0}, the dataframe 
  therefore contains, depending on the orientation: 
  \code{c111o}, \code{c100o}, \code{c011o}, \code{c000o}, \code{c110o}, \code{c010o}, or 
  \code{c111i}, \code{c100i}, \code{c011i}, \code{c000i}, \code{c110i}, \code{c010i}.
  When the returns to scale option (\code{rts}) is different from \code{"crs"}, then \code{v111o} and 
  \code{v000o}, or \code{v111i} and \code{v000i} (depending on the orientation) are returned in addition
  to the distance function estimated under constant returns to scale (\code{"crs"}).
  The prefix "c" stands for constant returns to scale (\code{"crs"}) and "v" for 
  all other types of returns to scale (i.e. \code{"vrs"}, \code{"nirs"}, or \code{"ndrs"}).
  The suffix "o" means output-oriented while "i" refers to input-oriented.
  
  The distance function names are displayed with three digits: 
  \emph{(i)} the first digit represents the period of the reference technology, 
  \emph{(ii)} the second digit represents the period of the inputs, and 
  \emph{(iii)} the third digit represents the period of the outputs. 
  For instance \code{c010o} means output-oriented efficiency under constant returns to scale (\code{"crs"}), 
  with the reference technology of period \code{0}, inputs of period \code{1} and outputs of period \code{0}.}

\item{Changes}{Malmquist productivity index and its components are provided, 
depending on the orientation.
  \tabular{ll}{
    \code{malmquist} \tab Malmquist productivity index\cr
    \code{effch} \tab Efficiency change\cr
    \code{tech} \tab Technological change\cr
    \code{obtech} \tab Output-biased technological change\cr 
    \code{ibtech} \tab Input-biased technological change\cr
    \code{matech} \tab Magnitude component\cr
    \code{pure.out.effch} \tab Pure output efficiency change\cr
                          \tab \code{    } \emph{(when \code{rts != "crs"} and \code{orientation = "out"})}\cr
    \code{out.scalech} \tab Output scale efficiency change\cr
                       \tab \code{    } \emph{(when \code{rts != "crs"} and \code{orientation = "out"})}\cr
    \code{pure.inp.effch} \tab Pure input efficiency change\cr
                          \tab \code{    } \emph{(when \code{rts != "crs"} and \code{orientation = "in"})}\cr
    \code{inp.scalech} \tab Input scale efficiency change\cr
                       \tab \code{    } \emph{(when \code{rts != "crs"} and \code{orientation = "in"})}}}
  
\bold{Note that:}
\enumerate{
\item{\code{obtech} (Output-biased technological change), \code{ibtech} (Input-biased technological change), 
  and \code{matech} (Magnitude component) are components of technological change (\code{tech}).}
\item{\code{pure.out.effch} (Pure output efficiency change) and \code{out.scalech} (Output scale efficiency change) 
  are components of efficiency change (\code{effch}).}
\item{\code{pure.inp.effch} (Pure input efficiency change), and \code{inp.scalech} (Input scale efficiency change) 
  are components of efficiency change (\code{effch}).}}

From an object of class \verb{'Malmquist'} obtained from \code{malm()}, the 
\itemize{
\item \code{\link{Levels}} function extracts Shephard distance function estimates; and 
\item \code{\link{Changes}} function extracts Malmquist productivity index and components.}
}

\references{
Färe R., and Grosskopf S. (1996), \emph{Intertemporal Production Frontiers: With 
Dynamic DEA}. Springer Eds.}

\author{K Hervé Dakpo, Yann Desjeux, Laure Latruffe}

\note{The Malmquist productivity index and components are computed such that both \code{orientation}'s 
results provide the same information: growth when index greater than one and decline when index lower 
than one. Moreover under \code{rts = "crs"}, both \code{orientation} options (i.e. \code{"out"} and 
\code{"in"}) yield the same results.}

\section{Warning}{The \code{malm()} function will not work with unbalanced panel data.}

\seealso{See \code{\link{Levels}} to retrieve a data frame with Shephard distance function estimates.\cr
See \code{\link{Changes}} to retrieve a data frame with Malmquist productivity index and components.}

\examples{

## Malmquist productivity index compares each observation in period 1 to the same 
## observation in period 0
\dontrun{
  Malmquist <- malm(data = usagri, id.var = "States", time.var = "Years", 
  x.vars = c("q.capital", "q.land","q.labor","q.materials"), 
  y.vars = c("q.livestock", "q.crop", "q.other"), rts = "nirs")
  Malmquist
}
}

\keyword{models}
