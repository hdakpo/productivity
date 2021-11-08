\name{fisher}
\alias{fisher}
\alias{print.Fisher}
\title{Fisher productivity and profitability index}
\description{Using Data Envelopment Analysis (DEA), this function measures 
productivity and profitability in levels and changes with Fisher index.

The Fisher productivity index is the geometric average of Laspeyres and Paasche indices.

Deflated shadow prices of inputs and outputs can also be computed.}

\usage{fisher(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, tech.change = TRUE, 
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", 
  "in", "in-out"), parallel = FALSE, cores = max(1, detectCores() - 1), scaled = TRUE, 
  shadow = FALSE)

\method{print}{Fisher}(x, digits = NULL, \dots)}

\arguments{
  \item{data}{A dataframe containing the required information for measuring productivity and 
  profitability.}
  \item{id.var}{Firms' ID variable. Can be an integer or a text string.}
  \item{time.var}{Time period variable. Can be an integer or a text string.}
  \item{x.vars}{Input quantity variables. Can be a vector of text strings or integers.}
  \item{y.vars}{Output quantity variables. Can be a vector of text strings or integers.}
  \item{w.vars}{Input price variables. Can be a vector of text strings or integers.}
  \item{p.vars}{Output price variables. Can be a vector of text strings or integers.}
  \item{tech.change}{Logical. If \code{TRUE} (default), the model allows for technological change. 
  If \code{FALSE}, technological change is prohibited. See also the \code{Details} section.}
  \item{tech.reg}{Logical. If \code{TRUE} (default), the model allows for negative technological change 
  (i.e. technological regress). If \code{FALSE}, only positive technological change (i.e. technological progress) 
  is allowed. See also the \code{Details} section.}
  \item{rts}{Character string specifying the returns to scale assumption. 
  The default value is \code{"vrs"} (variable returns to scale). Other possible options 
  are \code{"crs"} (constant returns to scale), \code{"nirs"} (non-increasing returns to scale), 
  or \code{"ndrs"} (non-decreasing returns to scale).}
  \item{orientation}{Character string specifying the orientation. 
  The default value is \code{"out"} (output-orientation). Other possible options 
  are \code{"in"} (input-orientation), and \code{"in-out"} (both input- and output-orientations). 
  For \code{"in-out"}, the geometric mean of input- and output-orientations' results is returned.}
  \item{parallel}{Logical. Allows parallel computation. If \code{FALSE} (default) the estimation is conducted 
  in sequential mode. If \code{TRUE}, parallel mode is activated using the number of cores specified in \code{cores}. 
  When the sample size is small, it is recommended to keep the \code{parallel} option to its default value (\code{FALSE}).}
  \item{cores}{Integer. Used only if \code{parallel = TRUE}. It specifies the number of cores to be used 
  for parallel computation. By default, \code{cores = max(1, detectCores() - 1)}.}
  \item{scaled}{Logical. If \code{TRUE} (default), input and output quantities are rescaled. If \code{FALSE}, a warning message 
  is displayed when very large (>1e5) and/or very small (<1e-4) values are present in the input and output quantity variables. 
  See also the \code{Details} section.}
  \item{shadow}{Logical. Default is \code{FALSE} (no shadow prices are returned). When set to \code{TRUE}, input and output 
  shadow prices are returned. These shadow prices are informative only and may be subject to the linear programming solver used.}
  \item{x}{An object of class \verb{'Fisher'}.}
  \item{digits}{The minimum number of significant digits to be printed in values.
  Default = \code{max(3, getOption("digits") - 3)}.} 
  \item{\dots}{Currently not used.} 
}

\details{
When \code{tech.change} is set to \code{FALSE}, this overrides the effect of \code{tech.reg}. 

Setting \code{scaled = FALSE} (no rescaling of data) may lead to numerical problems in solving LP 
problems while optimizing DEA models. In extreme cases it may also prevent models from being optimized.

The Fisher index is not transitive and therefore each firm is compared to itself in the previous period. 
Since there is no previous period for the first period, the results for this first period are replaced by \code{NA}. 
}

\value{
\code{fisher()} returns a list of class \verb{'Fisher'} for which a summary of productivity and profitability 
measures in levels and changes, as well as a summary shadow prices (if \code{shadow = TRUE}), is printed.

This list contains the following items:

\item{Levels}{Several elements are provided, depending on the \code{orientation} specified:
  \tabular{ll}{
    \code{REV} \tab Revenues\cr
    \code{COST} \tab Costs\cr 
    \code{PROF} \tab Profitability\cr
    \code{P} \tab Aggregated output prices\cr
    \code{W} \tab Aggregated input prices\cr
    \code{TT} \tab Terms of trade (i.e. \code{P/W})\cr
    \code{AO} \tab Aggregated outputs\cr
    \code{AI} \tab Aggregated inputs\cr
    \code{TFP} \tab Total Factor Productivity (TFP)\cr
    \code{MP} \tab Maximum productivity\cr
    \code{TFPE} \tab TFP efficiency score\cr
    \code{OTE} \tab Output-oriented technical efficiency score \emph{(\code{orientation = "out"})}\cr
    \code{OSE} \tab Output-oriented scale efficiency score \emph{(\code{orientation = "out"})}\cr
    \code{RAE} \tab Revenue allocative efficiency \emph{(\code{orientation = "out"})}\cr
               \tab \code{    } \emph{(equivalent to output-oriented mix efficiency score)}\cr
    \code{ROSE} \tab Residual output-oriented scale efficiency score \emph{(\code{orientation = "out"})}\cr
    \code{OSME} \tab Output-oriented scale-mix efficiency score \emph{(\code{orientation = "out"})}\cr
    \code{ITE} \tab Input-oriented technical efficiency score \emph{(\code{orientation = "in"})}\cr
    \code{ISE} \tab Input-oriented scale efficiency score \emph{(\code{orientation = "in"})}\cr
    \code{CAE} \tab Cost allocative efficiency \emph{(\code{orientation = "in"})}\cr
               \tab \code{    } \emph{(equivalent to input-oriented mix efficiency score)}\cr
    \code{RISE} \tab Residual input-oriented scale efficiency score \emph{(\code{orientation = "in"})}\cr
    \code{ISME} \tab Input-oriented scale-mix efficiency score \emph{(\code{orientation = "in"})}\cr
    \code{OTE.ITE} \tab Geometric mean of \code{OTE} and \code{ITE} \emph{(\code{orientation = "in-out"})}\cr
    \code{OSE.ISE} \tab Geometric mean of \code{OSE} and \code{ISE} \emph{(\code{orientation = "in-out"})}\cr
    \code{RAE.CAE} \tab Geometric mean of \code{RAE} and \code{CAE} \emph{(\code{orientation = "in-out"})}\cr
    \code{ROSE.RISE} \tab Geometric mean of \code{ROSE} and \code{RISE} \emph{(\code{orientation = "in-out"})}\cr
    \code{OSME.ISME} \tab Geometric mean of \code{OSME} and \code{ISME} \emph{(\code{orientation = "in-out"})}\cr
    \code{RME} \tab Residual mix efficiency score \cr
    \code{RE} \tab Revenue efficiency \emph{(\code{orientation = "out"})}\cr
    \code{CE} \tab Cost efficiency \emph{(\code{orientation = "in"})}\cr
    \code{RE.CE} \tab Geometric mean of \code{RE} and \code{CE} \emph{(\code{orientation = "in-out"})}}}

\item{Changes}{Change indices of the different elements of \code{Levels} are provided. 
Each change is prefixed by \code{"d"} (e.g. profitability change is denoted \code{dPROF}, 
output-oriented efficiency change is denoted \code{dOTE}, etc.). 
Each firm is compared to itself in the previous period. Since there is no previous period for the 
first period, the results for this first period are replaced by \code{NA}.}

\item{Shadowp}{Returned only if \code{shadow = TRUE}. It contains the deflated cost input (\code{x.vars}) shadow prices and the 
deflated revenue output (\code{y.vars}) shadow prices.\cr}

From an object of class \verb{'Fisher'} obtained from \code{fisher()}, the 
\itemize{
\item \code{\link{Levels}} function extracts individual productivity and profitability \bold{levels};
\item \code{\link{Changes}} function extracts individual productivity and profitability \bold{change indices}; and 
\item If \code{shadow = TRUE}, the \code{\link{Shadowp}} function extracts individual input and output \bold{deflated shadow prices}.}
}

\references{
Diewert W.E. (1992), Fisher ideal output, input, and productivity indexes revisited. 
\emph{Journal of Productivity Analysis}, \bold{3}(3), 211-248.
\url{https://doi.org/10.1007/BF00158354}

Coelli T.J., D.S.P. Rao, C.J. O'Donnell, and G.E. Battese (2005), 
\emph{An Introduction to Efficiency and Productivity Analysis}. Springer Eds.

O'Donnell C.J. (2011), The sources of productivity change in the manufacturing 
sectors of the U.S. economy. School of Economics, University of Queensland, Australia.
URL: \url{http://www.uq.edu.au/economics/cepa/docs/WP/WP072011.pdf }
}

\author{K Hervé Dakpo, Yann Desjeux, Laure Latruffe}

\note{All output-oriented efficiency scores are computed \emph{a la} Shephard, while all 
input-oriented efficiency scores are computed \emph{a la} Farrell.
Hence, all efficiency scores are greater than zero and are lower or equal to one.}

\section{Warning}{The \code{fisher()} function will not work with unbalanced panel data.

The Fisher index may be sensitive to the rescaling.

For extreme efficient observations, the problem of multiple solutions may arise and the values of 
shadow prices may differ depending on the linear programming solver used (here \pkg{lpSolveAPI}).}

\seealso{See \code{\link{Levels}} to retrieve a data frame with individual Fisher 
productivity and profitability in levels and components.\cr
See \code{\link{Changes}} to retrieve a data frame with individual Fisher 
productivity and profitability changes and components.\cr
See \code{\link{Shadowp}} to retrieve individual deflated input and output shadow prices, provided that \code{shadow = TRUE}.\cr

See also \code{\link{laspeyres}} and \code{\link{paasche}} for computations with alternative indices.}

\examples{

## Fisher profitability and productivity levels and changes' computations
\dontrun{
  Fisher.prod <- fisher(data = usagri, id.var = "States", time.var = "Years", x.vars = c(7:10), 
  y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13), orientation = "out")
  Fisher.prod
}
}

\keyword{models}
