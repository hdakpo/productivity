\name{hicksmoorsteen}
\alias{hicksmoorsteen}
\alias{print.HicksMoorsteen}
\title{Hicks-Moorsteen productivity and profitability index}
\description{Using Data Envelopment Analysis (DEA), this function measures 
productivity and profitability in levels and changes with Hicks-Moorsteen index.

The Hicks-Moorsteen index is the geometric average of its components, i.e. 
Malmquist-\emph{hs} and Malmquist-\emph{it} indices.

Deflated shadow prices of inputs and outputs used to compute Malmquist-\emph{hs} and 
Malmquist-\emph{it} indices can also be returned.}

\usage{hicksmoorsteen(data, id.var, time.var, x.vars, y.vars, w.vars = NULL, p.vars = NULL, 
  tech.change = TRUE, tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), 
  orientation = c("out", "in", "in-out"), parallel = FALSE, cores = max(1, 
  detectCores() - 1), scaled = TRUE, components = FALSE)

\method{print}{HicksMoorsteen}(x, digits = NULL, \dots)}

\arguments{
  \item{data}{A dataframe containing the required information for measuring productivity and 
  profitability.}
  \item{id.var}{Firms' ID variable. Can be an integer or a text string.}
  \item{time.var}{Time period variable. Can be an integer or a text string.}
  \item{x.vars}{Input quantity variables. Can be a vector of text strings or integers.}
  \item{y.vars}{Output quantity variables. Can be a vector of text strings or integers.}
  \item{w.vars}{Input price variables (Optional). Can be a vector of text strings or integers. 
  \code{NULL} by default, and in this case productivity only is measured.}
  \item{p.vars}{Output price variables (Optional). Can be a vector of text strings or integers. 
  \code{NULL} by default, and in this case productivity only is measured.}
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
  \item{components}{Logical. Default is \code{FALSE} (only Hicks-Moorsteen indices are returned). When set to \code{TRUE}, 
  the components Malmquist-\emph{hs} and Malmquist-\emph{it} indices are also returned (in terms of levels, changes, along 
  with shadow prices used to compute Malmquist-\emph{hs} and Malmquist-\emph{it}).}
  \item{x}{An object of class \verb{'HicksMoorsteen'}.}
  \item{digits}{The minimum number of significant digits to be printed in values.
  Default = \code{max(3, getOption("digits") - 3)}.} 
  \item{\dots}{Currently not used.} 
}

\details{
The Hicks-Moorsteen index is the geometric average of Malmquist-\emph{hs} and Malmquist-\emph{it} indices.
For a firm \emph{i} Malmquist-\emph{it} computes the productivity index based on the reference year \emph{t}.
For a firm \emph{h}, Malmquist-\emph{hs} computes the productivity index based on the reference year \emph{s} (i.e. \emph{t-1}).
Therefore, the Malmquist-\emph{it} index uses the current period shadow prices as aggregators, while the Malmquist-\emph{hs} index uses the 
previous period shadow prices as aggregators.

When \code{tech.change} is set to \code{FALSE}, this overrides the effect of \code{tech.reg}. 

Setting \code{scaled = FALSE} (no rescaling of data) may lead to numerical problems in solving LP 
problems while optimizing DEA models. In extreme cases it may also prevent models from being optimized.

The Hicks-Moorsteen index is not transitive and therefore each firm is compared to itself in the previous period. 
Since there is no previous period for the first period, the results for this first period are replaced by \code{NA}. 
}

\value{
\code{hicksmoorsteen()} returns a list of class \verb{'HicksMoorsteen'} for which a summary of productivity and profitability 
(when price information is specified) measures in levels and changes is printed.

This list contains the following elements:

-- \bold{HicksMoorsteen}, containing levels and changes related to Hick-Moorsteen index per-se, with:
  \item{Levels}{Several elements are provided, depending on the \code{orientation} specified:
    \tabular{ll}{
      \code{REV} \tab Revenues \emph{(when \code{w.vars} and \code{p.vars} are specified)}\cr
      \code{COST} \tab Costs \emph{(when \code{w.vars} and \code{p.vars} are specified)}\cr
      \code{PROF} \tab Profitability (\emph{when \code{w.vars} and \code{p.vars} are specified)}\cr
      \code{P} \tab Aggregated output prices \emph{(when \code{w.vars} and \code{p.vars} are specified)}\cr
      \code{W} \tab Aggregated input prices \emph{(when \code{w.vars} and \code{p.vars} are specified)}\cr
      \code{TT} \tab Terms of trade (i.e. \code{P/W}) \emph{(when \code{w.vars} and \code{p.vars} are specified)}\cr
      \code{AO} \tab Aggregated outputs\cr
      \code{AI} \tab Aggregated inputs\cr
      \code{TFP} \tab Total Factor Productivity (TFP)\cr
      \code{MP} \tab Maximum productivity\cr
      \code{TFPE} \tab TFP efficiency score\cr
      \code{OTE} \tab Output-oriented technical efficiency score \emph{(\code{orientation = "out"})}\cr
      \code{OSE} \tab Output-oriented scale efficiency score \emph{(\code{orientation = "out"})}\cr
      \code{OME} \tab Output-oriented mix efficiency score \emph{(\code{orientation = "out"})}\cr
      \code{ROSE} \tab Residual output-oriented scale efficiency score \emph{(\code{orientation = "out"})}\cr
      \code{OSME} \tab Output-oriented scale-mix efficiency score \emph{(\code{orientation = "out"})}\cr
      \code{ITE} \tab Input-oriented technical efficiency score \emph{(\code{orientation = "in"})}\cr
      \code{ISE} \tab Input-oriented scale efficiency score \emph{(\code{orientation = "in"})}\cr
      \code{IME} \tab Input-oriented mix efficiency score \emph{(\code{orientation = "in"})}\cr
      \code{RISE} \tab Residual input-oriented scale efficiency score \emph{(\code{orientation = "in"})}\cr
      \code{ISME} \tab Input-oriented scale-mix efficiency score \emph{(\code{orientation = "in"})}\cr
      \code{OTE.ITE} \tab Geometric mean of \code{OTE} and \code{ITE} \emph{(\code{orientation = "in-out"})}\cr
      \code{OSE.ISE} \tab Geometric mean of \code{OSE} and \code{ISE} \emph{(\code{orientation = "in-out"})}\cr
      \code{OME.IME} \tab Geometric mean of \code{OME} and \code{IME} \emph{(\code{orientation = "in-out"})}\cr
      \code{ROSE.RISE} \tab Geometric mean of \code{ROSE} and \code{RISE} \emph{(\code{orientation = "in-out"})}\cr
      \code{OSME.ISME} \tab Geometric mean of \code{OSME} and \code{ISME} \emph{(\code{orientation = "in-out"})}\cr
      \code{RME} \tab Residual mix efficiency score}}

  \item{Changes}{Change indices of the different elements of \code{Levels} are provided. Each change is prefixed by 
   \code{"d"} (e.g. profitability change is denoted \code{dPROF}, output-oriented efficiency change is denoted \code{dOTE}, etc.).\cr}

-- \bold{MalmquistHS}, only returned when \code{components = TRUE} and accessible using \code{\link{Levels}}, \code{\link{Changes}}, 
and \code{\link{Shadowp}}, containing levels, changes, and shadow prices related to Malmquist-\emph{hs} index, with: 
  \item{Levels}{Several elements are provided, depending on the \code{orientation} specified.}
  \item{Changes}{Change indices of the different elements of \code{Levels}.}
  \item{Shadowp}{For each observation, input (\code{x.vars}) and output (\code{y.vars}) deflated shadow prices used to compute 
  Malmquist-\emph{hs} index are returned.\cr}

\bold{-- MalmquistIT}, only returned when \code{components = TRUE} and accessible using \code{\link{Levels}}, \code{\link{Changes}}, 
and \code{\link{Shadowp}}, containing levels, changes, and shadow prices related to Malmquist-\emph{it} index, with: 
  \item{Levels}{Several elements are provided, depending on the \code{orientation} specified.}
  \item{Changes}{Change indices of the different elements of \code{Levels} are provided.}
  \item{Shadowp}{For each observation, input (\code{x.vars}) and output (\code{y.vars}) deflated shadow prices used to compute 
  Malmquist-\emph{it} index are returned.\cr}


From an object of class \verb{'HicksMoorsteen'} obtained from \code{hicksmoorsteen()}, the 
\itemize{
\item \code{\link{Levels}} function extracts individual Hicks-Moorsteen productivity and profitability \bold{levels};
\item \code{\link{Changes}} function extracts individual Hicks-Moorsteen productivity and profitability \bold{change indices}; and 
\item \code{\link{Shadowp}} function extracts individual input and output \bold{deflated shadow prices} of Malmquist-\emph{hs} 
and Malmquist-\emph{it} indices, when \code{components = TRUE}.}
}

\references{

Briec W., and Kerstens K. (2011). The Hicks-Moorsteen Productivity Index Satisfies the 
Determinateness Axiom. \emph{The Manchester School}, \bold{79}(4), 765--775. 
\url{https://doi.org/10.1111/j.1467-9957.2010.02169.x}

Caves D.W., Christensen L.R., and Diewert W.E.(1982). The Economic Theory of Index Numbers 
and the Measurement of Input, Output, and Productivity. \emph{Econometrica}, \bold{50}(6), 1393--1414. 
URL: \url{http://www.jstor.org/stable/1913388}

O'Donnell C.J. (2008), An aggregate quantity-price framework for measuring 
and decomposing productivity and profitability change. School of Economics, 
University of Queensland, Australia.
URL: \url{https://www.uq.edu.au/economics/cepa/docs/WP/WP072008.pdf}

O'Donnell C.J. (2010). Measuring and decomposing agricultural productivity and profitability 
change. \emph{Australian Journal of Agricultural and Resource Economics}, \bold{54}(4), 527--560.
\url{https://doi.org/10.1111/j.1467-8489.2010.00512.x}

O'Donnell C.J. (2011), The sources of productivity change in the manufacturing 
sectors of the U.S. economy. School of Economics, University of Queensland, Australia.
URL: \url{http://www.uq.edu.au/economics/cepa/docs/WP/WP072011.pdf}
}

\author{K Hervé Dakpo, Yann Desjeux, Laure Latruffe}

\note{All output-oriented efficiency scores are computed \emph{a la} Shephard, while all input-oriented efficiency 
scores are computed \emph{a la} Farrell. Hence, all efficiency scores are greater than zero and are lower or equal to one.}

\section{Warning}{The \code{hicksmoorsteen()} function will not work with unbalanced panel data.

The Hicks-Moorsteen index may be sensitive to the rescaling.

The productivity levels are obtained using shadow prices computed using dual (multipliers) DEA models. However, for extreme 
efficient observations the issue of multiple solutions may arise and the values of shadow prices may differ depending on the 
linear programming solver used (here \pkg{lpSolveAPI}).}

\seealso{See \code{\link{Levels}} to retrieve Hicks-Moorsteen (along with Malmquist-\emph{hs} and Malmquist-\emph{it}) productivity 
and profitability in levels and components.\cr
See \code{\link{Changes}} to retrieve Hicks-Moorsteen (along with Malmquist-\emph{hs} and Malmquist-\emph{it}) productivity 
and profitability changes and components.\cr
See \code{\link{Shadowp}} to retrieve deflated input and output shadow prices of Malmquist-\emph{hs} and Malmquist-\emph{it}.}

\examples{

## Hicks-Moorsteen productivity, without price information
\dontrun{
  Hicks1 <- hicksmoorsteen(data = usagri, id.var = "States", time.var = "Years", x.vars = c(7:10), 
  y.vars = c(4:6), rts = "crs", orientation = "in")
  Hicks1
}
## Hicks-Moorsteen productivity and profitability, with price information
\dontrun{
Hicks2 <- hicksmoorsteen(data = usagri, id.var = "States", time.var = "Years", 
  x.vars = c(7:10), y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13))
  Hicks2
}
}

\keyword{models}
