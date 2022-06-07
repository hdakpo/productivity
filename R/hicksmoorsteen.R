################################################################################
#                                                                              #
# R functions for the productivity package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Hicks-Moorsteen productivity index (hicksmoorsteen)                          #
#------------------------------------------------------------------------------#

#' Hicks-Moorsteen productivity index
#'
#' Using Data Envelopment Analysis (DEA), this function measures productivity
#' in levels and changes with Hicks-Moorsteen index.
#'
#' Profitability index is also returned when price information is specified.
#'
#' The Hicks-Moorsteen index is the geometric average of its components, i.e.
#' Malmquist-\emph{hs} and Malmquist-\emph{it} indices.
#'
#' Deflated shadow prices of inputs and outputs used to compute
#' Malmquist-\emph{hs} and Malmquist-\emph{it} indices can also be returned.
#'
#' @aliases hicksmoorsteen print.HicksMoorsteen
#'
#' @param data A dataframe containing the required information for measuring
#' productivity and profitability.
#' @param id.var Firms' ID variable. Can be an integer or a text string.
#' @param time.var Time period variable. Can be an integer or a text string.
#' @param x.vars Input quantity variables. Can be a vector of text strings or
#' integers.
#' @param y.vars Output quantity variables. Can be a vector of text strings or
#' integers.
#' @param w.vars Input price variables (Optional). Can be a vector of text
#' strings or integers.
#' @param p.vars Output price variables (Optional). Can be a vector of text
#' strings or integers.
#' @param tech.change Logical. If \code{TRUE} (default), the model allows for
#' technological change. If \code{FALSE}, technological change is prohibited.
#' See also the \code{Details} section.
#' @param tech.reg Logical. If \code{TRUE} (default), the model allows for
#' negative technological change (i.e. technological regress). If \code{FALSE},
#' only positive technological change (i.e. technological progress) is allowed.
#' See also the \code{Details} section.
#' @param rts Character string specifying the returns to scale assumption.
#' The default value is \code{"vrs"} (variable returns to scale). Other possible
#' options are \code{"crs"} (constant returns to scale), \code{"nirs"}
#' (non-increasing returns to scale), or \code{"ndrs"} (non-decreasing returns
#' to scale).
#' @param orientation Character string specifying the orientation.
#' The default value is \code{"out"} (output-orientation). Other possible
#' options are \code{"in"} (input-orientation), and \code{"in-out"}
#' (both input- and output-orientations). For \code{"in-out"}, the geometric
#' mean of input- and output-orientations' results is returned.
#' @param parallel Logical. Allows parallel computation. If \code{FALSE}
#' (default), the estimation is conducted in sequential mode. If \code{TRUE},
#' parallel mode is activated using the number of cores specified in
#' \code{cores}. When the sample size is small, it is recommended to keep the
#' \code{parallel} option to its default value (\code{FALSE}).
#' @param cores Integer. Used only if \code{parallel = TRUE}. It specifies the
#' number of cores to be used for parallel computation. By default,
#' \code{cores = max(1, detectCores() - 1)}.
#' @param scaled Logical. If \code{TRUE} (default), input and output quantities
#' are rescaled. If \code{FALSE}, a warning message is displayed when very large
#' (>1e5) and/or very small (<1e-4) values are present in the input and output
#' quantity variables. See also the \code{Details} section.
#' @param by.id Integer specifying the reference observation used for computing
#' the indices (Optional). \code{by.id} must range between one and the total
#' number of firms per period. See also the \code{Details} section.
#' @param by.year Integer specifying the reference year used for computing the
#' indices (Optional). \code{by.year} must range between one and the total
#' number of time periods. See also the \code{Details} section.
#' @param shadow Logical. Default is \code{FALSE} (no shadow prices are
#' returned). When set to \code{TRUE}, the deflated input and output shadow
#' prices of the 'representative observation' (i.e. the sample means of
#' quantities and prices) used to compute the Hicks-Moorsteen index are returned.
#' @param components Logical. Default is \code{FALSE} (only Hicks-Moorsteen
#' indices are returned). When set to \code{TRUE}, the components
#' Malmquist-\emph{hs} and Malmquist-\emph{it} indices are also returned
#' (in terms of levels, changes, along with shadow prices used to compute
#' Malmquist-\emph{hs} and Malmquist-\emph{it}).
#' @param x an object of class \code{HicksMoorsteen} (returned by the function
#' \code{\link{hicksmoorsteen}}).
#' @param digits The minimum number of significant digits to be printed in
#' values. Default = \code{max(3, getOption("digits") - 3)}.
#' @param ... additional arguments of the print, Levels, Changes, Shadowp
#' are currently ignored.
#'
#' @details
#'
#' The Hicks-Moorsteen index is the geometric average of Malmquist-\emph{hs}
#' and Malmquist-\emph{it} indices.
#'
#' For a firm \emph{i} Malmquist-\emph{it} computes the productivity index based
#' on the reference year \emph{t}. For a firm \emph{h}, Malmquist-\emph{hs}
#' computes the productivity index based on the reference year
#' \emph{s} (i.e. \emph{t-1}). Therefore, the Malmquist-\emph{it} index uses the
#' current period shadow prices as aggregators, while the Malmquist-\emph{hs}
#' index uses the previous period shadow prices as aggregators.
#'
#' When \code{tech.change} is set to \code{FALSE}, this overrides the effect
#' of \code{tech.reg}.
#'
#' Setting \code{scaled = FALSE} (no rescaling of data) may lead to numerical
#' problems in solving LP problems while optimizing DEA models. In extreme cases
#' it may also prevent models from being optimized.
#'
#' By default \code{by.id = NULL} and \code{by.year = NULL}. This means that in
#' the computation of change indices, each observation is by default compared to
#' itself in the first period. \code{by.id} and \code{by.year} allow to specify
#' a  reference (e.g. a specific observation in a specific period). If
#' \code{by.id} is specified and \code{by.year = NULL}, then the reference
#' observation is \code{by.id} in the first period. If \code{by.year} is
#' specified and \code{by.id = NULL}, then each observation is compared to
#' itself in the specified period of time.
#'
#' The Hicks-Moorsteen index is a fixed-weights-based TFP index as the Lowe.
#' The Hicks-Moorsteen index computes in a first step deflated input and output
#' prices for a representative observation (sample mean) and in a second step,
#' these prices are use as aggregators.
#'
#' A set of extractor functions for objects of class \code{'HicksMoorsteen'}
#' including methods to the generic functions
#' \code{\link[=print.HicksMoorsteen]{print}},
#' \code{\link[=Levels.HicksMoorsteen]{Levels}},
#' \code{\link[=Changes.HicksMoorsteen]{Changes}}, and
#' \code{\link[=Shadowp.HicksMoorsteen]{Shadowp}}.
#'
#' @return \code{hicksmoorsteen()} returns a list of class \verb{'HicksMoorsteen'}
#' that contains productivity and profitability (when price information
#' is specified) measures in levels and changes, as well as inputs and outputs
#' deflated shadow prices (if \code{shadow = TRUE}).
#'
#' This list contains the following elements:
#'
#' -- \bold{HicksMoorsteen}, containing levels and changes related to
#' Hick-Moorsteen index per-se, with:
#'
#' \item{Levels}{Several elements are provided, depending on the
#' \code{orientation} specified:
#'
#' \tabular{ll}{
#' \code{REV} \tab Revenues \emph{(when \code{w.vars} and \code{p.vars} are
#' specified)}\cr
#' \code{COST} \tab Costs \emph{(when \code{w.vars} and \code{p.vars} are
#' specified)}\cr
#' \code{PROF} \tab Profitability \emph{(when \code{w.vars} and \code{p.vars}
#' are specified)}\cr
#' \code{P} \tab Aggregated output prices \emph{(when \code{w.vars} and
#' \code{p.vars} are specified)}\cr
#' \code{W} \tab Aggregated input prices \emph{(when \code{w.vars} and
#' \code{p.vars} are specified)}\cr
#' \code{TT} \tab Terms of trade (i.e. \code{P/W}) \emph{(when \code{w.vars} and
#' \code{p.vars} are specified)}\cr
#' \code{AO} \tab Aggregated outputs\cr
#' \code{AI} \tab Aggregated inputs\cr
#' \code{TFP} \tab Total Factor Productivity (TFP)\cr
#' \code{MP} \tab Maximum productivity\cr
#' \code{TFPE} \tab TFP efficiency score\cr
#' \code{OTE} \tab Output-oriented technical efficiency score
#' \emph{(\code{orientation = "out"})}\cr
#' \code{OSE} \tab Output-oriented scale efficiency score
#' \emph{(\code{orientation = "out"})}\cr
#' \code{OME} \tab Output-oriented mix efficiency score
#' \emph{(\code{orientation = "out"})}\cr
#' \code{ROSE} \tab Residual output-oriented scale efficiency score
#' \emph{(\code{orientation = "out"})}\cr
#' \code{OSME} \tab Output-oriented scale-mix efficiency score
#' \emph{(\code{orientation = "out"})}\cr
#' \code{ITE} \tab Input-oriented technical efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{ISE} \tab Input-oriented scale efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{IME} \tab Input-oriented mix efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{RISE} \tab Residual input-oriented scale efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{ISME} \tab Input-oriented scale-mix efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{OTE.ITE} \tab Geometric mean of \code{OTE} and \code{ITE}
#' \emph{(\code{orientation = "in-out"})}\cr
#' \code{OSE.ISE} \tab Geometric mean of \code{OSE} and \code{ISE}
#' \emph{(\code{orientation = "in-out"})}\cr
#' \code{OME.IME} \tab Geometric mean of \code{OME} and \code{IME}
#' \emph{(\code{orientation = "in-out"})}\cr
#' \code{ROSE.RISE} \tab Geometric mean of \code{ROSE} and \code{RISE}
#' \emph{(\code{orientation = "in-out"})}\cr
#' \code{OSME.ISME} \tab Geometric mean of \code{OSME} and \code{ISME}
#' \emph{(\code{orientation = "in-out"})}\cr
#' \code{RME} \tab Residual mix efficiency score}}
#'
#' \item{Changes}{Change indices of the different elements of \code{Levels} are
#' provided. Each change is prefixed by \code{"d"} (e.g. profitability change is
#' denoted \code{dPROF}, output-oriented efficiency change is denoted
#' \code{dOTE}, etc.).}
#'
#' -- \bold{MalmquistHS}, only returned when \code{components = TRUE} and
#' accessible using \code{\link{Levels}}, \code{\link{Changes}}, and
#' \code{\link{Shadowp}}, containing levels, changes, and shadow prices related
#' to Malmquist-\emph{hs} index, with:
#'  \item{Levels}{Several elements are provided, depending on the
#'  \code{orientation} specified.}
#'  \item{Changes}{Change indices of the different elements of \code{Levels}.}
#'  \item{Shadowp}{For each observation, input (\code{x.vars}) and output
#'  (\code{y.vars}) deflated shadow prices used to compute Malmquist-\emph{hs}
#'  index are returned.\cr}
#'
#'  \bold{-- MalmquistIT}, only returned when \code{components = TRUE} and
#'  accessible using \code{\link{Levels}}, \code{\link{Changes}}, and
#'  \code{\link{Shadowp}}, containing levels, changes, and shadow prices related
#'  to Malmquist-\emph{it} index, with:
#'  \item{Levels}{Several elements are provided, depending on the
#'  \code{orientation} specified.}
#'  \item{Changes}{Change indices of the different elements of \code{Levels}
#'  are provided.}
#'  \item{Shadowp}{For each observation, input (\code{x.vars}) and output
#'  (\code{y.vars}) deflated shadow prices used to compute Malmquist-\emph{it}
#'  index are returned.\cr}
#'
#' @note All output-oriented efficiency scores are computed \emph{a la} Shephard,
#' while all input-oriented efficiency scores are computed \emph{a la} Farrell.
#' Hence, all efficiency scores are greater than zero and are lower or equal to
#' one.
#'
#' @author K Hervé Dakpo, Yann Desjeux, Laure Latruffe, Stefan Wimmer
#'
#' @section Warning:
#' The \code{hicksmoorsteen()} function will not work with
#' unbalanced panel data. The Hicks-Moorsteen index may be sensitive to the
#' rescaling.
#'
#' For extreme efficient observations, the problem of multiple solutions may
#' arise and the values of shadow prices may differ depending on the linear
#' programming solver used (here \pkg{lpSolveAPI}).
#'
#' @seealso
#' \code{\link[=print.HicksMoorsteen]{print}}: for printing summary of each
#' element of the list in the \code{'HicksMoorsteen'} object;
#'
#' \code{\link[=Levels.HicksMoorsteen]{Levels}}:  to retrieve
#' Hicks-Moorsteen (along with Malmquist-\emph{hs} and Malmquist-\emph{it})
#' productivity and profitability in levels and components;
#'
#' \code{\link[=Changes.HicksMoorsteen]{Changes}}: to retrieve
#' Hicks-Moorsteen (along with Malmquist-\emph{hs} and Malmquist-\emph{it})
#' productivity and profitability changes and components;
#'
#' \code{\link[=Shadowp.HicksMoorsteen]{Shadowp}}: to retrieve deflated
#' input and output shadow prices of Malmquist-\emph{hs} and Malmquist-\emph{it}.
#'
#' @references Briec W., and Kerstens K. (2011). The Hicks-Moorsteen
#' Productivity Index Satisfies the Determinateness Axiom.
#' \emph{The Manchester School}, \bold{79}(4), 765--775.
#' \url{https://doi.org/10.1111/j.1467-9957.2010.02169.x}
#'
#' Caves D.W., Christensen L.R., and Diewert W.E.(1982). The Economic Theory of
#' Index Numbers and the Measurement of Input, Output, and Productivity.
#' \emph{Econometrica}, \bold{50}(6), 1393--1414.
#' URL: \url{http://www.jstor.org/stable/1913388}
#'
#' O'Donnell C.J. (2008), An aggregate quantity-price framework for
#' measuring and decomposing productivity and profitability change. School of
#' Economics, University of Queensland, Australia.
#' URL: \url{https://www.uq.edu.au/economics/cepa/docs/WP/WP072008.pdf }
#'
#' O'Donnell C.J. (2010). Measuring and decomposing agricultural productivity
#' and profitability change. \emph{Australian Journal of Agricultural and
#' Resource Economics}, \bold{54}(4), 527--560.
#' \url{https://doi.org/10.1111/j.1467-8489.2010.00512.x}
#'
#' O'Donnell C.J. (2011), The sources of productivity change in the
#' manufacturing sectors of the U.S. economy. School of Economics, University of
#' Queensland, Australia.
#' URL: \url{http://www.uq.edu.au/economics/cepa/docs/WP/WP072011.pdf }
#'
#' @keywords models optimize
#'
#' @examples
#' ## Hicks-Moorsteen productivity, without price information
#' \dontrun{
#' Hicks1 <- hicksmoorsteen(data = usagri, id.var = "States", time.var = "Years",
#' x.vars = c(7:10), y.vars = c(4:6), rts = "crs", orientation = "in")
#' Hicks1
#' }
#' ## Hicks-Moorsteen productivity and profitability, with price information
#' \dontrun{
#' Hicks2 <- hicksmoorsteen(data = usagri, id.var = "States", time.var = "Years",
#' x.vars = c(7:10), y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13))
#' Hicks2
#' }
#' @export
# Hicks-Moorsteen productivity index ----------
hicksmoorsteen <- function(data, id.var, time.var, x.vars, y.vars, w.vars = NULL, p.vars = NULL, tech.change = TRUE,
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"), parallel = FALSE,
  cores = max(1, detectCores() - 1), scaled = TRUE, components = FALSE) {
  step1 <- check.1(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  if (!balanced(data = data, id.var = id.var, time.var = time.var))
    stop("Hicks-Moorsteen index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
  rts <- match.arg(rts)
  RTS <- c("vrs", "crs", "nirs", "ndrs")
  if (!(rts %in% RTS))
    stop("Unknown scale of returns: ", paste(rts), call. = FALSE)
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in", "in-out")
  if (!(orientation %in% ORIENTATION))
    stop("Unknown orientation: ", paste(orientation), call. = FALSE)
  data <- data[order(data[, step1$time.var], data[, step1$id.var]), ]
  year.vec <- unique(data[, time.var])
  if (scaled == FALSE) {
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 | data[, c(step1$x.vars, step1$y.vars)] <= 1e-04))
      warning("Some quantity variables are not between 1e-4 and 1e5.
We recommend rescaling the data or set the scaled option to TRUE to avoid numerical problems\n\r", call. = FALSE)
    mean.y <- 1
    mean.x <- 1
  } else {
    mean.y <- if (length(y.vars) == 1) mean(data[, step1$y.vars]) else apply(data[, step1$y.vars], 2, FUN = mean)
    mean.x <- if (length(x.vars) == 1) mean(data[, step1$x.vars]) else apply(data[, step1$x.vars], 2, FUN = mean)
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)], 2, FUN = function(x) x/mean(x))
  }
  pas <- 5
  it <- iter(c(paste0("\rProgress: ", seq(0,100-pas,pas), "%\r"), "DONE!        \r\n\r"))
  itt <- round(seq(1, nrow(data), nrow(data)/(100/pas)),0)
  itt[(100/pas)+1L] <- nrow(data)
  if (parallel == TRUE & cores == 1) { parallel <- FALSE }
  if (parallel == TRUE & cores > 1) {
    registerDoParallel(cores = cores)
  } else {
    registerDoSEQ()
  }
  if (tech.change == TRUE) {
    res.hm.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI", "doParallel"),
      .export = c("hm.1", "DO.teseme", "DI.teseme", "D.tfp",
                  "DO.shdu", "DI.shdu")) %dopar% {
      hm.1(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, components, mean.x, mean.y, itt, it)
    }
  } else {
    res.hm.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI", "doParallel"),
      .export = c("hm.2", "DO.teseme", "DI.teseme", "D.tfp",
                  "DO.shdu", "DI.shdu")) %dopar% {
      hm.2(data, step1, ano, year.vec, rts, orientation, parallel, components, mean.x, mean.y, itt, it)
    }
  }
  if (components == FALSE) {
    row.names(res.hm.loop) <- seq(1:dim(res.hm.loop)[1])
    res.hm.loop <- cbind(data[, c(step1$id.var, step1$time.var)], res.hm.loop)
    if ("REV" %in% colnames(res.hm.loop)) {
      indices <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"),
                           .export = "fdiv") %dopar% {
                    cbind(res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("REV", "COST", "PROF")]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano - 1],
                    c("REV", "COST", "PROF")], res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("P", "W")]/(res.hm.loop[res.hm.loop[, 2] == year.vec[ano - 1],
                    c("REV", "COST")]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("Qs", "Xs")]), TT = res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    "TT"]/fdiv(res.hm.loop[res.hm.loop[, 2] == year.vec[ano - 1],
                    c("REV", "COST")]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("Qs", "Xs")]), res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("AO", "AI")]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("Qs", "Xs")], res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("TFP", "MP", "TFPE")]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    c("TFP2", "MP2", "TFPE2")], res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                    14:15]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano - 1], 14:15],
                    res.hm.loop[res.hm.loop[, 2] == year.vec[ano], 16:19]/res.hm.loop[res.hm.loop[,
                    2] == year.vec[ano], 27:30])
                           }
    } else {
      indices <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"),
                           .export = "fdiv") %dopar% {
                             cbind(res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                             c("AO", "AI")]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                             c("Qs", "Xs")], res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                             c("TFP", "MP", "TFPE")]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                             c("TFP2", "MP2", "TFPE2")], res.hm.loop[res.hm.loop[, 2] == year.vec[ano],
                             8:9]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano - 1], 8:9],
                             res.hm.loop[res.hm.loop[, 2] == year.vec[ano], 10:13]/res.hm.loop[res.hm.loop[, 2] == year.vec[ano], 21:24])
                           }
    }
    registerDoSEQ()
    stopImplicitCluster()
    INT <- matrix(nrow = dim(res.hm.loop[res.hm.loop[, 2] == year.vec[1], ])[1], ncol = if (dim(indices)[2] == 11) {
      11
    } else {
      17
    })
    INT <- as.data.frame(INT)
    names(INT) <- names(indices)
    indices <- rbind(INT, indices)
    indices <- cbind(res.hm.loop[, 1:2], indices)
    names(indices)[3:if (dim(indices)[2] == 13) {
      13
    } else {
      19
    }] <- paste0("d", names(res.hm.loop[3:if (dim(indices)[2] == 13) {
      13
    } else {
      19
    }]))
    res.tfp <- list(Levels = res.hm.loop[, if ("REV" %in% colnames(res.hm.loop)) {1:19} else {1:13}], Changes = indices)
    class(res.tfp) <- c("list", "HicksMoorsteen")
  } else {
    if (dim(res.hm.loop)[2] %% 2 != 0) {
      res.mal_hs.loop <- res.hm.loop[, 1:((dim(res.hm.loop)[2]-3)/2 + 3)]
      res.mal_it.loop <- res.hm.loop[, c(1:3, (((dim(res.hm.loop)[2]-3)/2 + 3) + 1):dim(res.hm.loop)[2])]
    } else {
      res.mal_hs.loop <- res.hm.loop[, 1:(dim(res.hm.loop)[2]/2)]
      res.mal_it.loop <- res.hm.loop[, (dim(res.hm.loop)[2]/2 + 1):dim(res.hm.loop)[2]]
    }
  row.names(res.mal_hs.loop) <- seq(1:dim(res.mal_hs.loop)[1])
  res.mal_hs.loop <- cbind(data[, c(step1$id.var, step1$time.var)], res.mal_hs.loop)
  names(res.mal_hs.loop) <- gsub('.hs', "", names(res.mal_hs.loop))
  row.names(res.mal_it.loop) <- seq(1:dim(res.mal_it.loop)[1])
  res.mal_it.loop <- cbind(data[, c(step1$id.var, step1$time.var)], res.mal_it.loop)
  names(res.mal_it.loop) <- gsub('.it', "", names(res.mal_it.loop))
  res.hm.loop <- sqrt(res.mal_hs.loop[, -c(1:2, (dim(res.mal_hs.loop)[2] + 1 -
  length(x.vars) - length(y.vars)):dim(res.mal_hs.loop)[2])] *
    res.mal_it.loop[, -c(1:2, (dim(res.mal_it.loop)[2] + 1 - length(x.vars) -
    length(y.vars)):dim(res.mal_it.loop)[2])])
  row.names(res.hm.loop) <- seq(1:dim(res.hm.loop)[1])
  res.hm.loop <- cbind(data[, c(step1$id.var, step1$time.var)], res.hm.loop)
  if ("REV" %in% colnames(res.mal_hs.loop)) {
    indices.hs <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar%
    {
      cbind(res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.mal_hs.loop[res.mal_hs.loop[,
      2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.mal_hs.loop[res.mal_hs.loop[, 2] ==
      year.vec[ano], c("P", "W")]/(res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano - 1], c("REV",
      "COST")]/res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], c("Qs", "Xs")]), TT = res.mal_hs.loop[res.mal_hs.loop[,
      2] == year.vec[ano], "TT"]/fdiv(res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano - 1],
     c("REV", "COST")]/res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], c("Qs", "Xs")]), res.mal_hs.loop[res.mal_hs.loop[,
     2] == year.vec[ano], c("AO", "AI")]/res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano],
    c("Qs", "Xs")], res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], c("TFP", "MP", "TFPE")]/res.mal_hs.loop[res.mal_hs.loop[,
   2] == year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano],
  14:15]/res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano - 1], 14:15], res.mal_hs.loop[res.mal_hs.loop[,
  2] == year.vec[ano], 16:19]/res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], 27:30])
    }
  } else {
    indices.hs <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar%
    {
      cbind(res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], c("AO", "AI")]/res.mal_hs.loop[res.mal_hs.loop[,
      2] == year.vec[ano], c("Qs", "Xs")], res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano],
      c("TFP", "MP", "TFPE")]/res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], c("TFP2", "MP2",
      "TFPE2")], res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], 8:9]/res.mal_hs.loop[res.mal_hs.loop[,
      2] == year.vec[ano - 1], 8:9], res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[ano], 10:13]/res.mal_hs.loop[res.mal_hs.loop[,
      2] == year.vec[ano], 21:24])
    }
  }
  if ("REV" %in% colnames(res.mal_it.loop)) {
    indices.it <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar%
    {
      cbind(res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.mal_it.loop[res.mal_it.loop[,
      2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.mal_it.loop[res.mal_it.loop[, 2] ==
      year.vec[ano], c("P", "W")]/(res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano - 1], c("REV",
     "COST")]/res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], c("Qs", "Xs")]), TT = res.mal_it.loop[res.mal_it.loop[,
     2] == year.vec[ano], "TT"]/fdiv(res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano - 1],
     c("REV", "COST")]/res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], c("Qs", "Xs")]), res.mal_it.loop[res.mal_it.loop[,
     2] == year.vec[ano], c("AO", "AI")]/res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano],
     c("Qs", "Xs")], res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], c("TFP", "MP", "TFPE")]/res.mal_it.loop[res.mal_it.loop[,
    2] == year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano],
    14:15]/res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano - 1], 14:15], res.mal_it.loop[res.mal_it.loop[,
    2] == year.vec[ano], 16:19]/res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], 27:30])
    }
  } else {
    indices.it <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar%
    {
      cbind(res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], c("AO", "AI")]/res.mal_it.loop[res.mal_it.loop[,
      2] == year.vec[ano], c("Qs", "Xs")], res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano],
      c("TFP", "MP", "TFPE")]/res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], c("TFP2", "MP2",
      "TFPE2")], res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], 8:9]/res.mal_it.loop[res.mal_it.loop[,
      2] == year.vec[ano - 1], 8:9], res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[ano], 10:13]/res.mal_it.loop[res.mal_it.loop[,
      2] == year.vec[ano], 21:24])
    }
  }
  registerDoSEQ()
  stopImplicitCluster()
  #HM
  indices.hm <- sqrt(indices.hs * indices.it)
  INT <- matrix(nrow = dim(res.hm.loop[res.hm.loop[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.hm)[2] ==
      11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.hm)
  indices.hm <- rbind(INT, indices.hm)
  indices.hm <- cbind(res.hm.loop[, 1:2], indices.hm)
  names(indices.hm)[3:if (dim(indices.hm)[2] == 13) {
    13
  } else {
    19
  }] <- paste0("d", names(res.hm.loop[3:if (dim(indices.hm)[2] == 13) {
    13
  } else {
    19
  }]))
  #HS
  INT <- matrix(nrow = dim(res.mal_hs.loop[res.mal_hs.loop[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.hs)[2] ==
    11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.hs)
  indices.hs <- rbind(INT, indices.hs)
  indices.hs <- cbind(res.mal_hs.loop[, 1:2], indices.hs)
  names(indices.hs)[3:if (dim(indices.hs)[2] == 13) {
    13
  } else {
    19
  }] <- paste0("d", names(res.mal_hs.loop[3:if (dim(indices.hs)[2] == 13) {
    13
  } else {
    19
  }]))
  #IT
  INT <- matrix(nrow = dim(res.mal_it.loop[res.mal_it.loop[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.it)[2] ==
    11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.it)
  indices.it <- rbind(INT, indices.it)
  indices.it <- cbind(res.mal_it.loop[, 1:2], indices.it)
  names(indices.it)[3:if (dim(indices.it)[2] == 13) {
    13
  } else {
    19
  }] <- paste0("d", names(res.mal_it.loop[3:if (dim(indices.it)[2] == 13) {
    13
  } else {
    19
  }]))
    Shadowp.hs <- cbind(data[, c(step1$id.var, step1$time.var)], if ("REV" %in% colnames(res.hm.loop)) {
      res.mal_hs.loop[, 31:(dim(res.mal_hs.loop)[2])]
    } else {
      res.mal_hs.loop[, 25:(dim(res.mal_hs.loop)[2])]
    })
    names(Shadowp.hs) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
    Shadowp.it <- cbind(data[, c(step1$id.var, step1$time.var)], if ("REV" %in% colnames(res.hm.loop)) {
      res.mal_it.loop[, 31:(dim(res.mal_it.loop)[2])]
    } else {
      res.mal_it.loop[, 25:(dim(res.mal_it.loop)[2])]
    })
    names(Shadowp.it) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
    res.tfp.hs <- list(Levels = res.mal_hs.loop[, if ("REV" %in% colnames(res.hm.loop)) {1:19} else {1:13}], Changes = indices.hs, Shadowp = Shadowp.hs)
    res.tfp.it <- list(Levels = res.mal_it.loop[, if ("REV" %in% colnames(res.hm.loop)) {1:19} else {1:13}], Changes = indices.it, Shadowp = Shadowp.it)
  res.tfp.hm <- list(Levels = res.hm.loop[, if ("REV" %in% colnames(res.hm.loop)) {1:19} else {1:13}], Changes = indices.hm)
  res.tfp <- list(HicksMoorsteen = res.tfp.hm, MalmquistHS = res.tfp.hs, MalmquistIT = res.tfp.it)
  class(res.tfp) <- c("list", "HicksMoorsteen")
  }
  return(res.tfp)
  }

# Hicks-Moorsteen print fonction ----------
#' @rdname hicksmoorsteen
#' @export
print.HicksMoorsteen <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  if (length(x) == 2) {
    cat("\nHicks-Moorsteen productivity and profitability levels (summary):\n\n")
    print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\nHicks-Moorsteen productivity and profitability changes (summary):\n\n")
    print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n")
    invisible(x)
  } else {
    cat("\nHicks-Moorsteen productivity and profitability levels (summary):\n\n")
    print(summary(x[["HicksMoorsteen"]][["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\nHicks-Moorsteen productivity and profitability changes (summary):\n\n")
    print(summary(x[["HicksMoorsteen"]][["Changes"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n")
    invisible(x)
  }
}

# Return Hicks-Moorsteen productivity and profitability Levels ----------
#' @rdname hicksmoorsteen
#' @export
Levels.HicksMoorsteen <- function(x, ...) {
  if (length(x) == 2) {
    return(x$Levels)
  } else {
    return(lapply(x, function(x) x$Levels))
  }
}

# Return Hicks-Moorsteen productivity and profitability Changes ----------
#' @rdname hicksmoorsteen
#' @export
Changes.HicksMoorsteen <- function(x, ...) {
  if (length(x) == 2) {
    return(x$Changes)
  } else {
    return(lapply(x, function(x) x$Changes))
  }
}

# Return deflated inputs/outputs shadow prices ----------
#' @rdname hicksmoorsteen
#' @export
Shadowp.HicksMoorsteen <- function(x, ...) {
  if (length(x) == 2) {
    stop("No shadow prices are returned in your \"", class(x)[2], "\"", " object.
       Specifying 'components = TRUE' should be considered in the function generating the \"", class(x)[2], "\"", " object.")
  } else {
    List <- lapply(x, function(x) x$Shadowp)
    return(List[!sapply(List,is.null)])
  }
}
