################################################################################
#                                                                              #
# R functions for the productivity package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Lowe productivity index (lowe)                                               #
#------------------------------------------------------------------------------#

#' Lowe productivity index
#'
#' Using Data Envelopment Analysis (DEA), this function measures productivity
#' in levels and changes with Lowe index.
#'
#' Deflated shadow prices of inputs and outputs can also be computed.
#'
#' @aliases lowe print.Lowe
#'
#' @param data A dataframe containing the required information for measuring
#' productivity and profitability.
#' @param id.var Firms' ID variable. Can be an integer or a text string.
#' @param time.var Time period variable. Can be an integer or a text string.
#' @param x.vars Input quantity variables. Can be a vector of text strings or
#' integers.
#' @param y.vars Output quantity variables. Can be a vector of text strings or
#' integers.
#' @param w.vars Input price variables. Can be a vector of text
#' strings or integers.
#' @param p.vars Output price variables. Can be a vector of text
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
#' prices are returned. These shadow prices are only informative and may be
#' sensitive to the linear programming solver used.
#' @param x an object of class\code{Lowe} (returned by the function
#' \code{\link{lowe}}).
#' @param digits The minimum number of significant digits to be printed in
#' values. Default = \code{max(3, getOption("digits") - 3)}.
#' @param ... additional arguments of the print, Levels, Changes, Shadowp
#' are currently ignored.
#'
#' @details
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
#' The Lowe index is a fixed-weights-based TFP index as the Lowe.
#' The Lowe index uses the average observed input and output prices as
#' aggregators.
#'
#' A set of extractor functions for objects of class \code{'Lowe'}
#' including methods to the generic functions
#' \code{\link[=print.Lowe]{print}},
#' \code{\link[=Levels.Lowe]{Levels}},
#' \code{\link[=Changes.Lowe]{Changes}}, and
#' \code{\link[=Shadowp.Lowe]{Shadowp}}.
#'
#' @return \code{lowe()} returns a list of class \verb{'Lowe'}
#' that contains productivity and profitability measures in levels and changes,
#' as well as inputs and outputs deflated shadow prices
#' (if \code{shadow = TRUE}).
#'
#' This list contains the following items:
#'
#' \item{Levels}{Several elements are provided, depending on the
#' \code{orientation} specified:
#'
#' \tabular{ll}{
#' \code{REV} \tab Revenues \cr
#' \code{COST} \tab Costs \cr
#' \code{PROF} \tab Profitability \cr
#' \code{P} \tab Aggregated output prices \cr
#' \code{W} \tab Aggregated input prices \cr
#' \code{TT} \tab Terms of trade (i.e. \code{P/W}) \cr
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
#' \code{RME} \tab Residual mix efficiency score \cr
#' \code{RE} \tab Revenue efficiency \emph{(\code{orientation = "out"})}\cr
#' \code{CE} \tab Cost efficiency \emph{(\code{orientation = "in"})}\cr
#' \code{RE.CE} \tab Geometric mean of \code{RE} and \code{CE}
#' \emph{(\code{orientation = "in-out"})}}}
#'
#' \item{Changes}{Change indices of the different elements of \code{Levels} are
#' provided. Each change is prefixed by \code{"d"} (e.g. profitability change is
#' denoted \code{dPROF}, output-oriented efficiency change is denoted
#' \code{dOTE}, etc.).}
#'
#' \item{Shadowp}{Returned only if \code{shadow = TRUE}. It contains the
#' deflated input and output shadow prices.\cr}
#'
#' @note All output-oriented efficiency scores are computed \emph{a la} Shephard,
#' while all input-oriented efficiency scores are computed \emph{a la} Farrell.
#' Hence, all efficiency scores are greater than zero and are lower or equal to
#' one.
#'
#' @author K Hervé Dakpo, Yann Desjeux, Laure Latruffe, Stefan Wimmer
#'
#' @section Warning:
#' The \code{lowe()} function will not work with
#' unbalanced panel data. The Lowe index may be sensitive to the
#' rescaling.
#'
#' For extreme efficient observations, the problem of multiple solutions may
#' arise and the values of shadow prices may differ depending on the linear
#' programming solver used (here \pkg{lpSolveAPI}).
#'
#' @seealso
#' \code{\link[=print.Lowe]{print}}: for printing summary of each
#' element of the list in the \code{'Lowe'} object;
#'
#' \code{\link[=Levels.Lowe]{Levels}}: for extracting individual
#' productivity and profitability \bold{levels};
#'
#' \code{\link[=Changes.Lowe]{Changes}}: for extracting individual
#' productivity and profitability \bold{change indices};
#'
#' \code{\link[=Shadowp.Lowe]{Shadowp}}: for extracting inputs and
#' outputs \bold{deflated shadow prices}, if \code{shadow = TRUE};
#'
#' \code{\link[=lowe]{lowe}}: for computing the Lowe
#' productivity index.
#'
#' @references O'Donnell C.J. (2008), An aggregate quantity-price framework for
#' measuring and decomposing productivity and profitability change. School of
#' Economics, University of Queensland, Australia.
#' URL: \url{https://www.uq.edu.au/economics/cepa/docs/WP/WP072008.pdf}
#'
#' O'Donnell C.J. (2011), The sources of productivity change in the
#' manufacturing sectors of the U.S. economy. School of Economics, University of
#' Queensland, Australia.
#' URL: \url{http://www.uq.edu.au/economics/cepa/docs/WP/WP072011.pdf}
#'
#' O'Donnell C.J. (2012), Nonparametric estimates of the components of
#' productivity and profitability change in U.S. Agriculture.
#' \emph{American Journal of Agricultural Economics}, \bold{94}(4), 873--890.
#' \url{https://doi.org/10.1093/ajae/aas023}
#'
#' @keywords models optimize
#'
#' @examples
#' ## Lowe profitability and productivity levels and changes' computations
#' \dontrun{
#' Lowe.prod <- lowe(data = usagri, id.var = "States", time.var = "Years",
#' x.vars = c(7:10), y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13),
#' orientation = "in-out", by.id = 1, by.year = 1)
#' Lowe.prod
#' }
#' @export
# Lowe productivity index ----------
lowe <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, tech.change = TRUE,
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"),
  parallel = FALSE, cores = max(1, detectCores() - 1), scaled = TRUE, by.id = NULL, by.year = NULL,
  shadow = FALSE) {
  step1 <- check.2(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  if (!balanced(data = data, id.var = id.var, time.var = time.var))
    stop("Lowe index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
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
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)],
      2, FUN = function(x) x/mean(x))
  }
  P.lo <- apply(as.matrix(data[, step1$p.vars]), 2, mean)
  names(P.lo) <- paste0("U", 1:length(step1$p.vars))
  W.lo <- apply(as.matrix(data[, step1$w.vars]), 2, mean)
  names(W.lo) <- paste0("V", 1:length(step1$w.vars))
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
    res.lo.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI",
      "doParallel"), .export = c("lo.1", "DO.teseme", "DI.teseme", "D.tfp",
                                 "DO.shdu", "DI.shdu")) %dopar%
      {
        lo.1(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel,
             PRICESO = P.lo, PRICESI = W.lo, mean.x, mean.y, itt, it, shadow)
      }
  } else {
    res.lo.loop <- lo.2(data, step1, rts, orientation, parallel,
                        PRICESO = P.lo, PRICESI = W.lo, mean.x, mean.y, itt, it, shadow)
  }
  res.lo.loop <- cbind(data[, c(step1$id.var, step1$time.var)], res.lo.loop)
  row.names(res.lo.loop) <- seq(1:dim(res.lo.loop)[1])
    id.vec <- unique(res.lo.loop[, 1])
    if (!(is.null(by.id)) & !(is.null(by.year))) {
      if (by.id > length(id.vec))
        stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)),
          .call = FALSE)
      if (by.year > length(id.vec))
        stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)),
          .call = FALSE)
      indices <- res.lo.loop[, 3:20]/matrix(res.lo.loop[res.lo.loop[, 2] == year.vec[by.year],
        3:20][by.id, ], nrow = 1)
    } else {
      if (!(is.null(by.id)) & (is.null(by.year))) {
        if (by.id > length(id.vec))
          stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)),
          .call = FALSE)
        indices <- res.lo.loop[, 3:20]/matrix(res.lo.loop[by.id, 3:20], nrow = 1)
      } else {
        if ((is.null(by.id)) & !(is.null(by.year))) {
          if (by.year > length(id.vec))
          stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)),
            .call = FALSE)
          indices <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar%
          {
            res.lo.loop[res.lo.loop[, 2] == ano, 3:20]/res.lo.loop[res.lo.loop[, 2] == year.vec[by.year],
            3:20]
          }
        } else {
          indices <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar%
          {
            res.lo.loop[res.lo.loop[, 2] == ano, 3:20]/res.lo.loop[res.lo.loop[, 2] == year.vec[1],
            3:20]
          }
        }
      }
    }
    registerDoSEQ()
    stopImplicitCluster()
    indices <- cbind(res.lo.loop[, 1:2], indices)
    names(indices)[3:20] <- paste0("d", names(res.lo.loop[, 3:20]))
    if (shadow == TRUE) {
      Shadowp <- cbind(data[, c(step1$id.var, step1$time.var)], res.lo.loop[, 21:(dim(res.lo.loop)[2])])
      names(Shadowp) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
      res.tfp <- list(Levels = res.lo.loop[, 1:20], Changes = indices, Shadowp = Shadowp)
    } else {
      res.tfp <- list(Levels = res.lo.loop[, 1:20], Changes = indices)
    }
    class(res.tfp) <- c("list", "Lowe")
    return(res.tfp)
}

# Lowe print fonction ----------
#' @rdname lowe
#' @export
print.Lowe <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nLowe productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n\nLowe productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
  if (!is.null(x[["Shadowp"]])) {
    cat("\n\nLowe productivity shadow prices:\n\n")
    print(x[["Shadowp"]], digits = digits)
  }
  cat("\n")
  invisible(x)
}

# Return Lowe productivity and profitability Levels ----------
#' @rdname lowe
#' @export
Levels.Lowe <- function(x, ...) {
  return(x$Levels)
}

# Return Lowe productivity and profitability Changes ----------
#' @rdname lowe
#' @export
Changes.Lowe <- function(x, ...) {
  return(x$Changes)
}

# Return deflated inputs/outputs shadow prices ----------
#' @rdname lowe
#' @export
Shadowp.Lowe <- function(x, ...) {
  if (is.null(x$Shadowp)) {
    stop("No shadow prices are returned in your \"", class(x)[2], "\"", " object.
       Specifying 'shadow = TRUE' should be considered in the function generating the \"",
         class(x)[2], "\"", " object.")
  }
  return(x$Shadowp)
}
