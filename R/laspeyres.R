################################################################################
#                                                                              #
# R functions for the productivity package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Laspeyres productivity index (laspeyres)                                     #
#------------------------------------------------------------------------------#

#' Laspeyres productivity index
#'
#' Using Data Envelopment Analysis (DEA), this function measures productivity
#' in levels and changes with Laspeyres index.
#'
#' Deflated shadow prices of inputs and outputs can also be computed.
#'
#' @aliases laspeyres print.Laspeyres
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
#' only positive technological change (i.e. technological progress) is allaspeyresd.
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
#' @param shadow Logical. Default is \code{FALSE} (no shadow prices are
#' returned). When set to \code{TRUE}, the deflated input and output shadow
#' prices are returned. These shadow prices are only informative and may be
#' sensitive to the linear programming solver used.
#' @param x an object of class \code{Laspeyres} (returned by the function
#' \code{\link{laspeyres}}).
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
#' The Laspeyres index is not transitive and therefore each firm is compared to
#' itself in the previous period. Since there is no previous period for the
#' first period, the results (changes) for this first period are replaced by
#' \code{NA}
#'
#' A set of extractor functions for objects of class \code{'Laspeyres'}
#' including methods to the generic functions
#' \code{\link[=print.Laspeyres]{print}},
#' \code{\link[=Levels.Laspeyres]{Levels}},
#' \code{\link[=Changes.Laspeyres]{Changes}}, and
#' \code{\link[=Shadowp.Laspeyres]{Shadowp}}.
#'
#' @return \code{laspeyres()} returns a list of class \verb{'Laspeyres'}
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
#' \code{RAE} \tab Revenue allocative efficiency
#' \emph{(\code{orientation = "out"})}\cr
#' \tab \code{    } \emph{(equivalent to output-oriented mix efficiency score)}\cr
#' \code{ROSE} \tab Residual output-oriented scale efficiency score
#' \emph{(\code{orientation = "out"})}\cr
#' \code{OSME} \tab Output-oriented scale-mix efficiency score
#' \emph{(\code{orientation = "out"})}\cr
#' \code{ITE} \tab Input-oriented technical efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{ISE} \tab Input-oriented scale efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{CAE} \tab Cost allocative efficiency \emph{(\code{orientation = "in"})}\cr
#' \tab \code{    } \emph{(equivalent to input-oriented mix efficiency score)}\cr
#' \code{RISE} \tab Residual input-oriented scale efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{ISME} \tab Input-oriented scale-mix efficiency score
#' \emph{(\code{orientation = "in"})}\cr
#' \code{OTE.ITE} \tab Geometric mean of \code{OTE} and \code{ITE}
#' \emph{(\code{orientation = "in-out"})}\cr
#' \code{OSE.ISE} \tab Geometric mean of \code{OSE} and \code{ISE}
#' \emph{(\code{orientation = "in-out"})}\cr
#' \code{RAE.CAE} \tab Geometric mean of \code{RAE} and \code{CAE}
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
#' Hence, all efficiency scores are greater than zero and are laspeyresr or equal to
#' one.
#'
#' @author K Hervé Dakpo, Yann Desjeux, Laure Latruffe
#'
#' @section Warning:
#' The \code{laspeyres()} function will not work with
#' unbalanced panel data. The Laspeyres index may be sensitive to the
#' rescaling.
#'
#' For extreme efficient observations, the problem of multiple solutions may
#' arise and the values of shadow prices may differ depending on the linear
#' programming solver used (here \pkg{lpSolveAPI}).
#'
#' @seealso
#' \code{\link[=print.Laspeyres]{print}}: for printing summary of each
#' element of the list in the \code{'Laspeyres'} object;
#'
#' \code{\link[=Levels.Laspeyres]{Levels}}: for extracting individual
#' productivity and profitability \bold{levels};
#'
#' \code{\link[=Changes.Laspeyres]{Changes}}: for extracting individual
#' productivity and profitability \bold{change indices};
#'
#' \code{\link[=Shadowp.Laspeyres]{Shadowp}}: for extracting inputs and
#' outputs \bold{deflated shadow prices}, if \code{shadow = TRUE};
#'
#' \code{\link[=paasche]{paasche}}: for computing the Paasche
#' productivity index;
#'
#' \code{\link[=fisher]{fisher}}: for computing the Fisher
#' productivity index.
#'
#' @references Coelli T.J., D.S.P. Rao, C.J. O'Donnell, and G.E. Battese (2005),
#' \emph{An Introduction to Efficiency and Productivity Analysis}. Springer Eds.
#'
#' O'Donnell C.J. (2011), The sources of productivity change in the
#' manufacturing sectors of the U.S. economy. School of Economics, University of
#' Queensland, Australia.
#' URL: \url{http://www.uq.edu.au/economics/cepa/docs/WP/WP072011.pdf }
#'
#' @keywords models optimize
#'
#' @examples
#' ## Laspeyres profitability and productivity levels and changes' computations
#' \dontrun{
#' Laspeyres.prod <- laspeyres(data = usagri, id.var = "States", time.var = "Years",
#' x.vars = c(7:10), y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13),
#' orientation = "out")
#' Laspeyres.prod
#' }
#' @export
# Laspeyres productivity index ----------
laspeyres <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, tech.change = TRUE,
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"),
  parallel = FALSE, cores = max(1, detectCores() - 1), scaled = TRUE, shadow = FALSE) {
  step1 <- check.2(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  if (!balanced(data = data, id.var = id.var, time.var = time.var))
    stop("Laspeyres index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
  rts <- match.arg(rts)
  RTS <- c("vrs", "crs", "nirs", "ndrs")
  if (!(rts %in% RTS)) stop("Unknown scale of returns: ", paste(rts), call. = FALSE)
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in", "in-out")
  if (!(orientation %in% ORIENTATION)) stop("Unknown orientation: ", paste(orientation), call. = FALSE)
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
    res.las.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI",
      "doParallel"), .export = c("las.1",  "DO.teseme", "DI.teseme", "D.tfp",
                                 "DO.shdu", "DI.shdu")) %dopar% {
      las.1(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, mean.x, mean.y, itt, it, shadow)
    }
  } else {
    res.las.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI",
      "doParallel"), .export = c("las.2", "DO.teseme", "DI.teseme", "D.tfp",
                                 "DO.shdu", "DI.shdu")) %dopar% {
      las.2(data, step1, ano, year.vec, rts, orientation, parallel, mean.x, mean.y, itt, it, shadow)
    }
  }
  res.las.loop <- cbind(data[, c(step1$id.var, step1$time.var)], res.las.loop)
  row.names(res.las.loop) <- seq(1:dim(res.las.loop)[1])
    indices <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel")) %dopar%
      {
        cbind(res.las.loop[res.las.loop[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.las.loop[res.las.loop[,
          2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.las.loop[res.las.loop[,
          2] == year.vec[ano], c("P", "W")]/(res.las.loop[res.las.loop[, 2] == year.vec[ano -
          1], c("REV", "COST")]/res.las.loop[res.las.loop[, 2] == year.vec[ano],
          c("Qs", "Xs")]), TT = res.las.loop[res.las.loop[, 2] == year.vec[ano],
          "TT"]/fdiv(res.las.loop[res.las.loop[, 2] == year.vec[ano - 1], c("REV",
          "COST")]/res.las.loop[res.las.loop[, 2] == year.vec[ano], c("Qs", "Xs")]),
          res.las.loop[res.las.loop[, 2] == year.vec[ano], c("AO", "AI")]/res.las.loop[res.las.loop[,
          2] == year.vec[ano], c("Qs", "Xs")], res.las.loop[res.las.loop[, 2] ==
          year.vec[ano], c("TFP", "MP", "TFPE")]/res.las.loop[res.las.loop[, 2] ==
          year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.las.loop[res.las.loop[,
          2] == year.vec[ano], 14:15]/res.las.loop[res.las.loop[, 2] == year.vec[ano -
          1], 14:15], res.las.loop[res.las.loop[, 2] == year.vec[ano], 16:20]/res.las.loop[res.las.loop[,
          2] == year.vec[ano], 28:32])
      }
    registerDoSEQ()
    stopImplicitCluster()
    INT <- matrix(nrow = dim(res.las.loop[res.las.loop[, 2] == year.vec[1], ])[1], ncol = 18)
    INT <- as.data.frame(INT)
    names(INT) <- names(indices)
    indices <- rbind(INT, indices)
    indices <- cbind(res.las.loop[, 1:2], indices)
    names(indices)[3:20] <- paste0("d", names(res.las.loop[, 3:20]))
    if (shadow == TRUE) {
      Shadowp <- cbind(data[, c(step1$id.var, step1$time.var)], res.las.loop[, 33:(dim(res.las.loop)[2])])
      names(Shadowp) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
      res.tfp <- list(Levels = res.las.loop[, 1:20], Changes = indices, Shadowp = Shadowp)
    } else {
      res.tfp <- list(Levels = res.las.loop[, 1:20], Changes = indices)
    }
    class(res.tfp) <- c("list", "Laspeyres")
    return(res.tfp)
}

# Laspeyres print fonction ----------
#' @rdname laspeyres
#' @export
print.Laspeyres <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nLaspeyres productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n\nLaspeyres productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
  if (!is.null(x[["Shadowp"]])) {
    cat("\n\nLaspeyres productivity shadow prices:\n\n")
    print(x[["Shadowp"]], digits = digits)
  }
  cat("\n")
  invisible(x)
}

# Return Laspeyres productivity and profitability Levels ----------
#' @rdname laspeyres
#' @export
Levels.Laspeyres <- function(x, ...) {
  return(x$Levels)
}

# Return Laspeyres productivity and profitability Changes ----------
#' @rdname laspeyres
#' @export
Changes.Laspeyres <- function(x, ...) {
  return(x$Changes)
}

# Return deflated inputs/outputs shadow prices ----------
#' @rdname laspeyres
#' @export
Shadowp.Laspeyres <- function(x, ...) {
  if (is.null(x$Shadowp)) {
    stop("No shadow prices are returned in your \"", class(x)[2], "\"", " object.
       Specifying 'shadow = TRUE' should be considered in the function generating the \"",
         class(x)[2], "\"", " object.")
  }
  return(x$Shadowp)
}
