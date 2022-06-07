################################################################################
#                                                                              #
# R functions for the productivity package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Malmquist productivity index (malm)                                          #
#------------------------------------------------------------------------------#

#' Malmquist productivity index
#'
#' Using Data Envelopment Analysis (DEA), this function measures Malmquist
#' productivity index.
#'
#' @aliases malm print.Malmquist
#'
#' @param data A dataframe containing the required information for measuring
#' productivity and profitability.
#' @param id.var Firms' ID variable. Can be an integer or a text string.
#' @param time.var Time period variable. Can be an integer or a text string.
#' @param x.vars Input quantity variables. Can be a vector of text strings or
#' integers.
#' @param y.vars Output quantity variables. Can be a vector of text strings or
#' integers.
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
#' The default value is \code{"out"} (output-orientation). Another possible
#' option is \code{"in"} (input-orientation).
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
#' @param x an object of class \code{Malmquist} (returned by the function
#' \code{\link{malm}}).
#' @param digits The minimum number of significant digits to be printed in
#' values. Default = \code{max(3, getOption("digits") - 3)}.
#' @param ... currently ignored.
#'
#' @details
#' Distance functions required for computing the Malmquist index are radial
#' measures which verify the translation invariance property. Hence, unless very
#' large or very small values are present, the Malmquist index is insensitive to
#' the rescaling option (\code{scaled})
#'
#' Objects of class \code{'Malmquist'} are associated with the generic function
#' \code{\link[=print.Malmquist]{print}}
#'
#' @return \code{malm()} returns a list of class \verb{'Malmquist'} that
#' contains productivity measures in levels and changes.
#'
#' This list contains the following items:
#'
#' \item{Levels}{It contains the Shephard distance function estimates, useful to
#' compute and decompose the Malmquist productivity index. These distance
#' functions use input and output quantities for period \code{1} and period
#' \code{0}.
#'
#' In addition to the \code{id.var} variable and periods \code{1} and \code{0},
#' the dataframe therefore contains, depending on the orientation:
#' \code{c111o}, \code{c100o}, \code{c011o}, \code{c000o}, \code{c110o},
#' \code{c010o}, or \code{c111i}, \code{c100i}, \code{c011i}, \code{c000i},
#' \code{c110i}, \code{c010i}. When the returns to scale option (\code{rts}) is
#' different from \code{"crs"}, then \code{v111o} and \code{v000o}, or
#' \code{v111i} and \code{v000i} (depending on the orientation) are returned in
#' addition to the distance function estimated under constant returns to scale
#' (\code{"crs"}). The prefix "c" stands for constant returns to scale
#' (\code{"crs"}) and "v" for all other types of returns to scale
#' (i.e. \code{"vrs"}, \code{"nirs"}, or \code{"ndrs"}). The suffix "o" means
#' output-oriented while "i" refers to input-oriented.
#'
#' The distance function names are displayed with three digits:
#' \emph{(i)} the first digit represents the period of the reference technology,
#' \emph{(ii)} the second digit represents the period of the inputs, and
#' \emph{(iii)} the third digit represents the period of the outputs.
#' For instance \code{c010o} means output-oriented efficiency under constant
#' returns to scale (\code{"crs"}), with the reference technology of period
#' \code{0}, inputs of period \code{1} and outputs of period \code{0}.}
#'
#' \item{Changes}{Malmquist productivity index and its components are provided,
#' depending on the orientation.
#' \tabular{ll}{
#' \code{malmquist} \tab Malmquist productivity index\cr
#' \code{effch} \tab Efficiency change\cr
#' \code{tech} \tab Technological change\cr
#' \code{obtech} \tab Output-biased technological change\cr
#' \code{ibtech} \tab Input-biased technological change\cr
#' \code{matech} \tab Magnitude component\cr
#' \code{pure.out.effch} \tab Pure output efficiency change\cr
#' \tab \code{    } \emph{(when \code{rts != "crs"} and
#' \code{orientation = "out"})}\cr
#' \code{out.scalech} \tab Output scale efficiency change\cr
#' \tab \code{    } \emph{(when \code{rts != "crs"} and
#' \code{orientation = "out"})}\cr
#' \code{pure.inp.effch} \tab Pure input efficiency change\cr
#' \tab \code{    } \emph{(when \code{rts != "crs"} and
#' \code{orientation = "in"})}\cr
#' \code{inp.scalech} \tab Input scale efficiency change\cr
#' \tab \code{    } \emph{(when \code{rts != "crs"} and
#' \code{orientation = "in"})}}}
#'
#' \bold{Note that:}
#' \enumerate{
#' \item{\code{obtech} (Output-biased technological change), \code{ibtech}
#' (Input-biased technological change), and \code{matech} (Magnitude component)
#' are components of technological change (\code{tech}).}
#' \item{\code{pure.out.effch} (Pure output efficiency change) and
#' \code{out.scalech} (Output scale efficiency change) are components of
#' efficiency change (\code{effch}).}
#' \item{\code{pure.inp.effch} (Pure input efficiency change), and
#' \code{inp.scalech} (Input scale efficiency change) are components of
#' efficiency change (\code{effch}).}}
#'
#' From an object of class \verb{'Malmquist'} obtained from \code{malm()}, the
#' \itemize{
#' \item \code{\link{Levels}} function extracts Shephard distance function
#' estimates; and
#' \item \code{\link{Changes}} function extracts Malmquist productivity index
#' and components.}
#'
#' @note The Malmquist productivity index and components are computed such that
#' both \code{orientation}'s results provide the same information: growth when
#' index greater than one and decline when index lower than one. Moreover under
#' \code{rts = "crs"}, both \code{orientation} options (i.e. \code{"out"} and
#' \code{"in"}) yield the same results.
#'
#' @author K Hervé Dakpo, Yann Desjeux, Laure Latruffe, Stefan Wimmer
#'
#' @section Warning:
#' The \code{malm()} function will not work with unbalanced
#' panel data.
#'
#' @seealso
#' \code{\link[=print.Malmquist]{print}}: for printing summary of each
#' element of the list in the \code{'Malmquist'} object;
#'
#' \code{\link[=Levels.Malmquist]{Levels}}: to retrieve a data frame
#' with Shephard distance function estimates;
#'
#' \code{\link[=Changes.Malmquist]{Changes}}: to retrieve a data frame
#' with Malmquist productivity index and components.
#'
#' @references Färe R., and Grosskopf S. (1996), \emph{Intertemporal Production
#' Frontiers: With Dynamic DEA}. Springer Eds.
#'
#'@keywords models optimize
#'
#' @examples
#' ## Malmquist productivity index compares each observation in period 1 to the
#' ## same observation in period 0
#' \dontrun{
#' Malmquist <- malm(data = usagri, id.var = "States", time.var = "Years",
#'      x.vars = c("q.capital", "q.land","q.labor","q.materials"),
#'      y.vars = c("q.livestock", "q.crop", "q.other"), rts = "nirs")
#' Malmquist
#' }
#' @export
# Malmquist productivity index ----------
malm <- function(data, id.var, time.var, x.vars, y.vars, tech.reg = TRUE,
rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in"), parallel = FALSE,
cores = max(1, detectCores() - 1), scaled = TRUE) {
  step1 <- check.3(data, id.var, time.var, x.vars, y.vars)
  if (!balanced(data = data, id.var = id.var, time.var = time.var))
    stop("Malmquist index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
  rts <- match.arg(rts)
  RTS <- c("vrs", "crs", "nirs", "ndrs")
  if (!(rts %in% RTS))
    stop("Unknown scale of returns: ", paste(rts), call. = FALSE)
  data <- data[order(data[, step1$time.var], data[, step1$id.var]), ]
  year.vec <- unique(data[, time.var])
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in")
  if (!(orientation %in% ORIENTATION))
    stop("Unknown orientation: ", paste(orientation), call. = FALSE)
  if (scaled == FALSE) {
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 | data[, c(step1$x.vars, step1$y.vars)] <= 1e-04))
      warning("Some quantity variables are not between 1e-4 and 1e5.
We recommend rescaling the data or set the scaled option to TRUE to avoid numerical problems\n\r", call. = FALSE)
  } else {
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)],
      2, FUN = function(x) x/mean(x))
  }

  pas <- 5
  it <- iter(c(paste0("\rProgress: ", seq(0,100-pas,pas), "%\r"), "DONE!        \r\n\r"))
  itt <- round(seq(1, nrow(data) - length(levels(as.factor(data[,id.var]))), (nrow(data) - length(levels(as.factor(data[,id.var]))))/(100/pas)),0)
  itt[(100/pas)+1L] <- nrow(data) - length(levels(as.factor(data[,id.var])))

  if (parallel == TRUE & cores == 1) { parallel <- FALSE }
  if (parallel == TRUE & cores > 1) {
      registerDoParallel(cores = cores)
    } else {
    registerDoSEQ()
    }

  res_malm_loop <- foreach(ano = 1:(length(year.vec) - 1), .combine = rbind, .packages = c("lpSolveAPI",
    "doParallel"), .export = c("malm.1", "DO.sh", "DI.sh")) %dopar% {
    malm.1(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, itt, it)
  }
  registerDoSEQ()
  stopImplicitCluster()
  res_malm_loop <- cbind(data[data[, step1$time.var] %in% year.vec[-1], c(step1$id.var, step1$time.var)], res_malm_loop)
  names(res_malm_loop)[2] <- "Year.1"
  row.names(res_malm_loop) <- seq(1:dim(res_malm_loop)[1])
    data.indices <- res_malm_loop[, 1:3]
    if (orientation == "out") {
    data.indices[, "malmquist"] <- ((res_malm_loop[, "c111o"]/res_malm_loop[, "c100o"]) * (res_malm_loop[,
      "c011o"]/res_malm_loop[, "c000o"]))^0.5
    data.indices[, "effch"] <- res_malm_loop[, "c111o"]/res_malm_loop[, "c000o"]
    data.indices[, "tech"] <- ((res_malm_loop[, "c011o"]/res_malm_loop[, "c111o"]) * (res_malm_loop[,
      "c000o"]/res_malm_loop[, "c100o"]))^0.5
    data.indices[, "obtech"] <- ((res_malm_loop[, "c011o"]/res_malm_loop[, "c111o"]) * (res_malm_loop[,
      "c110o"]/res_malm_loop[, "c010o"]))^0.5
    data.indices[, "ibtech"] <- ((res_malm_loop[, "c100o"]/res_malm_loop[, "c000o"]) * (res_malm_loop[,
      "c010o"]/res_malm_loop[, "c110o"]))^0.5
    data.indices[, "matech"] <- res_malm_loop[, "c000o"]/res_malm_loop[, "c100o"]
    if (dim(res_malm_loop)[2] > 9) {
        data.indices[, "pure.out.effch"] <- res_malm_loop[, "v111o"]/res_malm_loop[, "v000o"]
        data.indices[, "out.scalech"] <- (res_malm_loop[, "c111o"]/res_malm_loop[, "c000o"])/(res_malm_loop[, "v111o"]/res_malm_loop[, "v000o"])
      }
    } else {
    data.indices[, "malmquist"] <- ((res_malm_loop[, "c100i"]/res_malm_loop[, "c111i"]) * (res_malm_loop[,
        "c000i"]/res_malm_loop[, "c011i"]))^0.5
      data.indices[, "effch"] <- res_malm_loop[, "c000i"]/res_malm_loop[, "c111i"]
      data.indices[, "tech"] <- ((res_malm_loop[, "c111i"]/res_malm_loop[, "c011i"]) * (res_malm_loop[,
        "c100i"]/res_malm_loop[, "c000i"]))^0.5
      data.indices[, "obtech"] <- ((res_malm_loop[, "c111i"]/res_malm_loop[, "c011i"]) * (res_malm_loop[,
        "c010i"]/res_malm_loop[, "c110i"]))^0.5
      data.indices[, "ibtech"] <- ((res_malm_loop[, "c000i"]/res_malm_loop[, "c100i"]) * (res_malm_loop[,
        "c110i"]/res_malm_loop[, "c010i"]))^0.5
      data.indices[, "matech"] <- res_malm_loop[, "c100i"]/res_malm_loop[, "c000i"]
      if (dim(res_malm_loop)[2] > 9) {
        data.indices[, "pure.inp.effch"] <- res_malm_loop[, "v000i"]/res_malm_loop[, "v111i"]
        data.indices[, "inp.scalech"] <- (res_malm_loop[, "c000i"]/res_malm_loop[, "c111i"])/(res_malm_loop[, "v000i"]/res_malm_loop[, "v111i"])
      }
    }
    res.tfp <- list(Levels = res_malm_loop, Changes = data.indices)
    class(res.tfp) <- c("list", "Malmquist")
    return(res.tfp)
}

# Malmquist print fonction ----------
#' @rdname malm
#' @export
print.Malmquist <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nShephard distance function estimates (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:3)], digits = digits), digits = digits)
  cat("\n\nMalmquist productivity index results (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:3)], digits = digits), digits = digits)
  cat("\n")
  invisible(x)
}

# Malmquist print fonction ----------
#' @rdname malm
#' @export
print.Malmquist <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nMalmquist productivity levels (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n\nMalmquist productivity changes (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n")
  invisible(x)
}

# Return Malmquist productivity and profitability Levels ----------
#' @rdname malm
#' @export
Levels.Malmquist <- function(x, ...) {
  return(x$Levels)
}

# Return Malmquist productivity and profitability Changes ----------
#' @rdname malm
#' @export
Changes.Malmquist <- function(x, ...) {
  return(x$Changes)
}
