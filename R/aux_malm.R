## Classic malmquist auxiliary functions

malm.1 <- function(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, itt, it) {
  ## period (Xt1, Yt1)
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano + 1], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano + 1], step1$y.vars]))
  ## period (Xt, Yt)
  X2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  if (tech.reg == TRUE) {
    ## period (Xt1, Yt1)
    XREF1 <- X1
    YREF1 <- Y1
    ## period (Xt, Yt)
    XREF2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
    YREF2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  } else {
    ## period (Xt1, Yt1)
    XREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano + 1)], step1$x.vars]))
    YREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano + 1)], step1$y.vars]))
    ## period (Xt, Yt)
    XREF2 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$x.vars]))
    YREF2 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$y.vars]))
  }
  # Malmquist components
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), 
      .combine = rbind, .packages = c("lpSolveAPI")) %dopar% {
    if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
      cat(nextElem(it))
      flush.console()
      }
    # n1n2n3: period reference, period input, period output
    ## Under CRS ci=1/co
    if (orientation == "out") {
      c111o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
      c100o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
      c011o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
      c000o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
      c110o <- DO.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
      c010o <- DO.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
        if (rts != "crs") {
          v111o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          v000o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
          res1 <- c(Year.0 = year.vec[ano], c111o = c111o, c100o = c100o, c011o = c011o, c000o = c000o, 
            c110o = c110o, c010o = c010o, v111o = v111o, v000o = v000o)
        } else {
          res1 <- c(Year.0 = year.vec[ano], c111o = c111o, c100o = c100o, c011o = c011o, c000o = c000o, 
            c110o = c110o, c010o = c010o)
        }
      } else {
        c111i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
        c100i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
        c011i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
        c000i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
        c110i <- DI.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
        c010i <- DI.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          if (rts != "crs") {
            v111i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
            v000i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
            res1 <- c(Year.0 = year.vec[ano], c111i = c111i, c100i = c100i, c011i = c011i, c000i = c000i, 
              c110i = c110i, c010i = c010i, v111i = v111i, v000i = v000i)
          } else {
            res1 <- c(Year.0 = year.vec[ano], c111i = c111i, c100i = c100i, c011i = c011i, c000i = c000i, 
              c110i = c110i, c010i = c010i)
          }
      }
    return(res1)
}
res2
}

### Malmquist, print fonction
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