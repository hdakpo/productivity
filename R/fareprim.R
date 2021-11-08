# Färe-Primont productivity index (fareprim)

fareprim <- function(data, id.var, time.var, x.vars, y.vars, w.vars = NULL, p.vars = NULL, 
  tech.change = TRUE, tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), 
  orientation = c("out", "in", "in-out"), parallel = FALSE, cores = max(1, detectCores() - 1), 
  scaled = TRUE, by.id = NULL, by.year = NULL, shadow = FALSE) {
  step1 <- check.1(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  if (!balanced(data = data, id.var = id.var, time.var = time.var)) 
    stop("F\u00E4re-Primont index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
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
  P.fp <- DO.shdu(XOBS = apply(t(as.matrix(data[, step1$x.vars])), 1, mean), 
                  YOBS = apply(t(as.matrix(data[, step1$y.vars])), 1, mean), 
                  XREF = t(as.matrix(data[, step1$x.vars])), YREF = t(as.matrix(data[, step1$y.vars])), rts)
  W.fp <- DI.shdu(XOBS = apply(t(as.matrix(data[, step1$x.vars])), 1, mean), 
                  YOBS = apply(t(as.matrix(data[, step1$y.vars])), 1, mean), 
                  XREF = t(as.matrix(data[, step1$x.vars])), YREF = t(as.matrix(data[, step1$y.vars])), rts)
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
    res.fp.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI", 
      "doParallel"), .export = c("fp.1", "DO.teseme", "DI.teseme", "D.tfp")) %dopar% 
      {
        fp.1(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, 
             PRICESO = P.fp, PRICESI = W.fp, mean.x, mean.y, itt, it)
      }
  } else {
    res.fp.loop <- fp.2(data, step1, rts, orientation, parallel, 
                        PRICESO = P.fp, PRICESI = W.fp, mean.x, mean.y, itt, it)
  }
  res.fp.loop <- cbind(data[, c(step1$id.var, step1$time.var)], res.fp.loop)
  row.names(res.fp.loop) <- seq(1:dim(res.fp.loop)[1])
  names.fp <- c("REV", "COST", "PROF", "P", "W", "TT", "AO", "AI", "TFP", "MP", "TFPE", 
      "OTE", "OSE", "OME", "ROSE", "OSME", "ITE", "ISE", "IME", "RISE", "ISME", "RME", 
      "OTE.ITE", "OSE.ISE", "OME.IME", "ROSE.RISE", "OSME.ISME")
  names.var <- names(res.fp.loop)
  names.ind <- names.var[names.var %in% names.fp]
    id.vec <- unique(res.fp.loop[, 1])
    if (!(is.null(by.id)) & !(is.null(by.year))) {
      if (by.id > length(id.vec)) 
        stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
      if (by.year > length(id.vec)) 
        stop("by.year is out of range: by.year must be  lower or equal to ", paste(length(year.vec)), 
          .call = FALSE)
      indices <- res.fp.loop[, names.ind]/matrix(res.fp.loop[res.fp.loop[, 2] == year.vec[by.year], 
        names.ind][by.id, ], nrow = 1)
    } else {
      if (!(is.null(by.id)) & (is.null(by.year))) {
        if (by.id > length(id.vec)) 
          stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
        indices <- res.fp.loop[, names.ind]/matrix(res.fp.loop[by.id, names.ind], nrow = 1)
      } else {
        if ((is.null(by.id)) & !(is.null(by.year))) {
          if (by.year > length(id.vec)) 
          stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)), 
            .call = FALSE)
          indices <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            res.fp.loop[res.fp.loop[, 2] == ano, names.ind]/res.fp.loop[res.fp.loop[, 2] == year.vec[by.year], 
            names.ind]
          }
        } else {
          indices <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            res.fp.loop[res.fp.loop[, 2] == ano, names.ind]/res.fp.loop[res.fp.loop[, 2] == year.vec[1], 
            names.ind]
          }
        }
      }
    }
    registerDoSEQ()
    stopImplicitCluster()
    indices <- cbind(res.fp.loop[, 1:2], indices)
    names(indices)[-c(1, 2)] <- paste0("d", names.ind)
    if (shadow == TRUE) {
      Shadowp <- c(W.fp, P.fp)
      names(Shadowp) <- c(names(data[x.vars]), names(data[y.vars]))
      res.tfp <- list(Levels = res.fp.loop, Changes = indices, Shadowp = Shadowp)
    } else {
      res.tfp <- list(Levels = res.fp.loop, Changes = indices)
    }
    class(res.tfp) <- c("list", "FarePrimont")
    return(res.tfp)
}
