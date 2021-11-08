## Lowe productivity index.

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
