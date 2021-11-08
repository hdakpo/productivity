# Laspeyres productivity index (laspeyres)

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
