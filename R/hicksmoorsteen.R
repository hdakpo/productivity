# Hicks-Moorsteen productivity index (hicksmoorsteen)

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
