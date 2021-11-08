## Classic Malmquist productivity index

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
