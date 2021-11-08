## Productivity package auxiliary functions

# Data check

check.1 <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables (id.var). Only one can be defined", call. = FALSE)
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in id.var: ", paste(id.var), call. = FALSE)
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables (time.var). Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var:", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variables (x.vars)", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variables (y.vars)", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (is.null(w.vars) & is.null(p.vars)) {
    list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1)
  } else {
    if (!(is.null(w.vars)) & !(is.null(p.vars))) {
      if (length(w.vars) != length(x.vars)) 
        stop("x.vars and w.vars must be of the same length", call. = FALSE)
      if (length(p.vars) != length(y.vars)) 
        stop("y.vars and p.vars must be of the same length", call. = FALSE)
      if (is.numeric(w.vars)) {
        w.vars.1 <- names.var[w.vars]
      } else {
        w.vars.1 <- w.vars
      }
      var_logical <- w.vars.1 %in% names.var
      if (!(all(var_logical))) 
        stop("Unrecognizable variables in w.vars:", paste(w.vars[var_logical == F], 
          collapse = ","), call. = FALSE)
      if (is.numeric(p.vars)) {
        p.vars.1 <- names.var[p.vars]
      } else {
        p.vars.1 <- p.vars
      }
      var_logical <- p.vars.1 %in% names.var
      if (!(all(var_logical))) 
        stop("Unrecognizable variables in p.vars:", paste(p.vars[var_logical == F], 
          collapse = ","), call. = FALSE)
      list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1, 
        w.vars = w.vars.1, p.vars = p.vars.1)
    } else {
      if (!(is.null(w.vars)) & is.null(p.vars)) {
        stop("Output prices p.vars must also be supplied", call. = FALSE)
      } else {
        stop("Input prices w.vars must also be supplied", call. = FALSE)
      }
    }
  }
}

check.2 <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables (id.var). Only one can be defined", call. = FALSE)
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in id.var: ", paste(id.var), call. = FALSE)
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables (time.var). Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var:", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variables (x.vars)", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variables (y.vars)", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(w.vars)) {
    stop("Missing w variables (w.vars)", call. = FALSE)
  } else {
    if (is.numeric(w.vars)) {
      w.vars.1 <- names.var[w.vars]
    } else {
      w.vars.1 <- w.vars
    }
    var_logical <- w.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in w.vars:", paste(w.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(p.vars)) {
    stop("Missing p variables (p.vars)", call. = FALSE)
  } else {
    if (is.numeric(p.vars)) {
      p.vars.1 <- names.var[p.vars]
    } else {
      p.vars.1 <- p.vars
    }
    var_logical <- p.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in p.vars:", paste(p.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (length(w.vars) != length(x.vars)) 
    stop("x.vars and w.vars must be of the same length", call. = FALSE)
  if (length(p.vars) != length(y.vars)) 
    stop("y.vars and p.vars must be of the same length", call. = FALSE)
  list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1, w.vars = w.vars.1, 
    p.vars = p.vars.1)
}

check.3 <- function(data, id.var, time.var, x.vars, y.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables (id.var). Only one can be defined", call. = FALSE)
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in id.var: ", paste(id.var), call. = FALSE)
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables (time.var). Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var:", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variables (x.vars)", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variables (y.vars)", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1)
}

## OTE OSE and OME estimation

DO.teseme <- function(XOBS, YOBS, XREF, YREF, PRICESO, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, 1 + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(0, XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(-YOBS[j], YREF[j, ]))
  }
  set.objfn(built.lp, c(1, rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  set.rhs (built.lp, c(XOBS, rep(0, n_y)))
  lp.control(built.lp, sense = "max") 
  solve(built.lp)
  ote_crs <- 1/get.objective(built.lp)
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
       "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(0, rep(1, n_t)), type = ctyp, rhs = 1)
  }
  solve(built.lp)
  OTE <- 1/get.objective(built.lp)
  OSE <- ote_crs/OTE
  built.lp <- make.lp(n_x + n_y, n_y + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(rep(0, n_y), XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(-diag(1, ncol = n_y, nrow = n_y)[j, ], YREF[j, ]))
  }
  set.objfn(built.lp, c(PRICESO/sum(YOBS * PRICESO), rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.rhs (built.lp, c(XOBS, rep(0, n_y)))
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(rep(0, n_y), rep(1, n_t)), type = ctyp, rhs = 1)
  }
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  OME <- 1/(get.objective(built.lp) * OTE)
  c(OTE = OTE, OSE = OSE, OME = OME)
}

# Shephard  Output distance function

DO.sh <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, 1 + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(0, XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(-YOBS[j], YREF[j, ]))
  }
  set.objfn(built.lp, c(1, rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  set.rhs (built.lp, c(XOBS, rep(0, n_y)))
  lp.control(built.lp, sense = "max") 
  if (rts == "crs") {
    solve(built.lp)
    DO <- 1/get.objective(built.lp)
  } else {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(0, rep(1, n_t)), type = ctyp, rhs = 1)
    solve(built.lp)
    DO <- 1/get.objective(built.lp)
  }
  DO
}

##ITE ISE and IME estimation

DI.teseme <- function(XOBS, YOBS, XREF, PRICESI, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, 1 + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(-XOBS[i], XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(0, YREF[j, ]))
  }
  set.objfn(built.lp, c(1, rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.rhs (built.lp, c(rep(0, n_x), YOBS))
  set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  lp.control(built.lp, sense = "min")
  solve(built.lp)
  ite_crs <- get.objective(built.lp)
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(0, rep(1, n_t)), type = ctyp, rhs = 1)
  }
  solve(built.lp)
  ITE <- get.objective(built.lp)
  ISE <- ite_crs/ITE
  built.lp <- make.lp(n_x + n_y , n_x + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(-diag(1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(rep(0, n_x), YREF[j, ]))
  }
  set.objfn(built.lp, c(PRICESI/sum(XOBS * PRICESI), rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.rhs (built.lp, c(rep(0, n_x), YOBS))
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp,  c(rep(0, n_x), rep(1, n_t)), type = ctyp, rhs = 1)
  }
  lp.control(built.lp, sense = "min")
  solve(built.lp)
  IME <- get.objective(built.lp)/ITE
  c(ITE = ITE, ISE = ISE, IME = IME)
}

# Shephard Input distance function

DI.sh <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, 1 + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(-XOBS[i], XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(0, YREF[j, ]))
  }
  set.objfn(built.lp, c(1, rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.rhs (built.lp, c(rep(0, n_x), YOBS))
  set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  lp.control(built.lp, sense = "min")
  if (rts == "crs") {
    solve(built.lp)
    DI <- 1/get.objective(built.lp)
  } else {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(0, rep(1, n_t)), type = ctyp, rhs = 1)
    solve(built.lp)
    DI <- 1/get.objective(built.lp)
  }
  DI
}

## Point of maximum productivity

D.tfp <- function(XOBS, YOBS, XREF, YREF, PRICESO, PRICESI, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y + 1, n_y + n_x + n_t + 1)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(rep(0, n_y), diag(-1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ], 0))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(diag(-1, ncol = n_y, nrow = n_y)[j, ], rep(0, n_x), YREF[j, ], 0))
  }
  set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)))
  set.objfn(built.lp, c(PRICESO, rep(0, n_x), rep(0, n_t), 0))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "="))
  set.rhs (built.lp, c(rep(0, n_x), rep(0, n_y), 1))
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(rep(0, n_x + n_y), rep(1, n_t), -1), type = ctyp, rhs = 0)
  }
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  return(get.objective(built.lp))
}

## dual shephard output distance function

DO.shdu <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_t + 1, n_y + n_x)
  for (i in 1:n_y) {
    set.column(built.lp, i, c(-YREF[i, ], YOBS[i]))
  }
  for (j in 1:n_x) {
    set.column(built.lp, n_y + j, c(XREF[j, ], 0))
  }
  if (rts %in% c("vrs", "nirs")) {
    add.column(built.lp, c(rep(1, n_t), 0))
  } else {
    if (rts == "ndrs") {
      add.column(built.lp, c(rep(-1, n_t), 0))
    }
  }
  obj <- if (rts == "crs") {
    c(rep(0, n_y), XOBS)
  } else {
    if (rts %in% c("vrs", "nirs")) {
      c(rep(0, n_y), XOBS, 1)
    } else {
      if (rts == "ndrs") {
        c(rep(0, n_y), XOBS, -1)
      }
    }
  }
  set.objfn(built.lp, obj)
  set.constr.type(built.lp, c(rep(">=", n_t), "="))
  set.rhs (built.lp, c(rep(0, n_t), 1))
  if (rts == "vrs") set.bounds(built.lp, lower = -Inf, upper = Inf, columns = n_x + n_y + 1)
  lp.control(built.lp, sense = "min")
  solve(built.lp)
  prices_o <- get.variables(built.lp)[1:n_y]/(sum(get.variables(built.lp)[(1 + n_y):(n_y + n_x)] * XOBS) + 
              if (rts == "crs") {
                0
              } else {
                if (rts %in% c("vrs", "nirs")) {
                  get.variables(built.lp)[n_y + n_x + 1]
                } else {
                  if (rts == "ndrs") {
                    -get.variables(built.lp)[n_y + n_x + 1]
                  }
                }
              })
  names(prices_o) <- paste("U", 1:n_y, sep = "")
  prices_o
}

## dual shephard input distance function

DI.shdu <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_t + 1, n_y + n_x)
  for (i in 1:n_y) {
    set.column(built.lp, i, c(YREF[i, ], 0))
  }
  for (j in 1:n_x) {
    set.column(built.lp, n_y + j, c(-XREF[j, ], XOBS[j]))
  }
  if (rts %in% c("vrs", "ndrs")) {
    add.column(built.lp,  c(rep(1, n_t), 0))
  } else {
    if (rts == "nirs") {
      add.column(built.lp, c(rep(-1, n_t), 0))
    }
  }
  obj <- if (rts == "crs") {
    c(YOBS, rep(0, n_x))
  } else {
    if (rts %in% c("vrs", "ndrs")) {
      c(YOBS, rep(0, n_x), 1)
    } else {
      if (rts == "nirs") {
        c(YOBS, rep(0, n_x), -1)
      }
    }
  }
  set.objfn(built.lp, obj)
  set.constr.type(built.lp, c(rep("<=", n_t), "="))
  set.rhs (built.lp, c(rep(0, n_t), 1))
  if (rts == "vrs") set.bounds(built.lp, lower = -Inf, upper = Inf, columns = n_x + n_y + 1)
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  prices_i <- get.variables(built.lp)[(n_y + 1):(n_y + n_x)]/
    (sum(get.variables(built.lp)[1:n_y] * YOBS) + 
       if (rts == "crs") {
         0
       } else {
         if (rts %in% c("vrs", "ndrs")) {
           get.variables(built.lp)[n_x + n_y + 1]
         } else {
           if (rts == "nirs") {
             -get.variables(built.lp)[n_x + n_y + 1]
           }
         }
       })
  names(prices_i) <- paste("V", 1:n_x, sep = "")
  prices_i
}

## apply a function to divide 2 columns
fdiv <- function(x) x[, 1]/x[, 2]

## Test for balanced panel data

balanced <- function(data, id.var, time.var) {
  x <- data[, id.var]
  y <- data[, time.var]
  if (length(x) != length(y)) stop(paste0("The length of the two vectors (i.e. ", id.var, " and ", time.var, ") differs\n"))
  x <- data[, id.var][drop = TRUE]
  y <- data[, time.var][drop = TRUE]
  z <- table(x, y)
  if (any(as.vector(z) == 0)) {
    balanced <- FALSE
  } else {
    balanced <- TRUE
  }
  return(balanced)
}

## Return functions (i.e. Levels(); Changes(); Shadowp())

Levels <- function(object, ...) {
  if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche", "HicksMoorsteen"))) {
    stop("Function 'Levels' can not be applied to an object of class \"", class(object), "\"")
  }
  if (is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche")) | (is(object, "HicksMoorsteen") & (length(object) == 2))) {
  return(object$Levels)
  }
  if (is(object, "HicksMoorsteen") & (length(object) > 2)) {
  return(lapply(object, function(x) x$Levels))
  }
}

Changes <- function(object, ...) {
  if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche", "HicksMoorsteen"))) {
    stop("Function 'Changes' can not be applied to an object of class \"", class(object), "\"")
  }
  if (is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche")) | (is(object, "HicksMoorsteen") & (length(object) == 2))) {
  return(object$Changes)
  }
  if (is(object, "HicksMoorsteen") & (length(object) > 2)) {
  return(lapply(object, function(x) x$Changes))
  }
}


Shadowp <- function(object, ...) {
  if (is(object, c("Malmquist"))) {
    stop("Function 'Shadowp' can not be applied to an object of class \"", class(object)[2], "\"")
  }
  if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Paasche", "HicksMoorsteen"))) {
    stop("Function 'Shadowp' can not be applied to an object of class \"", class(object), "\"")
  }
  if (is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Paasche")) & is.null(object$Shadowp)) {
    stop("No shadow prices are returned in your \"", class(object)[2], "\"", " object. 
       Specifying 'shadow = TRUE' should be considered in the function generating the \"", class(object)[2], "\"", " object.")
  }
  if (is(object, "HicksMoorsteen")) {
    if (length(object) == 2) {
      stop("No shadow prices are returned in your \"", class(object)[2], "\"", " object. 
       Specifying 'components = TRUE' should be considered in the function generating the \"", class(object)[2], "\"", " object.")
    } else {
      List <- lapply(object, function(x) x$Shadowp)
      return(List[!sapply(List,is.null)])
    }
  }
  return(object$Shadowp)
}
