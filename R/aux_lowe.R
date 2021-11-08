## Lowe auxiliary functions first step lowe (with technical change)

lo.1 <- function(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, PRICESO, PRICESI, mean.x, mean.y, itt, it, shadow) {
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  if (tech.reg == TRUE) {
    XREF1 <- X1
    YREF1 <- Y1
  } else {
    XREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:ano], step1$x.vars]))
    YREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:ano], step1$y.vars]))
  }
  P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
    .packages = c("lpSolveAPI")) %dopar% {
      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
        cat(nextElem(it))
        flush.console()
      }
    AO <- sum(PRICESO * Y1[, dmu])
    AI <- sum(PRICESI * X1[, dmu])
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, rts)
    TFPE <- TFP/MP
    if (shadow == TRUE) {
      PO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      PI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
    } else {
      PO <- NULL
      PI <- NULL
    }
    if (orientation == "out") {
      teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)
      RE <- teseme.O["OME"] * teseme.O["OTE"]
      ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["OME"]))/AI)/MP
      OSME <- teseme.O["OME"] * ROSE
      RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
      REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
      COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
        MP = MP, TFPE = TFPE, teseme.O, ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME), RE = unname(RE), 
        PRICEI = PI, PRICEO = PO)
    } else {
      if (orientation == "in") {
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts)
        CE <- teseme.I["IME"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["IME"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["IME"] * RISE
        RME <- TFPE/teseme.I["ITE"]/teseme.I["ISE"]
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, teseme.I, RISE = unname(RISE), ISME = unname(ISME),
          RME = unname(RME), CE = unname(CE), PRICEI = PI, PRICEO = PO)
      } else {
        teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)
        RE <- teseme.O["OME"] * teseme.O["OTE"]
        ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["OME"]))/AI)/MP
        OSME <- teseme.O["OME"] * ROSE
        RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts)
        CE <- teseme.I["IME"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["IME"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["IME"] * RISE
        teseme.OI <- sqrt(teseme.O * teseme.I)
        names(teseme.OI) <- c("OTE.ITE", "OSE.ISE", "OME.IME")
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- teseme.OI["OME.IME"] * ROSE.RISE
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, teseme.OI, ROSE.RISE = unname(ROSE.RISE), 
          OSME.ISME = unname(OSME.ISME), RME = unname(RME), RE.CE = unname(RE.CE), PRICEI = PI, PRICEO = PO)
      }
    }
    return(res1)
  }
  res2
}

# first step lowe (without technical change)
lo.2 <- function(data, step1, rts, orientation, parallel, PRICESO, PRICESI,  mean.x, mean.y, itt, it, shadow) {
  X1 <- t(as.matrix(data[, step1$x.vars]))
  Y1 <- t(as.matrix(data[, step1$y.vars]))
  XREF1 <- X1
  YREF1 <- Y1
  P1 <- t(as.matrix(data[, step1$p.vars]))
  W1 <- t(as.matrix(data[, step1$w.vars]))
  res2 <- foreach(dmu = 1:length(data[, step1$id.var]), .combine = rbind, .packages = c("lpSolveAPI"), .export = c("DO.teseme", 
    "DI.teseme", "D.tfp", "DO.shdu", "DI.shdu")) %dopar% {
      if (nrow(data) > 99 & parallel == FALSE & dmu %in% itt) {
        cat(nextElem(it))
        flush.console()
      }
    AO <- sum(PRICESO * Y1[, dmu])
    AI <- sum(PRICESI * X1[, dmu])
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, rts)
    TFPE <- TFP/MP
    if (shadow == TRUE) {
      PO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      PI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
    } else {
      PO <- NULL
      PI <- NULL
    }
    if (orientation == "out") {
      teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)
      RE <- teseme.O["OME"] * teseme.O["OTE"]
      ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["OME"]))/AI)/MP
      OSME <- teseme.O["OME"] * ROSE
      RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
      REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
      COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
        MP = MP, TFPE = TFPE, teseme.O, ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME), RE = unname(RE), 
        PRICEI = PI, PRICEO = PO)
    } else {
      if (orientation == "in") {
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts)
        CE <- teseme.I["IME"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["IME"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["IME"] * RISE
        RME <- TFPE/teseme.I["ITE"]/teseme.I["ISE"]
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE,  teseme.I, RISE = unname(RISE), ISME = unname(ISME),
          RME = unname(RME), CE = unname(CE), PRICEI = PI, PRICEO = PO)
      } else {
        teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)
        RE <- teseme.O["OME"] * teseme.O["OTE"]
        ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["OME"]))/AI)/MP
        OSME <- teseme.O["OME"] * ROSE
        RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts)
        CE <- teseme.I["IME"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["IME"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["IME"] * RISE
        teseme.OI <- sqrt(teseme.O * teseme.I)
        names(teseme.OI) <- c("OTE.ITE", "OSE.ISE", "OME.IME")
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- teseme.OI["OME.IME"] * ROSE.RISE
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, teseme.OI, ROSE.RISE = unname(ROSE.RISE), 
          OSME.ISME = unname(OSME.ISME), RME = unname(RME), RE.CE = unname(RE.CE), PRICEI = PI, PRICEO = PO)
      }
    }
    return(res1)
  }
  res2
}

### Lowe, print fonction
print.Lowe <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nLowe productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n\nLowe productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
  if (!is.null(x[["Shadowp"]])) {
    cat("\n\nLowe productivity shadow prices (summary):\n\n")
    print(summary(x[["Shadowp"]][-c(1:2)], digits = digits), digits = digits)
  }
  cat("\n")
  invisible(x)
}
