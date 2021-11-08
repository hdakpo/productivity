## Paasche with technical change

paa.1 <- function(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, mean.x, mean.y, itt, it, shadow) {
  ## period (Xt1, Yt1)
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  ## period (Xt, Yt)
  X2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$x.vars]))
  }
  Y2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$y.vars]))
  }
  if (tech.reg == TRUE) {
    ## period (Xt1, Yt1)
    XREF1 <- X1
    YREF1 <- Y1
    ## period (Xt, Yt)
    XREF2 <- X2
    YREF2 <- Y2
  } else {
    ## period (Xt1, Yt1)
    XREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$x.vars]))
    YREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$y.vars]))
    ## period (Xt, Yt)
    XREF2 <- if (ano == 1) {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$x.vars]))
    } else {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano - 1)], step1$x.vars]))
    }
    YREF2 <- if (ano == 1) {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$y.vars]))
    } else {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano - 1)], step1$y.vars]))
    }
  }
  ## prices matrix
  P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  P2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$p.vars]))
  }
  W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  W2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$w.vars]))
  }
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
    .packages = c("lpSolveAPI")) %dopar% {
      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
        cat(nextElem(it))
        flush.console()
      }
    Qt <- sum(P1[, dmu] * Y1[, dmu])
    Qs <- sum(P1[, dmu] * Y2[, dmu])
    Xt <- sum(W1[, dmu] * X1[, dmu])
    Xs <- sum(W1[, dmu] * X2[, dmu])
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P1[, dmu], PRICESI = W1[, 
      dmu], rts)
    TFPE <- TFP/MP
    TFP2 <- Qs/Xs
    MP2 <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P1[, dmu], PRICESI = W1[, 
      dmu], rts)
    TFPE2 <- TFP2/MP2
    if (shadow == TRUE) {
      PO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      PI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
    } else {
      PO <- NULL
      PI <- NULL
    }
    if (orientation == "out") {
      teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P1[, dmu], rts)
      names(teseme.O)[3] <- "RAE"
      RE <- teseme.O["RAE"] * teseme.O["OTE"]
      ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["RAE"]))/AI)/MP
      OSME <- teseme.O["RAE"] * ROSE
      RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
      REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
      COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      teseme.O2 <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P1[, dmu], rts)
      names(teseme.O2) <- c("OTE2", "OSE2","RAE2")
      RE2 <- teseme.O2["RAE2"] * teseme.O2["OTE2"]
      ROSE2 <- ((Qs/(teseme.O2["OTE2"] * teseme.O2["RAE2"]))/Xs)/MP2
      OSME2 <- teseme.O2["RAE2"] * ROSE2
      RME2 <- TFPE2/teseme.O2["OTE2"]/teseme.O2["OSE2"]
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                TFP = TFP, MP = MP, TFPE = TFPE,  teseme.O, ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME), 
                RE = unname(RE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                TFPE2 = TFPE2, teseme.O2["RAE2"], ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2),
                RE2 = unname(RE2), PRICEI = PI, PRICEO = PO)
    } else {
      if (orientation == "in") {
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = W1[, dmu], rts)
        names(teseme.I)[3] <- "CAE"
        CE <- teseme.I["CAE"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["CAE"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["CAE"] * RISE
        RME <- TFPE/teseme.I["ITE"]/teseme.I["ISE"]
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.I2 <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = W1[, dmu], rts)
        names(teseme.I2) <- c("ITE2", "ISE2","CAE2")
        CE2 <- teseme.I2["CAE2"] * teseme.I2["ITE2"]
        RISE2 <- (Qs/(Xs * teseme.I2["CAE2"] * teseme.I2["ITE2"]))/MP2
        ISME2 <- teseme.I2["CAE2"] * RISE2
        RME2 <- TFPE2/teseme.I2["ITE2"]/teseme.I2["ISE2"]
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.I, RISE = unname(RISE), ISME = unname(ISME),
                  RME = unname(RME), CE = unname(CE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, 
                  MP2 = MP2, TFPE2 = TFPE2,teseme.I2["CAE2"], RISE2 = unname(RISE2), ISME2 = unname(ISME2),
                  RME2 = unname(RME2), CE2 = unname(CE2), PRICEI = PI, PRICEO = PO)
      } else {
        teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P1[, dmu], rts)
        names(teseme.O)[3] <- "RAE"
        RE <- teseme.O["RAE"] * teseme.O["OTE"]
        ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["RAE"]))/AI)/MP
        OSME <- teseme.O["RAE"] * ROSE
        RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = W1[, dmu], rts)
        names(teseme.I)[3] <- "CAE"
        CE <- teseme.I["CAE"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["CAE"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["CAE"] * RISE
        teseme.OI <- sqrt(teseme.O * teseme.I)
        names(teseme.OI) <- c("OTE.ITE", "OSE.ISE", "RAE.CAE")
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- teseme.OI["RAE.CAE"] * ROSE.RISE
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.O2 <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P1[, dmu], rts)
        names(teseme.O2) <- c("OTE2", "OSE2","RAE2")
        RE2 <- teseme.O2["RAE2"] * teseme.O2["OTE2"]
        ROSE2 <- ((Qs/(teseme.O2["OTE2"] * teseme.O2["RAE2"]))/Xs)/MP2
        OSME2 <- teseme.O2["RAE2"] * ROSE2
        RME2 <- TFPE2/teseme.O2["OTE2"]/teseme.O2["OSE2"]
        teseme.I2 <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = W1[, dmu], rts)
        names(teseme.I2) <- c("ITE2", "ISE2","CAE2")
        CE2 <- teseme.I2["CAE2"] * teseme.I2["ITE2"]
        RISE2 <- (Qs/(Xs * teseme.I2["CAE2"] * teseme.I2["ITE2"]))/MP2
        ISME2 <- teseme.I2["CAE2"] * RISE2
        teseme.OI2 <- sqrt(teseme.O2 * teseme.I2)
        names(teseme.OI2) <- c("OTE2.ITE2", "OSE2.ISE2", "RAE2.CAE2")
        RE2.CE2 <- sqrt(RE2 * CE2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- teseme.OI2["RAE2.CAE2"] * ROSE2.RISE2
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.OI, ROSE.RISE = unname(ROSE.RISE), 
                  OSME.ISME = unname(OSME.ISME), RME = unname(RME), RE.CE = unname(RE.CE), Qt = Qt, 
                  Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, teseme.OI2["RAE2.CAE2"], 
                  ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2), RME2 = unname(RME2),
                  RE2.CE2 = unname(RE2.CE2), PRICEI = PI, PRICEO = PO)
      }
    }
    return(res1)
  }
  res2
}

## Paasche without technical change

paa.2 <- function(data, step1, ano, year.vec, rts, orientation, parallel, mean.x, mean.y, itt, it, shadow) {
  ## period (Xt1, Yt1)
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  ## period (Xt, Yt)
  X2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$x.vars]))
  }
  Y2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$y.vars]))
  }
  ## prices matrix
  P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  P2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$p.vars]))
  }
  W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  W2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$w.vars]))
  }
  XREFs <- t(as.matrix(data[, step1$x.vars]))
  YREFs <- t(as.matrix(data[, step1$y.vars]))
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
    .packages = c("lpSolveAPI")) %dopar% {
      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
        cat(nextElem(it))
        flush.console()
      }
    Qt <- sum(P1[, dmu] * Y1[, dmu])
    Qs <- sum(P1[, dmu] * Y2[, dmu])
    Xt <- sum(W1[, dmu] * X1[, dmu])
    Xs <- sum(W1[, dmu] * X2[, dmu])
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], PRICESI = W1[, 
      dmu], rts)
    TFPE <- TFP/MP
    TFP2 <- Qs/Xs
    MP2 <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], PRICESI = W1[, 
      dmu], rts)
    TFPE2 <- TFP2/MP2
    if (shadow == TRUE) {
      PO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
      PI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
    } else {
      PO <- NULL
      PI <- NULL
    }
    if (orientation == "out") {
      teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
      names(teseme.O)[3] <- "RAE"
      RE <- teseme.O["RAE"] * teseme.O["OTE"]
      ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["RAE"]))/AI)/MP
      OSME <- teseme.O["RAE"] * ROSE
      RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
      REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
      COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      teseme.O2 <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
      names(teseme.O2) <- c("OTE2", "OSE2","RAE2")
      RE2 <- teseme.O2["RAE2"] * teseme.O2["OTE2"]
      ROSE2 <- ((Qs/(teseme.O2["OTE2"] * teseme.O2["RAE2"]))/Xs)/MP2
      OSME2 <- teseme.O2["RAE2"] * ROSE2
      RME2 <- TFPE2/teseme.O2["OTE2"]/teseme.O2["OSE2"]
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                TFP = TFP, MP = MP, TFPE = TFPE,  teseme.O, ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME), 
                RE = unname(RE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                TFPE2 = TFPE2, teseme.O2["RAE2"], ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2),
                RE2 = unname(RE2), PRICEI = PI, PRICEO = PO)
    } else {
      if (orientation == "in") {
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        names(teseme.I)[3] <- "CAE"
        CE <- teseme.I["CAE"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["CAE"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["CAE"] * RISE
        RME <- TFPE/teseme.I["ITE"]/teseme.I["ISE"]
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.I2 <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        names(teseme.I2) <- c("ITE2", "ISE2","CAE2")
        CE2 <- teseme.I2["CAE2"] * teseme.I2["ITE2"]
        RISE2 <- (Qs/(Xs * teseme.I2["CAE2"] * teseme.I2["ITE2"]))/MP2
        ISME2 <- teseme.I2["CAE2"] * RISE2
        RME2 <- TFPE2/teseme.I2["ITE2"]/teseme.I2["ISE2"]
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.I, RISE = unname(RISE), ISME = unname(ISME),
                  RME = unname(RME), CE = unname(CE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, 
                  MP2 = MP2, TFPE2 = TFPE2,teseme.I2["CAE2"], RISE2 = unname(RISE2), ISME2 = unname(ISME2),
                  RME2 = unname(RME2), CE2 = unname(CE2), PRICEI = PI, PRICEO = PO)
      } else {
        teseme.O <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
        names(teseme.O)[3] <- "RAE"
        RE <- teseme.O["RAE"] * teseme.O["OTE"]
        ROSE <- ((AO/(teseme.O["OTE"] * teseme.O["RAE"]))/AI)/MP
        OSME <- teseme.O["RAE"] * ROSE
        RME <- TFPE/teseme.O["OTE"]/teseme.O["OSE"]
        teseme.I <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        names(teseme.I)[3] <- "CAE"
        CE <- teseme.I["CAE"] * teseme.I["ITE"]
        RISE <- (AO/(AI * teseme.I["CAE"] * teseme.I["ITE"]))/MP
        ISME <- teseme.I["CAE"] * RISE
        teseme.OI <- sqrt(teseme.O * teseme.I)
        names(teseme.OI) <- c("OTE.ITE", "OSE.ISE", "RAE.CAE")
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- teseme.OI["RAE.CAE"] * ROSE.RISE
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.O2 <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
        names(teseme.O2) <- c("OTE2", "OSE2","RAE2")
        RE2 <- teseme.O2["RAE2"] * teseme.O2["OTE2"]
        ROSE2 <- ((Qs/(teseme.O2["OTE2"] * teseme.O2["RAE2"]))/Xs)/MP2
        OSME2 <- teseme.O2["RAE2"] * ROSE2
        RME2 <- TFPE2/teseme.O2["OTE2"]/teseme.O2["OSE2"]
        teseme.I2 <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        names(teseme.I2) <- c("ITE2", "ISE2","CAE2")
        CE2 <- teseme.I2["CAE2"] * teseme.I2["ITE2"]
        RISE2 <- (Qs/(Xs * teseme.I2["CAE2"] * teseme.I2["ITE2"]))/MP2
        ISME2 <- teseme.I2["CAE2"] * RISE2
        teseme.OI2 <- sqrt(teseme.O2 * teseme.I2)
        names(teseme.OI2) <- c("OTE2.ITE2", "OSE2.ISE2", "RAE2.CAE2")
        RE2.CE2 <- sqrt(RE2 * CE2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- teseme.OI2["RAE2.CAE2"] * ROSE2.RISE2
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.OI, ROSE.RISE = unname(ROSE.RISE), 
                  OSME.ISME = unname(OSME.ISME), RME = unname(RME), RE.CE = unname(RE.CE), Qt = Qt, 
                  Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, teseme.OI2["RAE2.CAE2"], 
                  ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2), RME2 = unname(RME2),
                  RE2.CE2 = unname(RE2.CE2), PRICEI = PI, PRICEO = PO)
      }
    }
    return(res1)
  }
  res2
}

### Paasche, print fonction
print.Paasche <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nPaasche productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n\nPaasche productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
  if (!is.null(x[["Shadowp"]])) {
    cat("\n\nPaasche productivity shadow prices (summary):\n\n")
    print(summary(x[["Shadowp"]][-c(1:2)], digits = digits), digits = digits)
  }
  cat("\n")
  invisible(x)
}
