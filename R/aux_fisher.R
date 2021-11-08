## Fisher with technical change


fish.1 <- function(data, step1, ano, year.vec, tech.reg, rts, orientation, parallel, mean.x, mean.y, itt, it, shadow) {
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
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), 
    .combine = rbind, .packages = c("lpSolveAPI")) %dopar% {
      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
        cat(nextElem(it))
        flush.console()
      }
    P.Qt <- sum(P1[, dmu] * Y1[, dmu])
    P.Qs <- sum(P1[, dmu] * Y2[, dmu])
    P.Xt <- sum(W1[, dmu] * X1[, dmu])
    P.Xs <- sum(W1[, dmu] * X2[, dmu])
    L.Qt <- sum(P2[, dmu] * Y1[, dmu])
    L.Qs <- sum(P2[, dmu] * Y2[, dmu])
    L.Xt <- sum(W2[, dmu] * X1[, dmu])
    L.Xs <- sum(W2[, dmu] * X2[, dmu])
    Qt <- sqrt(L.Qt * P.Qt)
    Qs <- sqrt(L.Qs * P.Qs)
    Xt <- sqrt(L.Xt * P.Xt)
    Xs <- sqrt(L.Xs * P.Xs)
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- sqrt(D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P1[, 
      dmu], PRICESI = W1[, dmu], rts) * D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, 
      YREF = YREF1, PRICESO = P2[, dmu], PRICESI = W2[, dmu], rts))
    TFPE <- TFP/MP
    TFP2 <- Qs/Xs
    MP2 <- sqrt(D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P1[, 
      dmu], PRICESI = W1[, dmu], rts) * D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, 
      YREF = YREF2, PRICESO = P2[, dmu], PRICESI = W2[, dmu], rts))
    TFPE2 <- TFP2/MP2
    if (shadow == TRUE) {
      PO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      PI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
    } else {
      PO <- NULL
      PI <- NULL
    }
    if (orientation == "out") {
      teseme.OL <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P2[, dmu], rts)
      teseme.OP <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P1[, dmu], rts)
      RAE <- sqrt(teseme.OL["OME"] * teseme.OP["OME"])
      RE <- RAE * teseme.OL["OTE"]
      ROSE <- ((AO/(teseme.OL["OTE"] * RAE))/AI)/MP
      OSME <- RAE * ROSE
      RME <- TFPE/teseme.OL["OTE"]/teseme.OL["OSE"]
      REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
      COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      teseme.O2L <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P2[, dmu], rts)
      names(teseme.O2L) <- c("OTE2", "OSE2","OME2")
      teseme.O2P <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P1[, dmu], rts)
      names(teseme.O2P) <- c("OTE2", "OSE2","OME2")
      RAE2 <- sqrt(teseme.O2L["OME2"] * teseme.O2P["OME2"])
      RE2 <- RAE2 * teseme.O2L["OTE2"]
      ROSE2 <- ((Qs/(teseme.O2L["OTE2"] * RAE2))/Xs)/MP2
      OSME2 <- RAE2 * ROSE2
      RME2 <- TFPE2/teseme.O2L["OTE2"]/teseme.O2L["OSE2"]
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                TFP = TFP, MP = MP, TFPE = TFPE,  teseme.OL[1:2], RAE = unname(RAE), ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME), 
                RE = unname(RE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                TFPE2 = TFPE2, RAE2, ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2),
                RE2 = unname(RE2), PRICEI = PI, PRICEO = PO)
    } else {
      if (orientation == "in") {
        teseme.IL <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = W2[, dmu], rts)
        teseme.IP <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = W1[, dmu], rts)
        CAE <- sqrt(teseme.IL["IME"] * teseme.IP["IME"])
        CE <- CAE * teseme.IL["ITE"]
        RISE <- (AO/(AI * CAE * teseme.IL["ITE"]))/MP
        ISME <- CAE * RISE
        RME <- TFPE/teseme.IL["ITE"]/teseme.IL["ISE"]
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.I2L <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = W2[, dmu], rts)
        names(teseme.I2L) <- c("ITE2", "ISE2","IME2")
        teseme.I2P <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = W1[, dmu], rts)
        names(teseme.I2P) <- c("ITE2", "ISE2","IME2")
        CAE2 <- sqrt(teseme.I2L["IME2"] * teseme.I2P["IME2"])
        CE2 <- CAE2 * teseme.I2L["ITE2"]
        RISE2 <- (Qs/(Xs * CAE2 * teseme.I2L["ITE2"]))/MP2
        ISME2 <- CAE2 * RISE2
        RME2 <- TFPE2/teseme.I2L["ITE2"]/teseme.I2L["ISE2"]
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.IL[1:2], CAE = unname(CAE), RISE = unname(RISE), ISME = unname(ISME),
                  RME = unname(RME), CE = unname(CE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, 
                  MP2 = MP2, TFPE2 = TFPE2, CAE2, RISE2 = unname(RISE2), ISME2 = unname(ISME2),
                  RME2 = unname(RME2), CE2 = unname(CE2), PRICEI = PI, PRICEO = PO)
      } else {
        teseme.OL <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P2[, dmu], rts)
        teseme.OP <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P1[, dmu], rts)
        RAE <- sqrt(teseme.OL["OME"] * teseme.OP["OME"])
        RE <- RAE * teseme.OL["OTE"]
        ROSE <- ((AO/(teseme.OL["OTE"] * RAE))/AI)/MP
        OSME <- RAE * ROSE
        RME <- TFPE/teseme.OL["OTE"]/teseme.OL["OSE"]
        teseme.IL <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = W2[, dmu], rts)
        teseme.IP <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = W1[, dmu], rts)
        CAE <- sqrt(teseme.IL["IME"] * teseme.IP["IME"])
        CE <- CAE * teseme.IL["ITE"]
        RISE <- (AO/(AI * CAE * teseme.IL["ITE"]))/MP
        ISME <- CAE * RISE
        teseme.OIL <- sqrt(teseme.OL * teseme.IL)
        names(teseme.OIL)[1:2] <- c("OTE.ITE", "OSE.ISE")
        RAE.CAE <- sqrt(RAE * CAE)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- RAE.CAE * ROSE.RISE
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.O2L <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P2[, dmu], rts)
        names(teseme.O2L) <- c("OTE2", "OSE2","OME2")
        teseme.O2P <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P1[, dmu], rts)
        names(teseme.O2P) <- c("OTE2", "OSE2","OME2")
        RAE2 <- sqrt(teseme.O2L["OME2"] * teseme.O2P["OME2"])
        RE2 <- RAE2 * teseme.O2L["OTE2"]
        ROSE2 <- ((Qs/(teseme.O2L["OTE2"] * RAE2))/Xs)/MP2
        OSME2 <- RAE2 * ROSE2
        RME2 <- TFPE2/teseme.O2L["OTE2"]/teseme.O2L["OSE2"]
        teseme.I2L <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = W2[, dmu], rts)
        names(teseme.I2L) <- c("ITE2", "ISE2","IME2")
        teseme.I2P <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = W1[, dmu], rts)
        names(teseme.I2P) <- c("ITE2", "ISE2","IME2")
        CAE2 <- sqrt(teseme.I2L["IME2"] * teseme.I2P["IME2"])
        CE2 <- CAE2 * teseme.I2L["ITE2"]
        RISE2 <- (Qs/(Xs * CAE2 * teseme.I2L["ITE2"]))/MP2
        ISME2 <- CAE2 * RISE2
        teseme.OI2L <- sqrt(teseme.O2L * teseme.I2L)
        RE2.CE2 <- sqrt(RE2 * CE2)
        RAE2.CAE2 <- sqrt(RAE2 * CAE2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- RAE2.CAE2 * ROSE2.RISE2
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.OIL[1:2], RAE.CAE = unname(RAE.CAE), ROSE.RISE = unname(ROSE.RISE), 
                  OSME.ISME = unname(OSME.ISME), RME = unname(RME), RE.CE = unname(RE.CE), Qt = Qt, 
                  Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, RAE2.CAE2 = unname(RAE2.CAE2), 
                  ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2), RME2 = unname(RME2),
                  RE2.CE2 = unname(RE2.CE2), PRICEI = PI, PRICEO = PO)
      }
    }
    return(res1)
  }
  res2
}

## Fisher without technical change

fish.2 <- function(data, step1, ano, year.vec, rts, orientation, parallel, mean.x, mean.y, itt, it, shadow) {
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
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), 
    .combine = rbind, .packages = c("lpSolveAPI")) %dopar% {
      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
        cat(nextElem(it))
        flush.console()
      }
    P.Qt <- sum(P1[, dmu] * Y1[, dmu])
    P.Qs <- sum(P1[, dmu] * Y2[, dmu])
    P.Xt <- sum(W1[, dmu] * X1[, dmu])
    P.Xs <- sum(W1[, dmu] * X2[, dmu])
    L.Qt <- sum(P2[, dmu] * Y1[, dmu])
    L.Qs <- sum(P2[, dmu] * Y2[, dmu])
    L.Xt <- sum(W2[, dmu] * X1[, dmu])
    L.Xs <- sum(W2[, dmu] * X2[, dmu])
    Qt <- sqrt(L.Qt * P.Qt)
    Qs <- sqrt(L.Qs * P.Qs)
    Xt <- sqrt(L.Xt * P.Xt)
    Xs <- sqrt(L.Xs * P.Xs)
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- sqrt(D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, 
      dmu], PRICESI = W1[, dmu], rts) * D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, 
      YREF = YREFs, PRICESO = P2[, dmu], PRICESI = W2[, dmu], rts))
    TFPE <- TFP/MP
    TFP2 <- Qs/Xs
    MP2 <- sqrt(D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, 
      dmu], PRICESI = W1[, dmu], rts) * D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, 
      YREF = YREFs, PRICESO = P2[, dmu], PRICESI = W2[, dmu], rts))
    TFPE2 <- TFP2/MP2
    if (shadow == TRUE) {
      PO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
      PI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
    } else {
      PO <- NULL
      PI <- NULL
    }
    if (orientation == "out") {
      teseme.OL <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, dmu], rts)
      teseme.OP <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
      RAE <- sqrt(teseme.OL["OME"] * teseme.OP["OME"])
      RE <- RAE * teseme.OL["OTE"]
      ROSE <- ((AO/(teseme.OL["OTE"] * RAE))/AI)/MP
      OSME <- RAE * ROSE
      RME <- TFPE/teseme.OL["OTE"]/teseme.OL["OSE"]
      REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
      COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      teseme.O2L <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, dmu], rts)
      names(teseme.O2L) <- c("OTE2", "OSE2","OME2")
      teseme.O2P <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
      names(teseme.O2P) <- c("OTE2", "OSE2","OME2")
      RAE2 <- sqrt(teseme.O2L["OME2"] * teseme.O2P["OME2"])
      RE2 <- RAE2 * teseme.O2L["OTE2"]
      ROSE2 <- ((Qs/(teseme.O2L["OTE2"] * RAE2))/Xs)/MP2
      OSME2 <- RAE2 * ROSE2
      RME2 <- TFPE2/teseme.O2L["OTE2"]/teseme.O2L["OSE2"]
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                TFP = TFP, MP = MP, TFPE = TFPE,  teseme.OL[1:2], RAE = unname(RAE), ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME), 
                RE = unname(RE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                TFPE2 = TFPE2, RAE2, ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2),
                RE2 = unname(RE2), PRICEI = PI, PRICEO = PO)
    } else {
      if (orientation == "in") {
        teseme.IL <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W2[, dmu], rts)
        teseme.IP <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        CAE <- sqrt(teseme.IL["IME"] * teseme.IP["IME"])
        CE <- CAE * teseme.IL["ITE"]
        RISE <- (AO/(AI * CAE * teseme.IL["ITE"]))/MP
        ISME <- CAE * RISE
        RME <- TFPE/teseme.IL["ITE"]/teseme.IL["ISE"]
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.I2L <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W2[, dmu], rts)
        names(teseme.I2L) <- c("ITE2", "ISE2","IME2")
        teseme.I2P <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        names(teseme.I2P) <- c("ITE2", "ISE2","IME2")
        CAE2 <- sqrt(teseme.I2L["IME2"] * teseme.I2P["IME2"])
        CE2 <- CAE2 * teseme.I2L["ITE2"]
        RISE2 <- (Qs/(Xs * CAE2 * teseme.I2L["ITE2"]))/MP2
        ISME2 <- CAE2 * RISE2
        RME2 <- TFPE2/teseme.I2L["ITE2"]/teseme.I2L["ISE2"]
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.IL[1:2], CAE = unname(CAE), RISE = unname(RISE), ISME = unname(ISME),
                  RME = unname(RME), CE = unname(CE), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, 
                  MP2 = MP2, TFPE2 = TFPE2, CAE2, RISE2 = unname(RISE2), ISME2 = unname(ISME2),
                  RME2 = unname(RME2), CE2 = unname(CE2), PRICEI = PI, PRICEO = PO)
      } else {
        teseme.OL <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, dmu], rts)
        teseme.OP <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
        RAE <- sqrt(teseme.OL["OME"] * teseme.OP["OME"])
        RE <- RAE * teseme.OL["OTE"]
        ROSE <- ((AO/(teseme.OL["OTE"] * RAE))/AI)/MP
        OSME <- RAE * ROSE
        RME <- TFPE/teseme.OL["OTE"]/teseme.OL["OSE"]
        teseme.IL <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W2[, dmu], rts)
        teseme.IP <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        CAE <- sqrt(teseme.IL["IME"] * teseme.IP["IME"])
        CE <- CAE * teseme.IL["ITE"]
        RISE <- (AO/(AI * CAE * teseme.IL["ITE"]))/MP
        ISME <- CAE * RISE
        teseme.OIL <- sqrt(teseme.OL * teseme.IL)
        names(teseme.OIL)[1:2] <- c("OTE.ITE", "OSE.ISE")
        RAE.CAE <- sqrt(RAE * CAE)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- RAE.CAE * ROSE.RISE
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
        COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        teseme.O2L <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, dmu], rts)
        names(teseme.O2L) <- c("OTE2", "OSE2","OME2")
        teseme.O2P <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P1[, dmu], rts)
        names(teseme.O2P) <- c("OTE2", "OSE2","OME2")
        RAE2 <- sqrt(teseme.O2L["OME2"] * teseme.O2P["OME2"])
        RE2 <- RAE2 * teseme.O2L["OTE2"]
        ROSE2 <- ((Qs/(teseme.O2L["OTE2"] * RAE2))/Xs)/MP2
        OSME2 <- RAE2 * ROSE2
        RME2 <- TFPE2/teseme.O2L["OTE2"]/teseme.O2L["OSE2"]
        teseme.I2L <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W2[, dmu], rts)
        names(teseme.I2L) <- c("ITE2", "ISE2","IME2")
        teseme.I2P <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = W1[, dmu], rts)
        names(teseme.I2P) <- c("ITE2", "ISE2","IME2")
        CAE2 <- sqrt(teseme.I2L["IME2"] * teseme.I2P["IME2"])
        CE2 <- CAE2 * teseme.I2L["ITE2"]
        RISE2 <- (Qs/(Xs * CAE2 * teseme.I2L["ITE2"]))/MP2
        ISME2 <- CAE2 * RISE2
        teseme.OI2L <- sqrt(teseme.O2L * teseme.I2L)
        RE2.CE2 <- sqrt(RE2 * CE2)
        RAE2.CAE2 <- sqrt(RAE2 * CAE2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- RAE2.CAE2 * ROSE2.RISE2
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
                  TFP = TFP, MP = MP, TFPE = TFPE, teseme.OIL[1:2], RAE.CAE = unname(RAE.CAE), ROSE.RISE = unname(ROSE.RISE), 
                  OSME.ISME = unname(OSME.ISME), RME = unname(RME), RE.CE = unname(RE.CE), Qt = Qt, 
                  Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, RAE2.CAE2 = unname(RAE2.CAE2), 
                  ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2), RME2 = unname(RME2),
                  RE2.CE2 = unname(RE2.CE2), PRICEI = PI, PRICEO = PO)
      }
    }
    return(res1)
  }
  res2
}

### Fisher, print fonction
print.Fisher <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nFisher productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n\nFisher productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
  if (!is.null(x[["Shadowp"]])) {
    cat("\n\nFisher productivity shadow prices (summary):\n\n")
    print(summary(x[["Shadowp"]][-c(1:2)], digits = digits), digits = digits)
  }
  cat("\n")
  invisible(x)
}
