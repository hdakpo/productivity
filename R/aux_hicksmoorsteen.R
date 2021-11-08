## Hicks-Moorsteen with technical change (define malm-hs and malm-it)

hm.1 <- function (data, step1, ano, year.vec, tech.reg, rts, orientation, parallel,
                  components, mean.x, mean.y, itt, it) {
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
  
  if (length(step1) == 6) {
    P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
    W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  }
  if (components == FALSE) {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
                        cat(nextElem(it))
                        flush.console()
                      }
                      PRICESO.hs <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESI.hs <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESO.it <- DO.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      PRICESI.it <- DI.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      Qt <- sqrt(sum(PRICESO.hs * Y1[, dmu]) * sum(PRICESO.it * Y1[, dmu]))
                      Xt <- sqrt(sum(PRICESI.hs * X1[, dmu]) * sum(PRICESI.it * X1[, dmu]))
                      Qs <- sqrt(sum(PRICESO.hs * Y2[, dmu]) * sum(PRICESO.it * Y2[, dmu]))
                      Xs <- sqrt(sum(PRICESI.hs * X2[, dmu]) * sum(PRICESI.it * X2[, dmu]))
                      AO <- Qt
                      AI <- Xt
                      TFP <- AO/AI
                      MP <- sqrt(D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                   D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE <- TFP/MP
                      TFP2 <- Qs/Xs
                      MP2 <- sqrt(D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                    D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE2 <- TFP2/MP2
                      if (orientation == "out") {
                        teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts)
                        teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts)
                        OME <- sqrt(teseme.Ohs["OME"] * teseme.Oit["OME"])
                        ROSE <- ((AO/(teseme.Ohs["OTE"] * OME))/AI)/MP
                        OSME <- OME * ROSE
                        RME <- TFPE/teseme.Ohs["OTE"]/teseme.Ohs["OSE"]
                        teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts)
                        names(teseme.O2hs) <- c("OTE2", "OSE2", "OME2")
                        teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts)
                        names(teseme.O2it) <- c("OTE2", "OSE2", "OME2")
                        OME2 <- sqrt(teseme.O2hs["OME2"] * teseme.O2it["OME2"])
                        ROSE2 <- ((Qs/(teseme.O2hs["OTE2"] * OME2))/Xs)/MP2
                        OSME2 <- OME2 * ROSE2
                        RME2 <- TFPE2/teseme.O2hs["OTE2"]/teseme.O2hs["OSE2"]
                        if (length(step1) == 6) {
                          REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                          COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                          PROF <- REV/COST
                          P <- REV/AO
                          W <- COST/AI
                          TT <- P/W
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                    TFPE = TFPE, teseme.Ohs[1:2], OME = unname(OME), ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME),
                                    Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = unname(OME2), 
                                    ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2)) 
                        } else {
                          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, teseme.Ohs[1:2], OME = unname(OME), ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME),
                                    Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = unname(OME2), 
                                    ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2)) 
                        }
                      } else {
                        if (orientation == "in") {
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts)
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.it, rts)
                          IME <- sqrt(teseme.Ihs["IME"] * teseme.Iit["IME"])
                          RISE <- (AO/(AI * teseme.Ihs["ITE"] * IME))/MP
                          ISME <- IME * RISE
                          RME <- TFPE/teseme.Ihs["ITE"]/teseme.Ihs["ISE"]
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2", "ISE2", "IME2")
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2", "ISE2", "IME2")
                          IME2 <- sqrt(teseme.I2hs["IME2"] * teseme.I2it["IME2"])
                          RISE2 <- (Qs/(Xs * teseme.I2hs["ITE2"] * IME2))/MP2
                          ISME2 <- IME2 * RISE2
                          RME2 <- TFPE2/teseme.I2hs["ITE2"]/teseme.I2hs["ISE2"]
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P <- REV/AO
                            W <- COST/AI
                            TT <- P/W
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, teseme.Ihs[1:2], IME = unname(IME), RISE = unname(RISE), ISME = unname(ISME), RME = unname(RME),
                                      Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = unname(IME2), 
                                      RISE2 = unname(RISE2), ISME2 = unname(ISME2), RME2 = unname(RME2))
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, teseme.Ihs[1:2], IME = unname(IME), RISE = unname(RISE), ISME = unname(ISME), RME = unname(RME),
                                      Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = unname(IME2), 
                                      RISE2 = unname(RISE2), ISME2 = unname(ISME2), RME2 = unname(RME2)) 
                          }
                        } else {
                          teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts)
                          teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts)
                          OME <- sqrt(teseme.Ohs["OME"] * teseme.Oit["OME"])
                          ROSE <- ((AO/(teseme.Ohs["OTE"] * OME))/AI)/MP
                          OSME <- OME * ROSE
                          RME <- TFPE/teseme.Ohs["OTE"]/teseme.Ohs["OSE"]
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts)
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.it, rts)
                          IME <- sqrt(teseme.Ihs["IME"] * teseme.Iit["IME"])
                          RISE <- (AO/(AI * teseme.Ihs["ITE"] * IME))/MP
                          ISME <- IME * RISE
                          teseme.OIhs <- sqrt(teseme.Ohs * teseme.Ihs) #OTE OSE ITE ISE are the same for both hs and it indices
                          names(teseme.OIhs)[1:2] <- c("OTE.ITE", "OSE.ISE")
                          OME.IME <- sqrt(OME * IME)
                          ROSE.RISE <- sqrt(ROSE * RISE)
                          OSME.ISME <- (OME.IME * ROSE.RISE)
                          teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts)
                          names(teseme.O2hs) <- c("OTE2", "OSE2", "OME2")
                          teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts)
                          names(teseme.O2it) <- c("OTE2", "OSE2", "OME2")
                          OME2 <- sqrt(teseme.O2hs["OME2"] * teseme.O2it["OME2"])
                          ROSE2 <- ((Qs/(teseme.O2hs["OTE2"] * OME2))/Xs)/MP2
                          OSME2 <- OME2 * ROSE2
                          RME2 <- TFPE2/teseme.O2hs["OTE2"]/teseme.O2hs["OSE2"]
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2", "ISE2", "IME2")
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2", "ISE2", "IME2")
                          IME2 <- sqrt(teseme.I2hs["IME2"] * teseme.I2it["IME2"])
                          RISE2 <- (Qs/(Xs * teseme.I2hs["ITE2"] * IME2))/MP2
                          ISME2 <- IME2 * RISE2
                          teseme.OI2hs <- sqrt(teseme.O2hs * teseme.I2hs)
                          OME2.IME2 <- sqrt(OME2 * IME2)
                          ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
                          OSME2.ISME2 <- (OME2.IME2 * ROSE2.RISE2)
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P <- REV/AO
                            W <- COST/AI
                            TT <- P/W
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, teseme.OIhs[1:2], OME.IME = unname(OME.IME), ROSE.RISE = unname(ROSE.RISE), OSME.ISME = unname(OSME.ISME), 
                                      RME = unname(RME), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = unname(OME2.IME2), ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2),
                                      RME2 = unname(RME2))
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, teseme.OIhs[1:2], OME.IME = unname(OME.IME), ROSE.RISE = unname(ROSE.RISE), OSME.ISME = unname(OSME.ISME), 
                                      RME = unname(RME), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = unname(OME2.IME2), ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2),
                                      RME2 = unname(RME2))
                          }
                        }
                      }
                      return(res1)
                    }
  } else {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
                        cat(nextElem(it))
                        flush.console()
                      }
                      PRICESO.hs <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESI.hs <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESO.it <- DO.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      PRICESI.it <- DI.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      Qt.hs <- sum(PRICESO.hs * Y1[, dmu])
                      Qs.hs <- sum(PRICESO.hs * Y2[, dmu])
                      Xt.hs <- sum(PRICESI.hs * X1[, dmu])
                      Xs.hs <- sum(PRICESI.hs * X2[, dmu])
                      Qt.it <- sum(PRICESO.it * Y1[, dmu])
                      Qs.it <- sum(PRICESO.it * Y2[, dmu])
                      Xt.it <- sum(PRICESI.it * X1[, dmu])
                      Xs.it <- sum(PRICESI.it * X2[, dmu])
                      AO.hs <- Qt.hs
                      AI.hs <- Xt.hs
                      TFP.hs <- AO.hs/AI.hs
                      MP.hs <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE.hs <- TFP.hs/MP.hs
                      TFP2.hs <- Qs.hs/Xs.hs
                      MP2.hs <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE2.hs <- TFP2.hs/MP2.hs
                      AO.it <- Qt.it
                      AI.it <- Xt.it
                      TFP.it <- AO.it/AI.it
                      MP.it <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE.it <- TFP.it/MP.it
                      TFP2.it <- Qs.it/Xs.it
                      MP2.it <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE2.it <- TFP2.it/MP2.it
                      if (orientation == "out") {
                        teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts)
                        names(teseme.Ohs) <- c("OTE.hs", "OSE.hs", "OME.hs")
                        ROSE.hs <- ((AO.hs/(teseme.Ohs["OTE.hs"] * teseme.Ohs["OME.hs"]))/AI.hs)/MP.hs
                        OSME.hs <- teseme.Ohs["OME.hs"] * ROSE.hs
                        RME.hs <- TFPE.hs/teseme.Ohs["OTE.hs"]/teseme.Ohs["OSE.hs"]
                        teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts)
                        names(teseme.O2hs) <- c("OTE2.hs", "OSE2.hs", "OME2.hs")
                        ROSE2.hs <- ((Qs.hs/(teseme.O2hs["OTE2.hs"] * teseme.O2hs["OME2.hs"]))/Xs.hs)/MP2.hs
                        OSME2.hs <- teseme.O2hs["OME2.hs"] * ROSE2.hs
                        RME2.hs <- TFPE2.hs/teseme.O2hs["OTE2.hs"]/teseme.O2hs["OSE2.hs"]
                        teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts)
                        names(teseme.Oit) <- c("OTE.it", "OSE.it", "OME.it")
                        ROSE.it <- ((AO.it/(teseme.Oit["OTE.it"] * teseme.Oit["OME.it"]))/AI.it)/MP.it
                        OSME.it <- teseme.Oit["OME.it"] * ROSE.it
                        RME.it <- TFPE.it/teseme.Oit["OTE.it"]/teseme.Oit["OSE.it"]
                        teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts)
                        names(teseme.O2it) <- c("OTE2.it", "OSE2.it", "OME2.it")
                        ROSE2.it <- ((Qs.it/(teseme.O2it["OTE2.it"] * teseme.O2it["OME2.it"]))/Xs.it)/MP2.it
                        OSME2.it <- teseme.O2it["OME2.it"] * ROSE2.it
                        RME2.it <- TFPE2.it/teseme.O2it["OTE2.it"]/teseme.O2it["OSE2.it"]
                        if (length(step1) == 6) {
                          REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                          COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                          PROF <- REV/COST
                          P.hs <- REV/AO.hs
                          W.hs <- COST/AI.hs
                          TT.hs <- P.hs/W.hs
                          P.it <- REV/AO.it
                          W.it <- COST/AI.it
                          TT.it <- P.it/W.it
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                    TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ohs, 
                                    ROSE.hs = unname(ROSE.hs), OSME.hs =  unname(OSME.hs), RME.hs =  unname(RME.hs),
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, teseme.O2hs["OME2.hs"], ROSE2.hs =  unname(ROSE2.hs), OSME2.hs =  unname(OSME2.hs),
                                    RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Oit,
                                    ROSE.it =  unname(ROSE.it), OSME.it =  unname(OSME.it), RME.it =  unname(RME.it),
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, teseme.O2it["OME2.it"], ROSE2.it =  unname(ROSE2.it), OSME2.it =  unname(OSME2.it),
                                    RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it) 
                        } else {
                          res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ohs, 
                                    ROSE.hs = unname(ROSE.hs), OSME.hs =  unname(OSME.hs), RME.hs =  unname(RME.hs),
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, teseme.O2hs["OME2.hs"], ROSE2.hs =  unname(ROSE2.hs), OSME2.hs =  unname(OSME2.hs),
                                    RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Oit,
                                    ROSE.it =  unname(ROSE.it), OSME.it =  unname(OSME.it), RME.it =  unname(RME.it),
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, teseme.O2it["OME2.it"], ROSE2.it =  unname(ROSE2.it), OSME2.it =  unname(OSME2.it),
                                    RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it) 
                        }
                      } else {
                        if (orientation == "in") {
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts)
                          names(teseme.Ihs) <- c("ITE.hs", "ISE.hs", "IME.hs")
                          RISE.hs <- (AO.hs/(AI.hs * teseme.Ihs["ITE.hs"] * teseme.Ihs["IME.hs"]))/MP.hs
                          ISME.hs <- teseme.Ihs["IME.hs"] * RISE.hs
                          RME.hs <- TFPE.hs/teseme.Ihs["ITE.hs"]/teseme.Ihs["ISE.hs"]
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2.hs", "ISE2.hs", "IME2.hs")
                          RISE2.hs <- (Qs.hs/(Xs.hs * teseme.I2hs["ITE2.hs"] * teseme.I2hs["IME2.hs"]))/MP2.hs
                          ISME2.hs <- teseme.I2hs["IME2.hs"] * RISE2.hs
                          RME2.hs <- TFPE2.hs/teseme.I2hs["ITE2.hs"]/teseme.I2hs["ISE2.hs"]
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.it, rts)
                          names(teseme.Iit) <- c("ITE.it", "ISE.it", "IME.it")
                          RISE.it <- (AO.it/(AI.it * teseme.Iit["ITE.it"] * teseme.Iit["IME.it"]))/MP.it
                          ISME.it <- teseme.Iit["IME.it"] * RISE.it
                          RME.it <- TFPE.it/teseme.Iit["ITE.it"]/teseme.Iit["ISE.it"]
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2.it", "ISE2.it", "IME2.it")
                          RISE2.it <- (Qs.it/(Xs.it * teseme.I2it["ITE2.it"] * teseme.I2it["IME2.it"]))/MP2.it
                          ISME2.it <- teseme.I2it["IME2.it"] * RISE2.it
                          RME2.it <- TFPE2.it/teseme.I2it["ITE2.it"]/teseme.I2it["ISE2.it"]
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ihs, 
                                      RISE.hs = unname(RISE.hs), ISME.hs =  unname(ISME.hs), RME.hs =  unname(RME.hs),
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, teseme.I2hs["IME2.hs"], RISE2.hs =  unname(RISE2.hs), ISME2.hs =  unname(ISME2.hs),
                                      RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Iit,
                                      RISE.it =  unname(RISE.it), ISME.it =  unname(ISME.it), RME.it =  unname(RME.it),
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, teseme.I2it["IME2.it"], RISE2.it =  unname(RISE2.it), ISME2.it =  unname(ISME2.it),
                                      RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ihs, 
                                      RISE.hs = unname(RISE.hs), ISME.hs =  unname(ISME.hs), RME.hs =  unname(RME.hs),
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, teseme.I2hs["IME2.hs"], RISE2.hs =  unname(RISE2.hs), ISME2.hs =  unname(ISME2.hs),
                                      RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Iit,
                                      RISE.it =  unname(RISE.it), ISME.it =  unname(ISME.it), RME.it =  unname(RME.it),
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, teseme.I2it["IME2.it"], RISE2.it =  unname(RISE2.it), ISME2.it =  unname(ISME2.it),
                                      RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it)  
                          }
                        } else {
                          teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts)
                          names(teseme.Ohs) <- c("OTE.hs", "OSE.hs", "OME.hs")
                          ROSE.hs <- ((AO.hs/(teseme.Ohs["OTE.hs"] * teseme.Ohs["OME.hs"]))/AI.hs)/MP.hs
                          OSME.hs <- teseme.Ohs["OME.hs"] * ROSE.hs
                          RME.hs <- TFPE.hs/teseme.Ohs["OTE.hs"]/teseme.Ohs["OSE.hs"]
                          teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts)
                          names(teseme.O2hs) <- c("OTE2.hs", "OSE2.hs", "OME2.hs")
                          ROSE2.hs <- ((Qs.hs/(teseme.O2hs["OTE2.hs"] * teseme.O2hs["OME2.hs"]))/Xs.hs)/MP2.hs
                          OSME2.hs <- teseme.O2hs["OME2.hs"] * ROSE2.hs
                          RME2.hs <- TFPE2.hs/teseme.O2hs["OTE2.hs"]/teseme.O2hs["OSE2.hs"]
                          teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts)
                          names(teseme.Oit) <- c("OTE.it", "OSE.it", "OME.it")
                          ROSE.it <- ((AO.it/(teseme.Oit["OTE.it"] * teseme.Oit["OME.it"]))/AI.it)/MP.it
                          OSME.it <- teseme.Oit["OME.it"] * ROSE.it
                          RME.it <- TFPE.it/teseme.Oit["OTE.it"]/teseme.Oit["OSE.it"]
                          teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts)
                          names(teseme.O2it) <- c("OTE2.it", "OSE2.it", "OME2.it")
                          ROSE2.it <- ((Qs.it/(teseme.O2it["OTE2.it"] * teseme.O2it["OME2.it"]))/Xs.it)/MP2.it
                          OSME2.it <- teseme.O2it["OME2.it"] * ROSE2.it
                          RME2.it <- TFPE2.it/teseme.O2it["OTE2.it"]/teseme.O2it["OSE2.it"]
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts)
                          names(teseme.Ihs) <- c("ITE.hs", "ISE.hs", "IME.hs")
                          RISE.hs <- (AO.hs/(AI.hs * teseme.Ihs["ITE.hs"] * teseme.Ihs["IME.hs"]))/MP.hs
                          ISME.hs <- teseme.Ihs["IME.hs"] * RISE.hs
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2.hs", "ISE2.hs", "IME2.hs")
                          RISE2.hs <- (Qs.hs/(Xs.hs * teseme.I2hs["ITE2.hs"] * teseme.I2hs["IME2.hs"]))/MP2.hs
                          ISME2.hs <- teseme.I2hs["IME2.hs"] * RISE2.hs
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.it, rts)
                          names(teseme.Iit) <- c("ITE.it", "ISE.it", "IME.it")
                          RISE.it <- (AO.it/(AI.it * teseme.Iit["ITE.it"] * teseme.Iit["IME.it"]))/MP.it
                          ISME.it <- teseme.Iit["IME.it"] * RISE.it
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2.it", "ISE2.it", "IME2.it")
                          RISE2.it <- (Qs.it/(Xs.it * teseme.I2it["ITE2.it"] * teseme.I2it["IME2.it"]))/MP2.it
                          ISME2.it <- teseme.I2it["IME2.it"] * RISE2.it
                          teseme.OIhs <- sqrt(teseme.Ohs * teseme.Ihs)
                          names(teseme.OIhs) <- c("OTE.ITE.hs", "OSE.ISE.hs", "OME.IME.hs")
                          ROSE.RISE.hs <- sqrt(ROSE.hs * RISE.hs)
                          OSME.ISME.hs <- (teseme.OIhs["OME.IME.hs"] * ROSE.RISE.hs)
                          teseme.OI2hs <- sqrt(teseme.O2hs * teseme.I2hs)
                          names(teseme.OI2hs)[3] <- "OME2.IME2.hs"
                          ROSE2.RISE2.hs <- sqrt(ROSE2.hs * RISE2.hs)
                          OSME2.ISME2.hs <- (teseme.OI2hs["OME2.IME2.hs"] * ROSE2.RISE2.hs)
                          teseme.OIit <- sqrt(teseme.Oit * teseme.Iit)
                          names(teseme.OIit) <- c("OTE.ITE.it", "OSE.ISE.it", "OME.IME.it")
                          ROSE.RISE.it <- sqrt(ROSE.it * RISE.it)
                          OSME.ISME.it <- (teseme.OIit["OME.IME.it"] * ROSE.RISE.it)
                          teseme.OI2it <- sqrt(teseme.O2it * teseme.I2it)
                          names(teseme.OI2it)[3] <- "OME2.IME2.it"
                          ROSE2.RISE2.it <- sqrt(ROSE2.it * RISE2.it)
                          OSME2.ISME2.it <- (teseme.OI2it["OME2.IME2.it"] * ROSE2.RISE2.it)
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.OIhs,
                                      ROSE.RISE.hs = unname(ROSE.RISE.hs), OSME.ISME.hs = unname(OSME.ISME.hs), 
                                      RME.hs = unname(RME.hs), Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = unname(teseme.OI2hs["OME2.IME2.hs"]),
                                      ROSE2.RISE2.hs = unname(ROSE2.RISE2.hs), OSME2.ISME2.hs = unname(OSME2.ISME2.hs), RME2.hs = unname(RME2.hs), 
                                      PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.OIit,
                                      ROSE.RISE.it = unname(ROSE.RISE.it), OSME.ISME.it = unname(OSME.ISME.it), 
                                      RME.it = unname(RME.it), Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = unname(teseme.OI2it["OME2.IME2.it"]),
                                      ROSE2.RISE2.it = unname(ROSE2.RISE2.it), OSME2.ISME2.it = unname(OSME2.ISME2.it), RME2.it = unname(RME2.it), 
                                      PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.OIhs,
                                      ROSE.RISE.hs = unname(ROSE.RISE.hs), OSME.ISME.hs = unname(OSME.ISME.hs), 
                                      RME.hs = unname(RME.hs), Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = unname(teseme.OI2hs["OME2.IME2.hs"]),
                                      ROSE2.RISE2.hs = unname(ROSE2.RISE2.hs), OSME2.ISME2.hs = unname(OSME2.ISME2.hs), RME2.hs = unname(RME2.hs), 
                                      PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.OIit,
                                      ROSE.RISE.it = unname(ROSE.RISE.it), OSME.ISME.it = unname(OSME.ISME.it), 
                                      RME.it = unname(RME.it), Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = unname(teseme.OI2it["OME2.IME2.it"]),
                                      ROSE2.RISE2.it = unname(ROSE2.RISE2.it), OSME2.ISME2.it = unname(OSME2.ISME2.it), RME2.it = unname(RME2.it), 
                                      PRICESI.it, PRICESO.it)
                          }
                        }
                      }
                      return(res1)
                    }
  }
  res2
}

## Hicks-Moorsteen without technical change

hm.2 <- function (data, step1, ano, year.vec, rts, orientation, parallel,
                  components, mean.x, mean.y, itt, it) {
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
  ## all periods reference
  XREFs <- t(as.matrix(data[, step1$x.vars]))
  YREFs <- t(as.matrix(data[, step1$y.vars]))
  if (length(step1) == 6) {
    P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
    W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  }
  if (components == FALSE) {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
                        cat(nextElem(it))
                        flush.console()
                      }
                      PRICESO.hs <- DO.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.hs <- DI.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESO.it <- DO.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.it <- DI.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      Qt <- sqrt(sum(PRICESO.hs * Y1[, dmu]) * sum(PRICESO.it * Y1[, dmu]))
                      Xt <- sqrt(sum(PRICESI.hs * X1[, dmu]) * sum(PRICESI.it * X1[, dmu]))
                      Qs <- sqrt(sum(PRICESO.hs * Y2[, dmu]) * sum(PRICESO.it * Y2[, dmu]))
                      Xs <- sqrt(sum(PRICESI.hs * X2[,dmu]) * sum(PRICESI.it * X2[,dmu]))
                      AO <- Qt
                      AI <- Xt
                      TFP <- AO / AI
                      MP <- sqrt(D.tfp (XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, 
                                        YREF = YREFs, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                   D.tfp (XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, 
                                          YREF = YREFs, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE <- TFP / MP
                      TFP2 <- Qs / Xs
                      MP2 <- sqrt(D.tfp (XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF =YREFs, 
                                         PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                    D.tfp (XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF =YREFs, 
                                           PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE2 <- TFP2 / MP2
                      if (orientation == "out") {
                        teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                        teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                        OME <- sqrt(teseme.Ohs["OME"] * teseme.Oit["OME"])
                        ROSE <- ((AO/(teseme.Ohs["OTE"] * OME))/AI)/MP
                        OSME <- OME * ROSE
                        RME <- TFPE/teseme.Ohs["OTE"]/teseme.Ohs["OSE"]
                        teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                        names(teseme.O2hs) <- c("OTE2", "OSE2", "OME2")
                        teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                        names(teseme.O2it) <- c("OTE2", "OSE2", "OME2")
                        OME2 <- sqrt(teseme.O2hs["OME2"] * teseme.O2it["OME2"])
                        ROSE2 <- ((Qs/(teseme.O2hs["OTE2"] * OME2))/Xs)/MP2
                        OSME2 <- OME2 * ROSE2
                        RME2 <- TFPE2/teseme.O2hs["OTE2"]/teseme.O2hs["OSE2"]
                        if (length(step1) == 6) {
                          REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                          COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                          PROF <- REV/COST
                          P <- REV/AO
                          W <- COST/AI
                          TT <- P/W
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                    TFPE = TFPE, teseme.Ohs[1:2], OME = unname(OME), ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME),
                                    Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = unname(OME2), 
                                    ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2)) 
                        } else {
                          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, teseme.Ohs[1:2], OME = unname(OME), ROSE = unname(ROSE), OSME = unname(OSME), RME = unname(RME),
                                    Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = unname(OME2), 
                                    ROSE2 = unname(ROSE2), OSME2 = unname(OSME2), RME2 = unname(RME2)) 
                        }
                      } else {
                        if (orientation == "in") {
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          IME <- sqrt(teseme.Ihs["IME"] * teseme.Iit["IME"])
                          RISE <- (AO/(AI * teseme.Ihs["ITE"] * IME))/MP
                          ISME <- IME * RISE
                          RME <- TFPE/teseme.Ihs["ITE"]/teseme.Ihs["ISE"]
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2", "ISE2", "IME2")
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2", "ISE2", "IME2")
                          IME2 <- sqrt(teseme.I2hs["IME2"] * teseme.I2it["IME2"])
                          RISE2 <- (Qs/(Xs * teseme.I2hs["ITE2"] * IME2))/MP2
                          ISME2 <- IME2 * RISE2
                          RME2 <- TFPE2/teseme.I2hs["ITE2"]/teseme.I2hs["ISE2"]
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P <- REV/AO
                            W <- COST/AI
                            TT <- P/W
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, teseme.Ihs[1:2], IME = unname(IME), RISE = unname(RISE), ISME = unname(ISME), RME = unname(RME),
                                      Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = unname(IME2), 
                                      RISE2 = unname(RISE2), ISME2 = unname(ISME2), RME2 = unname(RME2))
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, teseme.Ihs[1:2], IME = unname(IME), RISE = unname(RISE), ISME = unname(ISME), RME = unname(RME),
                                      Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = unname(IME2), 
                                      RISE2 = unname(RISE2), ISME2 = unname(ISME2), RME2 = unname(RME2)) 
                          }
                        } else {
                          teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                          teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                          OME <- sqrt(teseme.Ohs["OME"] * teseme.Oit["OME"])
                          ROSE <- ((AO/(teseme.Ohs["OTE"] * OME))/AI)/MP
                          OSME <- OME * ROSE
                          RME <- TFPE/teseme.Ohs["OTE"]/teseme.Ohs["OSE"]
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          IME <- sqrt(teseme.Ihs["IME"] * teseme.Iit["IME"])
                          RISE <- (AO/(AI * teseme.Ihs["ITE"] * IME))/MP
                          ISME <- IME * RISE
                          teseme.OIhs <- sqrt(teseme.Ohs * teseme.Ihs) #OTE OSE ITE ISE are the same for both hs and it indices
                          names(teseme.OIhs)[1:2] <- c("OTE.ITE", "OSE.ISE")
                          OME.IME <- sqrt(OME * IME)
                          ROSE.RISE <- sqrt(ROSE * RISE)
                          OSME.ISME <- OME.IME * ROSE.RISE
                          teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                          names(teseme.O2hs) <- c("OTE2", "OSE2", "OME2")
                          teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                          names(teseme.O2it) <- c("OTE2", "OSE2", "OME2")
                          OME2 <- sqrt(teseme.O2hs["OME2"] * teseme.O2it["OME2"])
                          ROSE2 <- ((Qs/(teseme.O2hs["OTE2"] * OME2))/Xs)/MP2
                          OSME2 <- OME2 * ROSE2
                          RME2 <- TFPE2/teseme.O2hs["OTE2"]/teseme.O2hs["OSE2"]
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2", "ISE2", "IME2")
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2", "ISE2", "IME2")
                          IME2 <- sqrt(teseme.I2hs["IME2"] * teseme.I2it["IME2"])
                          RISE2 <- (Qs/(Xs * teseme.I2hs["ITE2"] * IME2))/MP2
                          ISME2 <- IME2 * RISE2
                          teseme.OI2hs <- sqrt(teseme.O2hs * teseme.I2hs)
                          OME2.IME2 <- sqrt(OME2 * IME2)
                          ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
                          OSME2.ISME2 <- OME2.IME2 * ROSE2.RISE2
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P <- REV/AO
                            W <- COST/AI
                            TT <- P/W
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, teseme.OIhs[1:2], OME.IME = unname(OME.IME), ROSE.RISE = unname(ROSE.RISE), OSME.ISME = unname(OSME.ISME), 
                                      RME = unname(RME), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = unname(OME2.IME2), ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2),
                                      RME2 = unname(RME2))
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, teseme.OIhs[1:2], OME.IME = unname(OME.IME), ROSE.RISE = unname(ROSE.RISE), OSME.ISME = unname(OSME.ISME), 
                                      RME = unname(RME), Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = unname(OME2.IME2), ROSE2.RISE2 = unname(ROSE2.RISE2), OSME2.ISME2 = unname(OSME2.ISME2),
                                      RME2 = unname(RME2))
                          }
                        }
                      }
                      return(res1)
                    }
  } else {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (nrow(data) > 99 & parallel == FALSE & ((ano-1)*nrow(data[data[, step1$time.var] == year.vec[ano], ])+dmu) %in% itt) {
                        cat(nextElem(it))
                        flush.console()
                      }
                      PRICESO.hs <- DO.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.hs <- DI.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESO.it <- DO.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.it <- DI.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      Qt.hs <- sum(PRICESO.hs * Y1[, dmu])
                      Qs.hs <- sum(PRICESO.hs * Y2[, dmu])
                      Xt.hs <- sum(PRICESI.hs * X1[, dmu])
                      Xs.hs <- sum(PRICESI.hs * X2[,dmu])
                      Qt.it <- sum(PRICESO.it * Y1[, dmu])
                      Qs.it <- sum(PRICESO.it * Y2[, dmu])
                      Xt.it <- sum(PRICESI.it * X1[, dmu])
                      Xs.it <- sum(PRICESI.it * X2[,dmu])
                      AO.hs <- Qt.hs
                      AI.hs <- Xt.hs
                      TFP.hs <- AO.hs/AI.hs
                      MP.hs <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE.hs <- TFP.hs/MP.hs
                      TFP2.hs <- Qs.hs/Xs.hs
                      MP2.hs <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE2.hs <- TFP2.hs/MP2.hs
                      AO.it <- Qt.it
                      AI.it <- Xt.it
                      TFP.it <- AO.it/AI.it
                      MP.it <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE.it <- TFP.it/MP.it
                      TFP2.it <- Qs.it/Xs.it
                      MP2.it <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE2.it <- TFP2.it/MP2.it
                      if (orientation == "out") {
                        teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                        names(teseme.Ohs) <- c("OTE.hs", "OSE.hs", "OME.hs")
                        ROSE.hs <- ((AO.hs/(teseme.Ohs["OTE.hs"] * teseme.Ohs["OME.hs"]))/AI.hs)/MP.hs
                        OSME.hs <- teseme.Ohs["OME.hs"] * ROSE.hs
                        RME.hs <- TFPE.hs/teseme.Ohs["OTE.hs"]/teseme.Ohs["OSE.hs"]
                        teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                        names(teseme.O2hs) <- c("OTE2.hs", "OSE2.hs", "OME2.hs")
                        ROSE2.hs <- ((Qs.hs/(teseme.O2hs["OTE2.hs"] * teseme.O2hs["OME2.hs"]))/Xs.hs)/MP2.hs
                        OSME2.hs <- teseme.O2hs["OME2.hs"] * ROSE2.hs
                        RME2.hs <- TFPE2.hs/teseme.O2hs["OTE2.hs"]/teseme.O2hs["OSE2.hs"]
                        teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                        names(teseme.Oit) <- c("OTE.it", "OSE.it", "OME.it")
                        ROSE.it <- ((AO.it/(teseme.Oit["OTE.it"] * teseme.Oit["OME.it"]))/AI.it)/MP.it
                        OSME.it <- teseme.Oit["OME.it"] * ROSE.it
                        RME.it <- TFPE.it/teseme.Oit["OTE.it"]/teseme.Oit["OSE.it"]
                        teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                        names(teseme.O2it) <- c("OTE2.it", "OSE2.it", "OME2.it")
                        ROSE2.it <- ((Qs.it/(teseme.O2it["OTE2.it"] * teseme.O2it["OME2.it"]))/Xs.it)/MP2.it
                        OSME2.it <- teseme.O2it["OME2.it"] * ROSE2.it
                        RME2.it <- TFPE2.it/teseme.O2it["OTE2.it"]/teseme.O2it["OSE2.it"]
                        if (length(step1) == 6) {
                          REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                          COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                          PROF <- REV/COST
                          P.hs <- REV/AO.hs
                          W.hs <- COST/AI.hs
                          TT.hs <- P.hs/W.hs
                          P.it <- REV/AO.it
                          W.it <- COST/AI.it
                          TT.it <- P.it/W.it
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                    TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ohs, 
                                    ROSE.hs = unname(ROSE.hs), OSME.hs =  unname(OSME.hs), RME.hs =  unname(RME.hs),
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, teseme.O2hs["OME2.hs"], ROSE2.hs =  unname(ROSE2.hs), OSME2.hs =  unname(OSME2.hs),
                                    RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Oit,
                                    ROSE.it =  unname(ROSE.it), OSME.it =  unname(OSME.it), RME.it =  unname(RME.it),
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, teseme.O2it["OME2.it"], ROSE2.it =  unname(ROSE2.it), OSME2.it =  unname(OSME2.it),
                                    RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it) 
                        } else {
                          res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ohs, 
                                    ROSE.hs = unname(ROSE.hs), OSME.hs =  unname(OSME.hs), RME.hs =  unname(RME.hs),
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, teseme.O2hs["OME2.hs"], ROSE2.hs =  unname(ROSE2.hs), OSME2.hs =  unname(OSME2.hs),
                                    RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Oit,
                                    ROSE.it =  unname(ROSE.it), OSME.it =  unname(OSME.it), RME.it =  unname(RME.it),
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, teseme.O2it["OME2.it"], ROSE2.it =  unname(ROSE2.it), OSME2.it =  unname(OSME2.it),
                                    RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it) 
                        }
                      } else {
                        if (orientation == "in") {
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          names(teseme.Ihs) <- c("ITE.hs", "ISE.hs", "IME.hs")
                          RISE.hs <- (AO.hs/(AI.hs * teseme.Ihs["ITE.hs"] * teseme.Ihs["IME.hs"]))/MP.hs
                          ISME.hs <- teseme.Ihs["IME.hs"] * RISE.hs
                          RME.hs <- TFPE.hs/teseme.Ihs["ITE.hs"]/teseme.Ihs["ISE.hs"]
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2.hs", "ISE2.hs", "IME2.hs")
                          RISE2.hs <- (Qs.hs/(Xs.hs * teseme.I2hs["ITE2.hs"] * teseme.I2hs["IME2.hs"]))/MP2.hs
                          ISME2.hs <- teseme.I2hs["IME2.hs"] * RISE2.hs
                          RME2.hs <- TFPE2.hs/teseme.I2hs["ITE2.hs"]/teseme.I2hs["ISE2.hs"]
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          names(teseme.Iit) <- c("ITE.it", "ISE.it", "IME.it")
                          RISE.it <- (AO.it/(AI.it * teseme.Iit["ITE.it"] * teseme.Iit["IME.it"]))/MP.it
                          ISME.it <- teseme.Iit["IME.it"] * RISE.it
                          RME.it <- TFPE.it/teseme.Iit["ITE.it"]/teseme.Iit["ISE.it"]
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2.it", "ISE2.it", "IME2.it")
                          RISE2.it <- (Qs.it/(Xs.it * teseme.I2it["ITE2.it"] * teseme.I2it["IME2.it"]))/MP2.it
                          ISME2.it <- teseme.I2it["IME2.it"] * RISE2.it
                          RME2.it <- TFPE2.it/teseme.I2it["ITE2.it"]/teseme.I2it["ISE2.it"]
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ihs, 
                                      RISE.hs = unname(RISE.hs), ISME.hs =  unname(ISME.hs), RME.hs =  unname(RME.hs),
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, teseme.I2hs["IME2.hs"], RISE2.hs =  unname(RISE2.hs), ISME2.hs =  unname(ISME2.hs),
                                      RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Iit,
                                      RISE.it =  unname(RISE.it), ISME.it =  unname(ISME.it), RME.it =  unname(RME.it),
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, teseme.I2it["IME2.it"], RISE2.it =  unname(RISE2.it), ISME2.it =  unname(ISME2.it),
                                      RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.Ihs, 
                                      RISE.hs = unname(RISE.hs), ISME.hs =  unname(ISME.hs), RME.hs =  unname(RME.hs),
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, teseme.I2hs["IME2.hs"], RISE2.hs =  unname(RISE2.hs), ISME2.hs =  unname(ISME2.hs),
                                      RME2.hs =  unname(RME2.hs), PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.Iit,
                                      RISE.it =  unname(RISE.it), ISME.it =  unname(ISME.it), RME.it =  unname(RME.it),
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, teseme.I2it["IME2.it"], RISE2.it =  unname(RISE2.it), ISME2.it =  unname(ISME2.it),
                                      RME2.it =  unname(RME2.it), PRICESI.it, PRICESO.it)  
                          }
                        } else {
                          teseme.Ohs <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                          names(teseme.Ohs) <- c("OTE.hs", "OSE.hs", "OME.hs")
                          ROSE.hs <- ((AO.hs/(teseme.Ohs["OTE.hs"] * teseme.Ohs["OME.hs"]))/AI.hs)/MP.hs
                          OSME.hs <- teseme.Ohs["OME.hs"] * ROSE.hs
                          RME.hs <- TFPE.hs/teseme.Ohs["OTE.hs"]/teseme.Ohs["OSE.hs"]
                          teseme.O2hs <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)
                          names(teseme.O2hs) <- c("OTE2.hs", "OSE2.hs", "OME2.hs")
                          ROSE2.hs <- ((Qs.hs/(teseme.O2hs["OTE2.hs"] * teseme.O2hs["OME2.hs"]))/Xs.hs)/MP2.hs
                          OSME2.hs <- teseme.O2hs["OME2.hs"] * ROSE2.hs
                          RME2.hs <- TFPE2.hs/teseme.O2hs["OTE2.hs"]/teseme.O2hs["OSE2.hs"]
                          teseme.Oit <- DO.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                          names(teseme.Oit) <- c("OTE.it", "OSE.it", "OME.it")
                          ROSE.it <- ((AO.it/(teseme.Oit["OTE.it"] * teseme.Oit["OME.it"]))/AI.it)/MP.it
                          OSME.it <- teseme.Oit["OME.it"] * ROSE.it
                          RME.it <- TFPE.it/teseme.Oit["OTE.it"]/teseme.Oit["OSE.it"]
                          teseme.O2it <- DO.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)
                          names(teseme.O2it) <- c("OTE2.it", "OSE2.it", "OME2.it")
                          ROSE2.it <- ((Qs.it/(teseme.O2it["OTE2.it"] * teseme.O2it["OME2.it"]))/Xs.it)/MP2.it
                          OSME2.it <- teseme.O2it["OME2.it"] * ROSE2.it
                          RME2.it <- TFPE2.it/teseme.O2it["OTE2.it"]/teseme.O2it["OSE2.it"]
                          teseme.Ihs <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          names(teseme.Ihs) <- c("ITE.hs", "ISE.hs", "IME.hs")
                          RISE.hs <- (AO.hs/(AI.hs * teseme.Ihs["ITE.hs"] * teseme.Ihs["IME.hs"]))/MP.hs
                          ISME.hs <- teseme.Ihs["IME.hs"] * RISE.hs
                          teseme.I2hs <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)
                          names(teseme.I2hs) <- c("ITE2.hs", "ISE2.hs", "IME2.hs")
                          RISE2.hs <- (Qs.hs/(Xs.hs * teseme.I2hs["ITE2.hs"] * teseme.I2hs["IME2.hs"]))/MP2.hs
                          ISME2.hs <- teseme.I2hs["IME2.hs"] * RISE2.hs
                          teseme.Iit <- DI.teseme(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          names(teseme.Iit) <- c("ITE.it", "ISE.it", "IME.it")
                          RISE.it <- (AO.it/(AI.it * teseme.Iit["ITE.it"] * teseme.Iit["IME.it"]))/MP.it
                          ISME.it <- teseme.Iit["IME.it"] * RISE.it
                          teseme.I2it <- DI.teseme(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)
                          names(teseme.I2it) <- c("ITE2.it", "ISE2.it", "IME2.it")
                          RISE2.it <- (Qs.it/(Xs.it * teseme.I2it["ITE2.it"] * teseme.I2it["IME2.it"]))/MP2.it
                          ISME2.it <- teseme.I2it["IME2.it"] * RISE2.it
                          teseme.OIhs <- sqrt(teseme.Ohs * teseme.Ihs)
                          names(teseme.OIhs) <- c("OTE.ITE.hs", "OSE.ISE.hs", "OME.IME.hs")
                          ROSE.RISE.hs <- sqrt(ROSE.hs * RISE.hs)
                          OSME.ISME.hs <- (teseme.OIhs["OME.IME.hs"] * ROSE.RISE.hs)
                          teseme.OI2hs <- sqrt(teseme.O2hs * teseme.I2hs)
                          names(teseme.OI2hs)[3] <- "OME2.IME2.hs"
                          ROSE2.RISE2.hs <- sqrt(ROSE2.hs * RISE2.hs)
                          OSME2.ISME2.hs <- (teseme.OI2hs["OME2.IME2.hs"] * ROSE2.RISE2.hs)
                          teseme.OIit <- sqrt(teseme.Oit * teseme.Iit)
                          names(teseme.OIit) <- c("OTE.ITE.it", "OSE.ISE.it", "OME.IME.it")
                          ROSE.RISE.it <- sqrt(ROSE.it * RISE.it)
                          OSME.ISME.it <- (teseme.OIit["OME.IME.it"] * ROSE.RISE.it)
                          teseme.OI2it <- sqrt(teseme.O2it * teseme.I2it)
                          names(teseme.OI2it)[3] <- "OME2.IME2.it"
                          ROSE2.RISE2.it <- sqrt(ROSE2.it * RISE2.it)
                          OSME2.ISME2.it <- (teseme.OI2it["OME2.IME2.it"] * ROSE2.RISE2.it)
                          if (length(step1) == 6) {
                            REV <- sum(Y1[, dmu] * mean.y * P1[, dmu])
                            COST <- sum(X1[, dmu] * mean.x * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.OIhs,
                                      ROSE.RISE.hs = unname(ROSE.RISE.hs), OSME.ISME.hs = unname(OSME.ISME.hs), 
                                      RME.hs = unname(RME.hs), Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = unname(teseme.OI2hs["OME2.IME2.hs"]),
                                      ROSE2.RISE2.hs = unname(ROSE2.RISE2.hs), OSME2.ISME2.hs = unname(OSME2.ISME2.hs), RME2.hs = unname(RME2.hs), 
                                      PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.OIit,
                                      ROSE.RISE.it = unname(ROSE.RISE.it), OSME.ISME.it = unname(OSME.ISME.it), 
                                      RME.it = unname(RME.it), Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = unname(teseme.OI2it["OME2.IME2.it"]),
                                      ROSE2.RISE2.it = unname(ROSE2.RISE2.it), OSME2.ISME2.it = unname(OSME2.ISME2.it), RME2.it = unname(RME2.it), 
                                      PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, teseme.OIhs,
                                      ROSE.RISE.hs = unname(ROSE.RISE.hs), OSME.ISME.hs = unname(OSME.ISME.hs), 
                                      RME.hs = unname(RME.hs), Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = unname(teseme.OI2hs["OME2.IME2.hs"]),
                                      ROSE2.RISE2.hs = unname(ROSE2.RISE2.hs), OSME2.ISME2.hs = unname(OSME2.ISME2.hs), RME2.hs = unname(RME2.hs), 
                                      PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, teseme.OIit,
                                      ROSE.RISE.it = unname(ROSE.RISE.it), OSME.ISME.it = unname(OSME.ISME.it), 
                                      RME.it = unname(RME.it), Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = unname(teseme.OI2it["OME2.IME2.it"]),
                                      ROSE2.RISE2.it = unname(ROSE2.RISE2.it), OSME2.ISME2.it = unname(OSME2.ISME2.it), RME2.it = unname(RME2.it), 
                                      PRICESI.it, PRICESO.it)
                          }
                        }
                      }
                      return(res1)
                    }
  }
res2
}

### Hicks-Moorsteen, print fonction
print.HicksMoorsteen <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  if (length(x) == 2) {
    cat("\nHicks-Moorsteen productivity and profitability levels (summary):\n\n")
    print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\nHicks-Moorsteen productivity and profitability changes (summary):\n\n")
    print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n")
    invisible(x)
  } else {
    cat("\nHicks-Moorsteen productivity and profitability levels (summary):\n\n")
    print(summary(x[["HicksMoorsteen"]][["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\nHicks-Moorsteen productivity and profitability changes (summary):\n\n")
    print(summary(x[["HicksMoorsteen"]][["Changes"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n")
    invisible(x)
  }
}
