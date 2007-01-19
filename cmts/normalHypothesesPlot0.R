normalHypothesesPlot <- function() {
  initializeDialog(title=gettextRcmdr("Normal Hyptheses Plot"))

  tDfFrame <- tkframe(top)
    tDfVariable <- tclVar("")
    tDfField <- tkentry(tDfFrame, width="6", textvariable=tDfVariable)

  panelFrame <- tkframe(top)
    muVar <- tclVar(gettextRcmdr("0"))
    muEntry <- tkentry(panelFrame, width="6", textvariable=muVar)
    sigmaVar <- tclVar(gettextRcmdr("1"))
    sigmaEntry <- tkentry(panelFrame, width="6", textvariable=sigmaVar)
    stdErrVar <- tclVar(gettextRcmdr(""))
    stdErrEntry <- tkentry(panelFrame, width="6", textvariable=stdErrVar)
    nVar <- tclVar(gettextRcmdr(""))
    nEntry <- tkentry(panelFrame, width="6", textvariable=nVar)

    xlimLowVar <- tclVar(gettextRcmdr("-2.5"))
    xlimLowEntry <- tkentry(panelFrame, width="6", textvariable=xlimLowVar)
    xlimHighVar <- tclVar(gettextRcmdr("2.5"))
    xlimHighEntry <- tkentry(panelFrame, width="6", textvariable=xlimHighVar)

    ylimLowVar <- tclVar(gettextRcmdr("0"))
    ylimLowEntry <- tkentry(panelFrame, width="6", textvariable=ylimLowVar)
    ylimHighVar <- tclVar(gettextRcmdr(""))
    ylimHighEntry <- tkentry(panelFrame, width="6", textvariable=ylimHighVar)

##  ## Null
##   nullFrame <- tkframe(top)
##     critZLowVar <- tclVar("-1.96")
##     critZLowEntry <- tkentry(nullFrame, width="6", textvariable=critZLowVar)
##     critZHighVar <- tclVar("1.96")
##     critZHighEntry <- tkentry(nullFrame, width="6", textvariable=critZHighVar)

##     critPLowVar <- tclVar(".025")
##     critPLowEntry <- tkentry(nullFrame, width="6", textvariable=critPLowVar)
##     critPHighVar <- tclVar(".975")
##     critPHighEntry <- tkentry(nullFrame, width="6", textvariable=critPHighVar)

##     critXLowVar <- tclVar("")
##     critXLowEntry <- tkentry(nullFrame, width="6", textvariable=critXLowVar)
##     critXHighVar <- tclVar("")
##     critXHighEntry <- tkentry(nullFrame, width="6", textvariable=critXHighVar)

##     radioButtons(name="ShadeNull",
##                  buttons=c("left", "right", "inside", "outside", "none"),
##                  labels=gettextRcmdr(c("left", "right", "inside", "outside", "none")),
##                  title=gettextRcmdr("Shading for Null"))
##   nullColorFrame <- tkframe(nullFrame)
##   nullColorVar <- tclVar("black")
##   nullColorEntry <- tkentry(nullColorFrame, width="10", textvariable=nullColorVar)

##  ## Alternative
##   altFrame <- tkframe(top)
##     muAltVar <- tclVar("")
##     muAltEntry <- tkentry(altFrame, width="6", textvariable=muAltVar)
  
##     critZAltLowVar    <- tclVar("")
##     critZAltLowEntry  <- tkentry(altFrame, width="6", textvariable=critZAltLowVar)
##     critZAltHighVar   <- tclVar("")
##     critZAltHighEntry <- tkentry(altFrame, width="6", textvariable=critZAltHighVar)

##     critPAltLowVar    <- tclVar("")
##     critPAltLowEntry  <- tkentry(altFrame, width="6", textvariable=critPAltLowVar)
##     critPAltHighVar   <- tclVar("")
##     critPAltHighEntry <- tkentry(altFrame, width="6", textvariable=critPAltHighVar)

##     critXAltLowVar    <- tclVar("")
##     critXAltLowEntry  <- tkentry(altFrame, width="6", textvariable=critXAltLowVar)
##     critXAltHighVar   <- tclVar("")
##     critXAltHighEntry <- tkentry(altFrame, width="6", textvariable=critXAltHighVar)

##     radioButtons(name="ShadeAlt",
##                  buttons=c("left", "right", "inside", "outside", "none"),
##         labels=gettextRcmdr(c("left", "right", "inside", "outside", "none")),
##         title=gettextRcmdr("Shading for Alternative"))
##   altColorFrame <- tkframe(altFrame)
##   altColorVar <- tclVar("red")
##   altColorEntry <- tkentry(altColorFrame, width="10", textvariable=altColorVar)


  onOK <- function() {
    closeDialog()

    mu <- as.numeric(tclvalue(muVar))
    sigma <- as.numeric(tclvalue(sigmaVar))
    stdErr <- as.numeric(tclvalue(stdErrVar))
    n <- as.numeric(tclvalue(nVar))

    xlimLow  <- as.numeric(tclvalue(xlimLowVar ))
    xlimHigh <- as.numeric(tclvalue(xlimHighVar))
    ylimLow  <- as.numeric(tclvalue(ylimLowVar ))
    ylimHigh <- as.numeric(tclvalue(ylimHighVar))

##     critZLow  <- as.numeric(tclvalue(critZLowVar ))
##     critZHigh <- as.numeric(tclvalue(critZHighVar))
##     critPLow  <- as.numeric(tclvalue(critPLowVar ))
##     critPHigh <- as.numeric(tclvalue(critPHighVar))
##     critXLow  <- as.numeric(tclvalue(critXLowVar ))
##     critXHigh <- as.numeric(tclvalue(critXHighVar))
##     ShadeNull <- as.character(tclvalue(ShadeNullVariable))

##     muAlt        <- as.numeric(tclvalue(muAltVar       ))
##     critZAltLow  <- as.numeric(tclvalue(critZAltLowVar ))
##     critZAltHigh <- as.numeric(tclvalue(critZAltHighVar))
##     critPAltLow  <- as.numeric(tclvalue(critPAltLowVar ))
##     critPAltHigh <- as.numeric(tclvalue(critPAltHighVar))
##     critXAltLow  <- as.numeric(tclvalue(critXAltLowVar ))
##     critXAltHigh <- as.numeric(tclvalue(critXAltHighVar))
##     ShadeAlt <- as.character(tclvalue(ShadeAltVariable))

##     if (is.na(xlimLow) || is.na(xlimHigh) || is.na(ylimLow) ||
##         xlimLow >= xlimHigh || (!is.na(ylimHigh) && ylimLow >= ylimHigh)) {
##       errorCondition(recall=normalHypothesesPlot,
##          message=gettextRcmdr("xlim and ylim must go from small to large"))
##       return()
##     }

##     if (sigma < 0 ||  stdErr < 0 || n < 1) {
##       errorCondition(recall=normalHypothesesPlot,
##                      message=gettextRcmdr("standard error < 0 or n < 1"))
##       return()
##     }

##     if (is.na(mu)) mu <- 0
##     if (mu <= xlimLow || xlimHigh <= mu) {
##       Message(recall=normalHypothesesPlot,
##               message=gettextRcmdr("mu not between x limits"),
##               type="warning")
##       return()
##     }

##     ## norm.setup(xlim.in=c(xlimLow, xlimHigh),
##     ##            ylim.in = c(0, 0.4)/se,
##     ##            mean=0,
##     ##            se=sd/sqrt(n), sd=1, n=1, ...)
##     command <- paste("norm.setup(xlim=c(", xlimLow, ",", xlimHigh, ")")
##     if (ylimLow!=0 && !is.na(ylimHigh))
##       command <- paste(command, ", ylim=c(", ylimLow, ",", ylimHigh, ")")

##     command.se <- ""
##     if (!(is.na(n) && is.na(stdErr) && (is.na(sigma) || sigma==1))) {
##       if (!is.na(stdErr))
##         command.se <- paste(command.se, ", se=", stdErr)
##       else {
##         if (is.na(sigma) || is.na(n)) {
##           errorCondition(recall=normalHypothesesPlot,
##                          message=gettextRcmdr("standard deviation or n missing"))
##           return()
##         }
##         else {
##           command.se <- paste(command.se, ", sd=", sigma, ", n=", n)
##           stdErr <- sigma/sqrt(n)
##         }
##       }
##     }
##     command <- paste(command, command.se, ")")
##     doItAndPrint(command)


##     ## null
##     critNull <- matrix(c(critZLow, critZHigh,
##                          critPLow, critPHigh,
##                          critXLow, critXHigh),
##                        nrow=3, ncol=2, byrow=TRUE)
##     command.nullZ <-
##       if (all(is.na(critNull))) ""
##       else {
##         zpx <- match(FALSE, apply(is.na(critNull), 1, all))
##         crit.lh <- critNull[zpx,]
##         crit.lh <- crit.lh[!is.na(crit.lh)]
##         cv <- switch(zpx,
##                      mean + se*crit.lh,
##                      mean + se*qnorm(crit.lh),
##                      crit.lh)
##         paste(", critical.values=", cv[1],
##               if (length(cv)==2) paste(",", cv[2]),
##               ")")
##       }

##     nullColor <- tclvalue(nullColorVar)
##     command.nullColor <-
##       if (nullColor == "none") ""
##       else  paste(", col=", nullColor)

##     ShadeNull <- as.character(tclvalue(ShadeNullVariable))
##     command.ShadeNull <-  paste(", shade=", ShadeNull)

##     command <- paste("norm.curve(axis.name='z'",
##                      command.nullZ, command.nullColor, command.ShadeNull,
##                      ")")
##     doItAndPrint(command)


    ## alternative
##     if (!is.na(muAlt)) {
##       critAlt <- matrix(c(critZAltLow, critZAltHigh,
##                           critPAltLow, critPAltHigh,
##                           critXAltLow, critXAltHigh),
##                         nrow=3, ncol=2, byrow=TRUE)
##       command.altZ <-
##         if (all(is.na(critAlt))) ""
##         else {
##           zpx <- match(FALSE, apply(is.na(critAlt), 1, all))
##           crit.lh <- critAlt[zpx,]
##           crit.lh <- crit.lh[!is.na(crit.lh)]
##           cv <- switch(zpx,
##                        mean + se*crit.lh,
##                        mean + se*qnorm(crit.lh),
##                        crit.lh)
##           paste(", critical.values=", cv[1],
##                 if (length(cv)==2) paste(",", cv[2]),
##                 ")")
##         }
      
##       altColor <- tclvalue(altColorVariable)
##       command.altColor <-
##         if (altColor == "none") ""
##         else  paste(", col=", altColorVar)
      
##       ShadeAlt <- as.character(tclvalue(ShadeAltVariable))
##       command.ShadeAlt <-  paste(", shade=", ShadeAlt)
      
##       command <- paste("norm.curve(axis.name='z1'",
##                        command.altZ, command.altColor, command.ShadeAlt,
##                        ")")
##       doItAndPrint(command)
##     }
    
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject="norm.curve")


  tkgrid(tklabel(panelFrame, text=gettextRcmdr("Panel Frame")),
         nEntry,
         sticky="w")
##  tkgrid(tklabel(nullFrame, text=gettextRcmdr("Null Hypothesis Frame")),
##         critZHighEntry,
##         sticky="w")


##   tkgrid(panelFrame, sticky="nw")
##   tkgrid(nullFrame, altFrame, columnspan=2, sticky="w")
##   tkgrid(nullColorFrame, altColorFrame, columnspan=2, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=3, columns=2)
}
