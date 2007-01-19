normalHypothesesPlot <- function(){
    initializeDialog(title=gettextRcmdr("Normal Distribution"))
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    criticalVar <- tclVar("1.645")
    criticalEntry <- tkentry(top, width="6", textvariable=criticalVar)
    onOK <- function(){
        closeDialog()
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        critical <- as.numeric(tclvalue(criticalVar))

        ## norm.setup(mean=mu, se=sigma)
        ## norm.curve(mean=mu, se=sigma, critical=mean+critical*se, shade="right", col="black", axis.name="z")

        doItAndPrint("old.par <- par(oma=c(4,0,2,5), mar=c(7,7,4,2)+.1)")

        command <- paste("norm.setup(mean=", mu, " , se=", sigma,
                         if (mu !=0 || sigma != 1) {
                           xlim <- mu + c(-1,1) * 2.5 * sigma
                           paste(", xlim=c(", xlim[1], ", ", xlim[2], ")", sep="")
                         },
                         ")", sep="")
        doItAndPrint(command)
        
        command <- paste("norm.curve(mean=", mu, " , se=", sigma,
                         ", critical=", mu, "+", critical, "*", sigma,
                         ", shade='right', col='black', axis.name='z'", ")")
        doItAndPrint(command)

        doItAndPrint("par(old.par)")

        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dnorm")
    tkgrid(tklabel(top, text=gettextRcmdr("mu (mean)")), muEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("sigma (standard deviation)")), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("z_alpha (critical value)")), criticalEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(criticalEntry, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=muEntry)
    }
