"twoWayTable.HH" <-
function(){
    require("abind")
    initializeDialog(title=gettextRcmdr("Two-Way Table"))
    variablesFrame <- tkframe(top)
    .factors <- Factors()
    rowBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Row variable (pick one)"))
    columnBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Column variable (pick one)"))
    subsetBox()
    onOK <- function() {
        row <- getSelection(rowBox)
        column <- getSelection(columnBox)
        if (length(row) == 0 || length(column) == 0){
            errorCondition(recall=twoWayTable.HH, message=gettextRcmdr("You must select two variables."))
            return()
            }
        if (row == column) {
            errorCondition(recall=twoWayTable.HH, message=gettextRcmdr("Row and column variables are the same."))
            return()
            }        

        rowPct <- tclvalue(rowPercentsVariable)
        colPct <- tclvalue(colPercentsVariable)
        totPct <- tclvalue(totPercentsVariable)
##        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTestVariable)
        chisqComp <- tclvalue(chisqCompVariable)
        chiComp <- tclvalue(chiCompVariable)
        expected <- tclvalue(expFreqVariable)
        fisher <- tclvalue(fisherTestVariable)

        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")
        closeDialog()

        command <- paste("xtabs(~", row, "+", column, ", data=", ActiveDataSet(),
            subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")

        if (rowPct == 1) doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (colPct == 1) doItAndPrint("colPercents(.Table) # Column Percentages")        
        if (totPct == 1) doItAndPrint("totPercents2.HH(.Table) # Total Percentages")        

        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(".Test")
            if (chisqComp == 1) doItAndPrint("round(.Test$residuals^2, 2) # Chi-square Components")
            if (chiComp == 1) doItAndPrint("round(.Test$residuals, 2) # Chi Components (residuals)")
            if (expected == 1) doItAndPrint("round(.Test$expected, 2) # Expected Counts")
##            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
            warnText <- NULL
            if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
                gettextRcmdr("expected frequencies are less than 1"))
            if (0 < (nlt5 <- sum(.Test$expected < 5))) warnText <- paste(warnText, "\n", nlt5,
                gettextRcmdr(" expected frequencies are less than 5"), sep="")
            if (!is.null(warnText)) Message(message=warnText,
                type="warning")
            logger("remove(.Test)") 
            remove(.Test, envir=.GlobalEnv) 
            }
        if (fisher == 1) doItAndPrint("fisher.test(.Table)")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)                                                      
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="xtabs")

    checkBoxes(frame="percentsFrame",
               boxes=c("rowPercents",
                 "colPercents",
                 "totPercents"), 
               initialValues=c("0","0","0"), 
               labels=gettextRcmdr(c(
                 "Row percentages",
                 "Column percentages",
                 "Total percentages")))
    
##   radioButtons(name="percents",
##                buttons=c("rowPercents",
##                  "columnPercents",
##                  "nonePercents"), 
##                values=c("row", "column", "none"),
##                initialValue="none", 
##                labels=gettextRcmdr(c
##                  ("Row percentages",
##                   "Column percentages",
##                   "No percentages")),
##                title=gettextRcmdr("Compute Percentages"))
    
    checkBoxes(frame="testsFrame",
               boxes=c(
                 "chisqTest",
                 "chisqComp",
                 "chiComp",
                 "expFreq",
                 "fisherTest"),
               initialValues=c("1", "1", "0", "0", "0"),
               labels=gettextRcmdr(c(
                 "Chi-square test of independence",
                 "Print chi-square components",
                 "Print chi components (residuals)",
                 "Print expected frequencies",
                 "Fisher's exact test")))

##     checkBoxes(frame="testsFrame",
##                boxes=c("chisqTest", "expFreq", "fisherTest"), initialValues=c("1", "0", "0"),
##                labels=gettextRcmdr(
##                  c("Chi-square test of independence",
##                    "Print expected frequencies",
##                    "Fisher's exact test")))

    tkgrid(getFrame(rowBox),
           tklabel(variablesFrame, text="    "),
           getFrame(columnBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Compute Percentages"), fg="blue"), sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(top,
                   text=gettextRcmdr("Hypothesis Tests"),
                   fg="blue"),
           sticky="w")
    tkgrid(testsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
  }

