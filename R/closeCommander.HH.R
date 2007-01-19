closeCommander <- function(exitQ=TRUE, scriptQ=exitQ, outputQ=exitQ) {
  ## Based on closeCommander.
  ## Default exitQ=TRUE gives same behavior as original.
  ## The alternative
  ##    closeCommander(exitQ=FALSE)
  ## closes Rcmdr with no questions and no saves
  if (!exists("commanderWindow", "RcmdrEnv") ||
      is.null(get("commanderWindow", "RcmdrEnv"))) {
    warning("Rcmdr is not currently open.")
    return(invisible(NULL))
  }
  if (exitQ) {
    response <- tclvalue(RcmdrTkmessageBox(message=gettextRcmdr("Exit?"),
                                           icon="question", type="okcancel",
                                           default="cancel"))
    if (response == "cancel") return(invisible(response))
  }
  sink(type="message")
  if (rglLoaded()) rgl.quit()
  if (!is.null(ActiveDataSet()) && getRcmdr("attach.data.set"))
    justDoIt(logger(paste("detach(", ActiveDataSet(), ")", sep="")))
  putRcmdr(".activeDataSet", NULL)
  putRcmdr(".activeModel", NULL)
  if (scriptQ && getRcmdr("log.commands") &&
      tclvalue(tkget(LogWindow(), "1.0", "end")) != "\n") {
    response2 <- RcmdrTkmessageBox(message=gettextRcmdr("Save script file?"),
                                   icon="question", type="yesno",
                                   default="yes")
    if ("yes" == tclvalue(response2)) saveLog()
  }
  if (outputQ && !getRcmdr("console.output") &&
      tclvalue(tkget(OutputWindow(), "1.0", "end")) != "\n") {
    response3 <- RcmdrTkmessageBox(message=gettextRcmdr("Save output file?"),
                                   icon="question", type="yesno",
                                   default="yes")
    if ("yes" == tclvalue(response3)) saveOutput()
  }
  if (.Platform$OS.type != "windows") options(getRcmdr("oldPager"))
  if (getRcmdr("suppress.X11.warnings")) {
    sink(type="message")
    close(getRcmdr("messages.connection"))
    remove(".messages", envir=.GlobalEnv)
  }
  options(getRcmdr("saveOptions"))
  tkdestroy(CommanderWindow())
  assign("commanderWindow", NULL, pos="RcmdrEnv")
  assign("logWindow",       NULL, pos="RcmdrEnv")
  assign("messagesWindow",  NULL, pos="RcmdrEnv")
  assign("outputWindow",    NULL, pos="RcmdrEnv")
  tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
  if ((!is.null(tkwait)) && tkwait) tclvalue(.commander.done) <<- "1"
  return(invisible(response))
}

environment(closeCommander) <- environment(Rcmdr:::closeCommander)
