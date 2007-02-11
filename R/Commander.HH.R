Commander.HH <-
  function(etc.Menus=file.path(.path.package(package="Rcmdr.HH")[1], "etc"),
           version=packageDescription("Rcmdr.HH", fields="Version"),
           Rcmdr.options) {
    if (!missing(Rcmdr.options)) options(Rcmdr=Rcmdr.options)

    current <- options("Rcmdr")[[1]]
    current$etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
    current$etcMenus <- etc.Menus
    current$Rcmdr.HH <- version
    options(Rcmdr=current)
    Commander()
    Message(paste(gettextRcmdr("R Commander HH Version "), version, sep=""))
}
