Commander.HH <-
  function(etc.Menus=file.path(.path.package(package="Rcmdr.HH")[1], "etc"),
           version="Rcmdr.HH_1.4 building on Rcmdr_1.2-6",
           Rcmdr.options) {
    if (!missing(Rcmdr.options)) options(Rcmdr=Rcmdr.options)

    current <- options("Rcmdr")[[1]]
    current$etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
    current$etcMenus <- etc.Menus
    current$Rcmdr.HH <- version
    options(Rcmdr=current)
    Commander()
  }

