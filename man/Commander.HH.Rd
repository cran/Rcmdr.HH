\name{Commander.HH}
\alias{Commander.HH}

\title{Start the R Commander with HH additions}
\description{
  Start the R Commander GUI (graphical user interface) with the HH additions.
}
\usage{
## reads menu from elsewhere and
## adds the Rcmdr.HH version number to options("Rcmdr")
Commander.HH(etc.Menus=file.path(.path.package(package="Rcmdr.HH")[1], "etc"),
             version=packageDescription("Rcmdr.HH", fields="Version"),
             Rcmdr.options)
}

\arguments{
  \item{etc.Menus}{Alternate menu location, defaults to \code{Rcmdr/etc} in
    \code{Commander} and defaults to \code{Rcmdr.HH/etc} in \code{Commander.HH}.}
  \item{version}{version number, accessible at runtime as \code{RcmdrVersion}.}
  \item{Rcmdr.options}{Optional list of options.  The user enters, for example,
    \code{Rcmdr.options=list(console.output=TRUE)}.
    This generates a value that can be accessed as \code{options$Rcmdr}}.
      See \code{\link[Rcmdr]{Commander}}.}

}

\details{
  This sets up options and then calls \code{\link[Rcmdr]{Commander}}.

  The menu is read from \code{Rcmdr.HH/etc} instead of \code{Rcmdr/etc}.

  If the Rcmdr window is already open, then it gives a warning
  and the existing Rcmdr window stays open.  This means that the
  additional menu items are not available.  The user must explicitly
  close the existing Rcmdr window and then run \code{Commander.HH()} to
  get the additional menu items.

}


\author{Changes by Richard M. Heiberger <rmh@temple.edu>, based on
  original by John Fox \email{jfox@mcmaster.ca}}


\examples{
\dontrun{
   ## start R
   library(Rcmdr.HH)        ## loads the package and opens the Rcmdr
                            ## window with the HH menu

   Commander()              ## Once, Rcmdr.HH has been loaded, this
                            ## command will restart Rcmdr.HH.

   options(Rcmdr=NULL); Commander()  ## back to the original Rcmdr

   Commander.HH()           ## Rcmdr startup with HH menu

   Commander.HH(Rcmdr.options=list(console.output=TRUE))
                            ## Send output to console instead of
                            ## to Rcmdr output window.
}
}

\keyword{misc}
