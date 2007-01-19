\name{closeCommander}
\alias{closeCommander}

\title{close R Commander with option to close without querying user}
\description{
  Close the R Commander GUI (graphical user interface), with option
  to close without querying the user about saving files.  A warning is
  issued if the R Commander is not open.
}
\usage{
closeCommander(exitQ=TRUE,
               scriptQ=exitQ,
               outputQ=exitQ)
}

\arguments{
  \item{exitQ}{Should we query user to confirm exiting Rcmdr?}
  \item{scriptQ}{Should we query user for a filename to save the
    Rcmdr script window?}
  \item{outputQ}{Should we query user for a filename to save the
    Rcmdr output window?}
}

\details{
  The default argument values \code{TRUE} give the same bahavior as
  the original \code{closeCommander}.  The alternate value \code{FALSE}
  forces Rcmdr to close without querying the user and without saving any
  windows.

  The objects \code{"commanderWindow"}, \code{"logWindow"},
  \code{"messagesWindow"}, \code{"outputWindow"} in the
  \code{"RcmdrEnv"} are set to \code{NULL}.
}


\author{Changes by Richard M. Heiberger <rmh@temple.edu>, based on
  original by John Fox \email{jfox@mcmaster.ca}}


\examples{
\dontrun{
   closeCommander()  ## default behavior
   closeCommander(exitQ=FALSE)  ## force no queries and no saves
}
}

\keyword{misc}
