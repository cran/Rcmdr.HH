\name{HH_regsubsets}
\alias{summary_HH}
\alias{summary_HH.regsubsets}
\alias{print.summary_HH_regsubsets}
\alias{plot.summary_HH_regsubsets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Display tabular results for Best Subsets Regression.}
\description{
  Print a tabular display of the results of Best Subsets Regression.
  This is an alternate display for the object from the \code{regsubsets}
  function.  This function is based on \code{\link[leaps]{regsubsets}}.
}
\usage{

summary_HH(object, ...)

\method{summary_HH}{regsubsets}(object,
           names = abbreviate(dimnames(incidence)[[2]], minlength = abbrev),
           abbrev = 1, min.size = 1, max.size = dim(sumry$which)[2],
           statistic = c("bic", "cp", "adjr2", "rsq", "rss", "stderr"),
           las = par("las"), 
           cex.subsets = 1, ..., main=statistic)

\method{plot}{summary_HH_regsubsets}(x, ...,
           statistic="adjr2", legend=FALSE,
           col="darkgray", cex=1, pch=16,
           col.text="black", cex.text=1, col.abline="darkgray")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ An object of class \code{"regsubsets"}.}
  \item{x}{ An object of class \code{"summary_HH_regsubsets"}.}
  \item{statistic}{Name of statistic to be plotted for each model.}
  \item{\dots}{ Other arguments to be passed down to 
    \code{subsets.regsubsets} and \code{plot}.}

  \item{names}{Abbreviations of variable names.}
  \item{abbrev}{minimum number of letters in each abbreviation.}
  \item{min.size}{minimum size subset to plot; default is 1.}
  \item{max.size}{maximum size subset to plot; default is number of predictors.}
  \item{legend}{logical variable, \code{TRUE} if the legend should be
    printed.
    If the legend is printed, the execution halts until the user clicks an
  empty space in the graph where the legend should be placed.}
  \item{las}{Orientation for model names on graph.}
  \item{cex.subsets}{can be used to change the relative size of the characters used to
    plot the regression subsets; default is \code{1}.}
  \item{main}{\code{"main"} title for graph.}
  \item{col, cex, pch}{\code{par} values for dot locating statistic.}
  \item{col.text, cex.text}{\code{par} values for abbreviations of
  models on plot.}
  \item{col.abline}{\code{par} parameters for \code{abline} when the
  statistic is \code{cp}.}
}
\value{
  \code{summary_HH} produces a table of models, with
  \code{p, rsq, rss, adjr2, cp, bic, stderr} for each.
  \code{plot.summary_HH_regsubsets} plots the specified statistic
  from the summary.
  All the others are support functions.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[leaps]{regsubsets}},
  \code{\link[leaps]{bestSubsetsRegressionModel.HH}}}
\examples{
data(Prestige, package="car")
Subsets.1 <- regsubsets(prestige~census+education+income+women,
                        data=Prestige, nbest=2)
summary_HH(Subsets.1, statistic.plot='cp')}
\keyword{regression}
