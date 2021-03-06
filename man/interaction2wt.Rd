\name{interaction2wt}
\alias{interaction2wt}
\alias{interaction2wt.formula}
\alias{interaction2wt.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot all main effects and twoway interactions in a multifactor design}
\description{
The main diagonal
displays boxplots for the main effects of each factor.  The
off-diagonals show the interaction plots for each pair of factors.
The \code{i,j} panel shows the same factors as the \code{j,i} but with
the trace- and x-factor roles interchanged.
}
\usage{
interaction2wt(x, ...)

\method{interaction2wt}{formula}(x, data = sys.parent(), responselab, ...)

\method{interaction2wt}{default}(x,
               response.var,
               responselab = deparse(substitute(y)),
               relation = list(x = "same", y = "same"),
               x.relation = relation$x,
               y.relation = relation$y,
               digits = 3,
               x.between=if (label.as.interaction.formula) 0 else 1,
               y.between=if (label.as.interaction.formula) 0 else 1,
               between,
               cex = 0.75,
               rot=c(0,0),
               panel.input = panel.interaction2wt,
               strip.input =
                if (label.as.interaction.formula) strip.default
                else strip.interaction2wt,
               par.strip.text.input = list(cex=.7, responselab=responselab),
               scales.additional,
               main.in =
                 paste(responselab,
                       ": main effects and 2-way interactions",
                       sep = ""),
               xlab = list(labels = ""),
               ylab = list(labels = ""),
               simple=FALSE,
               box.ratio=if (simple) .32 else 1,
               label.as.interaction.formula=TRUE,
               ...,
               main.cex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{Arguments when \code{x} is a formula.
  \item{x}{
    The object on which method dispatch is carried out.

    For the \code{"formula"} method, a formula describing the response
    variable and factors.
    The formula is generally of the form \code{y ~ g1 + g2 + \dots}.
    There may be one or more factors in the formula.

    For the \code{"default"} method, \code{data.frame} of factors.  This
    is usually constructed by \code{formula} method from the input
   data and the input formula.
}

  \item{data}{
    For the \code{formula} method, a data frame containing values for
    any variables in the formula.  In the R version,
    if not found in \code{data}, or if
    \code{data} is unspecified, the variables are looked for in the
    environment of the formula.
}
  \item{responselab}{Character name of response variable, defaults to
    the name of the response variable in the \code{formula}.}
  \item{\dots}{additional arguments, primarily trellis arguments.}

  \item{response.var}{For the \code{"default"} method, the
    response variable.  This is usually constructed by
    \code{formula} method from the input data and the input formula.}

  \item{simple}{logical.  \code{TRUE} if simple effects are to be
    displayed.  Arguments \code{simple.offset}, \code{simple.scale}, and
    \code{col.by.row} may also be needed.  See
    \code{\link{panel.interaction2wt}} for details.}
  \item{box.ratio}{\code{\link[lattice]{bwplot}} in R or
    \code{\link[trellis]{bwplot}} in S-Plus.}

Trellis/Lattice arguments.  Default values are set by the
the \code{formula} method.  The user may override the defaults.
See also \code{\link[lattice]{xyplot}} in R and
\code{\link[trellis]{trellis.args}} in S-Plus.

  \item{relation}{\code{trellis} argument.}
  \item{x.relation}{\code{x} value of \code{relation} argument.}
  \item{y.relation}{\code{y} value of \code{relation} argument.}
  \item{digits}{doesn't do anything at the moment}
  \item{x.between}{\code{x} value of \code{between} argument.}
  \item{y.between}{\code{y} value of \code{between} argument.}
  \item{between}{trellis/lattice \code{between} argument.
    If used, \code{between} has precedence over both the
    \code{x.between} and \code{y.between} arguments.}
  \item{cex}{S-Plus: changes the size of the median dot in the
  boxplots.  R: doesn't do anything.}
  \item{panel.input}{panel function.  Default is \code{panel.interaction2wt}.}
  \item{label.as.interaction.formula}{logical.  If \code{TRUE}, each
  panel has a single strip label of the form \code{y ~ a | b}.  If
  \code{FALSE}, each panel has a pair of strip labels, one for the
  \code{trace} factor and one for the \code{x} factor.}
  \item{strip.input}{strip function.  Default depends on the value of
  \code{label.as.interaction.formula}.}
  \item{par.strip.text.input}{\code{par.strip.text} argument.}
  \item{scales.additional}{additional arguments to \code{scales} argument.}
  \item{main.in}{Text of main title.}
%  \item{xlab.in}{S-Plus: FALSE suppresses x labels in trellis.  R: no effect.}
  \item{xlab}{No effect.}
  \item{ylab}{No effect.}
  \item{main.cex}{\code{cex} for main title.}
  \item{rot}{Rotation of x tick labels and y tick labels.  Only 0 and 90
  will look good.}
}
\value{
\code{"trellis"} object containing the plot.
}
\references{ Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{panel.interaction2wt}}}
\examples{
  ToothGrowth <- ToothGrowth  ## local copy
  ToothGrowth$dose <- positioned(ToothGrowth$dose) ## modify local copy
  anova(aov(len ~ supp*dose, data=ToothGrowth))
  interaction2wt(len ~ supp + dose, data=ToothGrowth)
  bwplot(len ~ supp | unpositioned(dose), data=ToothGrowth, layout=c(3,1))
  bwplot(len ~ dose | supp, data=ToothGrowth, layout=c(2,1),
         box.ratio=.5,
         panel=panel.bwplot.intermediate.hh,
         scales=list(x=list(at=position(ToothGrowth$dose))))
  bwplot(len ~ dose | supp, data=ToothGrowth, layout=c(2,1),
         box.ratio=.5,
         scales=list(x=list(at=position(ToothGrowth$dose))))


esoph=esoph
esoph$rate=with(esoph, ncases/ncontrols)

position(esoph$alcgp) <- 2:5
position(esoph$tobgp) <- 2:5

interaction2wt(rate ~ agegp + alcgp + tobgp, esoph, rot=c(90,0))
esoph.aov <- aov(rate ~ agegp + alcgp + tobgp, data=esoph)
anova(esoph.aov)
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ design }% __ONLY ONE__ keyword per line
