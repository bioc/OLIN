\name{backgroundCorrect2}
\alias{backgroundCorrect2}
\title{Background correction}
\description{Background correction based on
  \code{\link[limma:backgroundcorrect]{backgroundCorrect}} of the \code{limma} package.}

\usage{backgroundCorrect2(object,method="subtract", offset=0)}
\arguments{\item{object}{object of class \code{\link[marray:marrayRaw-class]{marrayRaw}}} 
  \item{method}{method for background correction:  \dQuote{none},
  \dQuote{subtract}, \dQuote{half},
  \dQuote{minimum}, \dQuote{movingmin}, \dQuote{edwards} or
  \dQuote{normexp}.
}
\item{offset}{numeric value to add to intensities}

}


\details{This function is a wrapper function for
\code{\link[limma:backgroundcorrect]{backgroundCorrect}}
with following methods implemented:
\itemize{
  \item \dQuote{none}: no background correction
  \item \dQuote{subtract}: simple subtraction of background intensities
  \item \dQuote{movingmin}: background intensities are first averaged over 3x3 grids
  of neighbouring spots and subsequently substracted
  \item \dQuote{minimum}: zero or negative intensities after background
        correction are set equal to half the minimum of positive
        corrected intensities
 \item \dQuote{edwards}: background correction based on  log-linear
       interpolation 
 \item \dQuote{normexp}: background correction based on fitting
       procedure 
}
  For further details and references, please refer to its help page. An
  alternative Bayesian model for background correction
  (\code{\link[limma]{kooperberg}}) is also implemented
  in the \code{limma} package.
}

\value{Background correct object of class \code{marrayRaw}.}

\author{Matthias Futschik}

\seealso{\code{\link[limma:backgroundcorrect]{backgroundCorrect}},\code{\link[limma:kooperberg]{kooperberg}}}

\examples{

# Loading data
data(sw)

#No background correction
  sw.none <- backgroundCorrect2(sw,method="none")
  plot(maA(sw.none)[,1],maM(sw.none)[,1])

# Simple subtraction
  sw.sub <- backgroundCorrect2(sw,method="sub")
  points(maA(sw.sub)[,1],maM(sw.sub)[,1],col="red")
 
}
\keyword{utilities}




