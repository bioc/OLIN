\name{anovaplate}
\alias{anovaplate}
\title{One-factorial ANOVA assessing pin-dependent bias}
\description{This function performs an one-factorial analysis of variance assessing  microtiter plate-dependent bias 
for a single array}
\usage{anovaplate(obj,index)}
\arguments{\item{obj}{object of class \dQuote{marrayRaw} or \dQuote{marrayNorm}} 
           \item{index}{index of array to be tested }
           
}
\details{The function \code{anovapin} performs a one-factorial ANOVA for objects of class \dQuote{marrayRaw} or 
         \dQuote{marrayNorm}. The predictor variable is the corresponding plate index as stored in the \code{maPlate} slot of \code{obj};
           the response variable is the logged fold-change    \code{M=(log2(Ch2)-log2(Ch1))}. 
             The null hypothesis is equal  \code{mean(M)} of  groups of spots derived from the same microtiter plate i.e.
             a spot's M does not dependent on the plate of origin. 
             The model formula used is \eqn{M \sim  (plate.index - 1)}{M ~ (plate.index - 1)} (without an intercept term).
  }
\value{The return value is a list of summary statistics of the fitted  model as produced by \code{summary.lm}.
      For example, the squared multiple correlation coefficient \eqn{R^{2}}{R-square} equals the proportion 
       of the variation of \code{M} that can be explained by the variation of plate index (based on the chosen
       ANOVA model.) }
\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{anova}}, \code{\link{summary.lm}}}
\examples{

# CHECK RAW DATA FOR INTENSITY-DEPENDENT BIAS
data(sw)
print(anovapin(sw,index=1))


# CHECK  DATA NORMALISED BY OLIN FOR INTENSITY-DEPENDENT BIAS
data(sw.olin)
print(anovapin(sw.olin,index=1))

}
\keyword{models}
\keyword{regression}





