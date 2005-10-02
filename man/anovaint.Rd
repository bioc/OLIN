\name{anovaint}
\alias{anovaint}
\title{One-factorial ANOVA assessing intensity-dependent bias}
\description{This function performs an one-factorial analysis of variance assessing  intensity-dependent bias 
for a single array. The predictor variable is the average logged intensity of both channels
and the response variable is the logged fold-change.}
\usage{anovaint(obj,index,N=10)}
\arguments{\item{obj}{object of class \dQuote{marrayRaw} or \dQuote{marrayNorm}} 
           \item{index}{index of array to be tested }
           \item{N}{number of (intensity) levels for ANOVA} 
}
\details{The function \code{anovaint} performs a one-factorial ANOVA for objects of class \dQuote{marrayRaw} or 
         \dQuote{marrayNorm}. The predictor variable is the average logged intensity of both channels
          \code{A=0.5*(log2(Ch1)+log2(Ch2))}. \code{Ch1,Ch2} are the fluorescence intensities of channel 1
            and channel 2, respectively. The response variable is the logged fold-change  
            \code{M=(log2(Ch2)-log2(Ch1))}. The \code{A}-scale is divided in \code{N} intervals generating
                 \code{N} levels of factor \code{A}. Note that 
               \code{N} should divide the total number of spots  approx. equally.
             The null hypothesis is the equality of \code{mean(M)} of the different levels (intervals). 
             The model formula used is \eqn{M \sim  (A - 1)}{M ~ (A - 1)} (without an intercept term).
  }
\value{The return value is a list of summary statistics of the fitted  model as produced by \code{summary.lm}.
      For example, the squared multiple correlation coefficient \eqn{R^{2}}{R-square} equals the proportion 
       of the variation of \code{M} that can be explained by the variation of \code{A} (based on the chosen
       ANOVA model.) }
\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{anova}}, \code{\link{summary.lm}}, 
         \code{\link{anovaspatial}}, \code{\link[marray]{marrayRaw}}, \code{\link[marray]{marrayNorm}}}
\examples{

# CHECK RAW DATA FOR INTENSITY-DEPENDENT BIAS
data(sw)
print(anovaint(sw,index=1,N=10))


# CHECK  DATA NORMALISED BY OLIN FOR INTENSITY-DEPENDENT BIAS
data(sw.olin)
print(anovaint(sw.olin,index=1,N=10))



}
\keyword{models}
\keyword{regression}





