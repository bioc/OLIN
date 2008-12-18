\name{anovaspatial}
\alias{anovaspatial}
\title{One-factorial ANOVA assessing spatial bias}
\description{This function performs an one-factorial analysis of variance to test for spatial bias 
for a single array. The predictor variable is the average logged intensity of both channels
and the response variable is the logged fold-change.}
\usage{anovaspatial(obj,index,xN=5,yN=5,visu=FALSE)}
\arguments{\item{obj}{object of class  \dQuote{marrayRaw} or \dQuote{marrayNorm}}
         \item{index}{index of array (within \code{obj}) to be tested }
	\item{xN}{number of intervals in x-direction}
	\item{yN}{number of intervals in y-direction}
	\item{visu}{If visu=TRUE, results are visualised (see below)} 
}
\details{The function \code{anovaspatial} performs a one-factorial ANOVA for objects of class \dQuote{marrayRaw} or 
         \dQuote{marrayNorm}. The predictor  variable is the average logged intensity of both channels
          (\code{A=0.5*(log2(Ch1)+log2(Ch2))}). \code{Ch1,Ch2} are the fluorescence intensities of channel 1
            and channel 2, respectively. The response variable is the logged fold-change  
            (\code{M=(log2(Ch2)-log2(Ch1))}). The spot locations  on the array is divided into \code{xN} intervals
             in x-direction and \code{yN} intervals in y-direction. This division defines  (\code{xN x yN}) 
            rectangular spatial blocks on 
             the array, and thus, (\code{xN x yN}) levels (or treatments) for \code{A}.  Note that 
              values chosen for \code{xN} and \code{yN}  should  divide the array columns and rows approx. equally.
              The null hypothesis is the equality of mean(\code{M}) of the different levels. 
              The model formula used by \code{anovaspatial} is \eqn{M \sim  (A - 1)}{M ~ (A - 1)} 
            (without an intercept term).}          
\value{The return value is a list of summary statistics of the fitted  model as produced by \code{summary.lm}.  
       For example, the squared multiple correlation coefficient \eqn{R^{2}}{R-square} equals the proportion 
       of the variation of \code{M} that can be related to the spot location (based on the chosen
       ANOVA.)  Optionally, the distribution of p-values (as derived by t-test and stated in the summary statistics) 
       can be  visualised.}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{anova}}, \code{\link{summary.lm}},
  \code{\link{anovaint}}, \code{\link[marray]{marrayRaw}},
  \code{\link[marray]{marrayNorm}}}

\examples{
# CHECK RAW DATA FOR SPATIAL BIAS
data(sw)
print(anovaspatial(sw,index=1,xN=8,yN=8,visu=TRUE))


# CHECK  DATA NORMALISED BY OLIN FOR SPATIAL BIAS
data(sw.olin)
print(anovaspatial(sw.olin,index=1,xN=8,yN=8,visu=TRUE)) 
# note the different scale of the colour bar

}
\keyword{models}
\keyword{regression}


