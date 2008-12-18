\name{ma.vector}
\alias{ma.vector} 
\title{Calculation of moving average for a vector} 		
\description{This functions calculates the moving average for a vector.}
\usage{ma.vector(A,M,av="median",delta=50)}
\arguments{\item{A}{vector of predictor to be used for sorting}
            \item{M}{vector of  variable to be averaged}
            \item{av}{averaging by \emph{mean} or \emph{median} (default)}
           \item{delta}{even integer determining the size of the sliding window (\code{2*delta+1}.)}
 }         
\details{The function \code{ma.vector} first sorts M according to the corresponding values of A.
Subsequently, a moving average is calculated with window size  (\code{2*delta+1}). The 
values for the moving average are set to zero if the corresponding window extends
over the boarder of the vector \code{M}.}
\value{Vector with moving average values of \code{M} }
\author{Matthias E. Futschik,\url{http://itb.biologie.hu-berlin.de/~futschik}}
\seealso{\code{\link{ma.matrix}}}
\examples{
### LOADING DATA
data(sw)
A <- maA(sw[,1])
M <- maM(sw[,1])
  
# MA-PLOT 
plot(A,M)

# MOVING AVERAGE
Mav <- ma.vector(A,M,av="median",delta=100)
points(A,Mav,col="red")
}



\keyword{utilities}
\keyword{regression}





