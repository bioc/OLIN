\name{colorbar.mxy}
\alias{colorbar.mxy}
\title{Generates a colour bar}
\description{Generates colour bar for MXY plots}
\usage{colorbar.mxy(color.lim,
       col=c(rgb(0,(100:0)/100,0),rgb(0,0,0),rgb((1:100)/100,green=0,blue=0)),
       ylab="",ylablim=FALSE)}
\arguments{\item{color.lim}{limits for colour range}
	\item{col}{colour palette to be used}
	\item{ylab}{label of ordinate of color bar}
        \item{ylablim}{If TRUE, the axis annotation consists only of the limits of the colour range.} 
	}
\details{The function \code{colorbar.mxy} produces a colour bar for MXY plots.
         The default colours used range from green (for the lower limit of the colour range)
         to red (for its upper limit).
         For visualisation, values below or above the limits for the colour range 
         (as given by \code{color.lim}) are set
         to the lower or upper limit, respectively.}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{mxy.plot}}, \code{\link{colorbar.sig}}}
\examples{

data(sw)

# GENERATING LAYOUT
mat <- matrix(1:2,ncol=2,nrow=1,byrow=TRUE)
l   <- layout(mat,widths=c(5,1))
  
# CHOOSING LIMITS OF COLOUR RANGE
color.lim <- c(-2,2)
  
# PLOTTING
Mtmp <- v2m(maM(sw)[,1],Ngc=maNgc(sw),Ngr=maNgr(sw),Nsc=maNsc(sw),Nsr=maNsr(sw),
               visu=TRUE,color.lim=color.lim)
colorbar.mxy(color.lim=color.lim,ylablim=FALSE,ylab="M")
}
\keyword{hplot}
 
