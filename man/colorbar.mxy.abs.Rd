\name{colorbar.mxy.abs}
\alias{colorbar.mxy.abs}
\title{Generates a colour bar}
\description{Generates colour bar for 2D plots of absolute values}
\usage{colorbar.mxy.abs(color.lim,color="red", ylab="",ylablim=FALSE)}
\arguments{\item{color.lim}{limits for colour range}
	\item{color}{colour to be used: \dQuote{red} or \dQuote{green}}
	\item{ylab}{label of y-axis}
        \item{ylablim}{If TRUE, the axis annotation consists only of the limits of the colour range.} 
	}
\details{The function \code{colorbar.mxy.abs} is a modification of  \code{colorbar.mxy} 
         to provide colour-bars for MXY plots of absolutes values. It is used in function 
         \code{\link{mxy.abs.plot}.}
         Further details can be found at  \code{\link{colorbar.mxy}}. }
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{mxy.abs.plot}},  \code{\link{colorbar.mxy}}, \code{\link{colorbar.sig}}}

\keyword{hplot}
 
