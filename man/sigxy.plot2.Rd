\name{sigxy.plot2}	
\alias{sigxy.plot2} 
\title{Visualisation of significance tests for spatial bias}
\description{This function produces a 2D-plot visualizing the significance
             of spatial bias.}

\usage{sigxy.plot2(object,Sp,Sn,color.lim=c(-3,3),...) }
\arguments{\item{object}{object of class marrayRaw or marrayNorm}
                \item{Sp}{vector of false discovery rates or p-values  for positive deviation of 
                 \eqn{\bar{M}}{median/mean of \code{M}}
		 as produced by \code{fdrspatial} or \code{p.spatial}}
      \item{Sn}{vector of false discovery rate or p-values for negative deviation of \eqn{\bar{M}}{median/mean of \code{M}}
                as produced by \code{fdrspatial} or \code{p.spatial}}
      \item{color.lim}{limits of color range for plotting vector corresponding to log10(\code{pS}) and log10(\code{nS})}
      \item{...}{Further optional graphical parameter for the \code{image} function generating the MXY plot}
}
\details{The function \code{sigxy.plot2} differs from \code{sigxy.plot} in its input arguments.
        The functionality is the same. For details, see \code{\link{sigxy.plot}}. }
\note{This function will be merged with \code{sigxy.plot} in future versions.}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{colorbar.sig}}, \code{\link{sigxy.plot}},
\code{\link{sigxy.plot},\link{fdr.spatial2}}, \code{\link{p.spatial2}}, \code{\link{image}}}
\examples{

# To run these examples, "un-comment" them!
#
# LOADING DATA
# data(sw)
# CALCULATION OF SIGNIFICANCE OF SPOT NEIGHBOURHOODS
# For this illustration, N was chosen rather small. For "real" analysis, it should be larger.
# FDR <- fdr.spatial2(sw,delta=2,N=10,av="median",edgeNA=TRUE)
#
# SIGNIFICANCE PLOTS OF ARRAY 1
# sigxy.plot2(sw[,1],FDR$FDRp[[1]],FDR$FDRn[[1]],color.lim=c(-5,5),main="FDR")
# SIGNIFICANCE PLOTS OF ARRAY 3
# sigxy.plot2(sw[,3],FDR$FDRp[[3]],FDR$FDRn[[3]],color.lim=c(-5,5),main="FDR")
#

}

\keyword{hplot}


