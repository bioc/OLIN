\name{sig.mask}	
\alias{sig.mask} 
\title{Masking of data based on significance testing}
\description{This function sets data to NA if the corresponding spots have significantly biased 
neighbourhoods on the intensity scale or on the spatial dimensions of the array.} 
\usage{sig.mask(object,Sp,Sn,thrp,thrn) }

\arguments{\item{object}{object of class \code{marrayRaw} or \code{marrrayNorm}}
	  \item{Sp}{list of vectors of false discovery rate  or p-values  for positive deviation of  \eqn{\bar{M}}{median/mean of \code{M}}
		as produced by \code{fdr.int2, p.int2, fdr.spatial2} or \code{p.spatial2}.}
      \item{Sn}{list vector of false discovery rate  or p-values  for negative deviation of  \eqn{\bar{M}}{median/mean of \code{M}}
		as produced by  \code{fdr.int2, p.int2, fdr.spatial2} or \code{p.spatial2}. }
      \item{thrp}{vector of  thresholds for significance of positive deviation (\code{Sp})}
      \item{thrn}{vector of  thresholds for significance of negative deviation (\code{Sn})}
}


\details{This function can be used for the masking of data that has been decided to be unrelaible
after the application of significance test for intenstiy- and location dependent dye bias (e.g. \code{p.int2, fdr.int2,
p.spatial2, fdr.spatial2}).}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{sigint.plot}}, \code{\link{fdr.int}},  \code{\link{p.int}}, \code{\link{sigxy.plot}},
\code{\link{fdr.spatial}},  \code{\link{p.spatial}}}


\examples{

# To run these commands, delete comment sign (#) !
#
# LOADING DATA
# data(sw)
#  
# MASKING REGIONS WITH SPATIAL DYE BIAS
#  
# CALCULATION OF SIGNIFICANCE OF SPOT NEIGHBOURHOODS
# For this example, N was chosen rather small. For "real" analysis, it should be larger.
# FDR <- fdr.spatial2(sw,delta=2,N=10,av="median",edgeNA=FALSE)
#
# VISUALISATION
# sigxy.plot2(sw[,1],FDR$FDRp[[1]],FDR$FDRn[[1]],color.lim=c(-5,5),main="FDR")
# 
# MASKING SIGNIFICANT NEIGHBOURHOODS
# thresp <- c(0.01,0.01,0.01,0.01)
# thresn <- c(0.01,0.01,0.01,0.01)
# sw.masked <- sig.mask(sw,Sp=FDR$FDRp,Sn=FDR$FDRn,thrp=thresp,thrn=thresn)
# mxy.plot(sw.masked[,4]) # plot masked data for array 4







}
\keyword{hplot}

