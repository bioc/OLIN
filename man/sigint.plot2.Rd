\name{sigint.plot2}	
\alias{sigint.plot2} 
\title{Visualisation of significance of intensity-dependent bias}
\description{This function produce visualises  the significance
             of intensity-dependent bias.}
 
\usage{sigint.plot2(object,Sp,Sn,ylim=c(-3,-3),...) }
\arguments{\item{object}{object of class marrayRaw or marrayNorm}
      \item{Sp}{vector of false discovery rate  or p-values  for positive deviation of  \eqn{\bar{M}}{median/mean of \code{M}}
		as produced by \code{fdr.int2} or \code{p.int2} }
      \item{Sn}{vector of false discovery rate or p-values for negative deviation of \eqn{\bar{M}}{median/mean of \code{M}}
                as produced by \code{fdr.int2} or \code{p.int2}}
      \item{ylim}{vector of minimal log10(fdr) or log10(p-value) to be visualised corresponding to \code{Sp} and \code{Sn}. 
                  FDR or p-values smaller than these values  will be set equal to  these threshold values for visualisation.}
      \item{...}{Further optional graphical parameter for the \code{plot} 
                function generating the MA plot}
}



\details{The function \code{sigint.plot2} only differs from \code{sigint.plot} in its input arguments.
        The functionality is the same. For details, see \code{\link{sigint.plot}}. }
\note{This function will be merged with \code{sigint.plot} in future versions.}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{sigxy.plot}}, \code{\link{fdr.int2}},  \code{\link{p.int2}} }
\examples{
# To run these examples, delete the comment signs (#) in front of the commands.
#
# LOADING DATA NOT-NORMALISED
# data(sw)
# CALCULATION OF SIGNIFICANCE OF SPOT NEIGHBOURHOODS
# For this example, N was chosen rather small. For "real" analysis, it should be larger.
# FDR <- fdr.int2(sw,delta=50,N=10,av="median")
# VISUALISATION OF RESULTS
# sigint.plot2(sw[,1],FDR$FDRp[[1]],FDR$FDRn[[1]],c(-5,-5)) # array 1
# sigint.plot2(sw[,4],FDR$FDRp[[4]],FDR$FDRn[[4]],c(-5,-5)) # array 4

}
\keyword{hplot}


