\name{sigint.plot}	
\alias{sigint.plot} 
\title{Visualisation of significance of intensity-dependent bias}
\description{This function visualises  the significance
             of intensity-dependent bias.}
 
\usage{sigint.plot(A,M,Sp,Sn,ylim=c(-3,-3),...) }
\arguments{\item{A}{vector of average logged spot intensity}
	  \item{M}{vector of logged fold changes}
      \item{Sp}{vector of false discovery rate  or p-values  for positive deviation of  \eqn{\bar{M}}{median/mean of \code{M}}
		as produced by \code{fdr.int} or \code{p.int} }
      \item{Sn}{vector of false discovery rate or p-values for negative deviation of \eqn{\bar{M}}{median/mean of \code{M}}
                as produced by \code{fdr.int} or \code{p.int}}
      \item{ylim}{vector of minimal log10(fdr) or log10(p-value) to be visualised corresponding to \code{Sp} and \code{Sn}. 
                  FDR or p-values smaller than these values  will be set equal to  these threshold values for visualisation.}
      \item{...}{Further optional graphical parameter for the \code{plot} 
                function generating the MA plot}
}



\details{The function \code{sigint.plot} produces a MA-plot of the significance  (\code{Sp},\code{Sn})
         generated by \code{fdr.int} or \code{p.int}. The abscissa (x-axis) is shows by the average logged spot intensity
        \code{A=0.5*(log(Cy3)+log(Cy5))}; the ordinate axis (y-axis) shows the log10(FDR) or log10(p) given by 
        \code{FDRp} or \code{Pn} and \code{FDRn} or \code{Pn}.
          The significance for positive \eqn{\bar{M}}{median/mean of \code{M}} of 
         spot intensity neighbourhoods are presented by red colour; the significance  for negative 
         \eqn{\bar{M}}{median/mean of \code{M}} of 
         spot intensity neighbourhoods are presented by green colour. The ordinate axis (y-axis)  give the
         log10-transformed FDR or p-values.}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{sigxy.plot}}, \code{\link{fdr.int}},  \code{\link{p.int}} }
\examples{

# To run these examples, "un-comment" them!
#
# LOADING DATA NOT-NORMALISED
# data(sw)
# CALCULATION OF SIGNIFICANCE OF SPOT NEIGHBOURHOODS
# This can take a while! For testing, you may choose a smaller N.  
# FDR <- fdr.int(maA(sw)[,1],maM(sw)[,1],delta=50,N=100,av="median")
# VISUALISATION OF RESULTS
# sigint.plot(maA(sw)[,1],maM(sw)[,1],FDR$FDRp,FDR$FDRn,c(-5,-5))

# data(sw.olin)
# CALCULATION OF SIGNIFICANCE OF SPOT NEIGHBOURHOODS 
# F <- fdr.int(maA(sw.olin)[,1],maM(sw.olin)[,1],delta=50,N=100,av="median")
# VISUALISATION OF RESULTS
# sigint.plot(maA(sw.olin)[,1],maM(sw.olin)[,1],FDR$FDRp,FDR$FDRn,c(-5,-5))


}
\keyword{hplot}


