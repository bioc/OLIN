\name{p.spatial}
\alias{p.spatial} 
\title{Assessment of the significance of spatial bias based on p-values}
\description{This function assesses the significance of spatial bias.  This is 
achieved by comparing the observed average values of logged fold-changes within a spot's spatial neighbourhood
with an empirical distribution generated by permutation tests. The significance is given
by (adjusted) p-values derived in one-sided permutation test.}
\usage{p.spatial(X,delta=2,N=-1,av="median",p.adjust.method="none")}
\arguments{\item{X}{matrix of logged fold changes}
              \item{delta}{integer determining the size of spot neighbourhoods 
            (\code{(2*delta+1)x(2*delta+1)}).}
           \item{N}{number of samples for  generation of empirical background distribution}
            \item{av}{averaging of \code{M} within neighbourhood by \emph{mean} or \emph{median} (default)}      
           \item{p.adjust.method}{method for adjusting p-values due to multiple testing regime. The available
                 methods are \dQuote{none}, \dQuote{bonferroni},  \dQuote{holm}, \dQuote{hochberg},
                 \dQuote{hommel} and \dQuote{fdr}.  See also \code{\link{p.adjust}}.}}

\details{The function \code{p.spatial} assesses the significance of spatial bias using an one-sided random 
           permutation test.
          The null hypothesis states random spotting i.e. the independence of log ratio \code{M} 
          and spot location. First, a neighbourhood of a spot is defined  by a two dimensional square window 
          of chosen size ((2*delta+1)x(2*delta+1)). Next, a test statistic is defined   by calculating 
           the \emph{median} or \emph{mean} of \code{M} for  \code{N} random  samples
           of size  ((2*delta+1)x(2*delta+1)). Note that this scheme defines a sampling with replacement
           procedure whereas  sampling without replacement is used for \code{fdr.spatial}.  
          Comparing the empirical distribution of  \eqn{\bar{M}}{median/mean of \code{M}}
           with the observed distribution of  \eqn{\bar{M}}{median/mean of \code{M}}, 
           the independence of \code{M} and spot location
           can be  assessed. If \code{M} is independent of spot's location, 
           the empirical distribution can be  expected to be 
           distributed around its mean value. To assess the significance of observing positive deviations of 
           \eqn{\bar{M}}{median/mean of \code{M}},
          p-values are calculated using Fisher's method. The p-value equals the fraction of values in the empirical
          distribution which are larger than the observed value . The minimal p-value is set to  \code{1/N}.  
          Correspondingly, the significance
          of observing negative deviations of \eqn{\bar{M}}{median/mean of \code{M}} can be determined.
  }

\value{A list of vectors containing the p-values  for positive (\code{Pp}) 
        and negative (\code{Pn}) deviations of
       \eqn{\bar{M}}{median/mean of \code{M}} of the spot's neighbourhood is produced.}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{ \code{\link{fdr.int}}, \code{\link{sigxy.plot}}, \code{\link{p.adjust}}}

\examples{

# To run these examples, "un-comment" them!
#
# LOADING DATA
# data(sw)
# M <- v2m(maM(sw)[,1],Ngc=maNgc(sw),Ngr=maNgr(sw),
#                Nsc=maNsc(sw),Nsr=maNsr(sw),main="MXY plot of SW-array 1")
#
# CALCULATION OF SIGNIFICANCE OF SPOT NEIGHBOURHOODS
# For this illustration, N was chosen rather small. For "real" analysis, it should be larger.
# P <- p.spatial(M,delta=2,N=10000,av="median")
# sigxy.plot(P$Pp,P$Pn,color.lim=c(-5,5),main="FDR")

# LOADING NORMALISED DATA
# data(sw.olin)
# M <- v2m(maM(sw.olin)[,1],Ngc=maNgc(sw.olin),Ngr=maNgr(sw.olin),
#                Nsc=maNsc(sw.olin),Nsr=maNsr(sw.olin),main="MXY plot of SW-array 1")

# CALCULATION OF SIGNIFICANCE OF SPOT NEIGHBOURHOODS
# P <- p.spatial(M,delta=2,N=10000,av="median")
# VISUALISATION OF RESULTS
# sigxy.plot(P$Pp,P$Pn,color.lim=c(-5,5),main="FDR")

}



\keyword{nonparametric}
\keyword{univar}
\keyword{htest}



