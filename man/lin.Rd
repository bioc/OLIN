\name{lin}
\alias{lin} 
\title{Local intensity-dependent normalisation of two-colour microarrays} 		
\description{This functions performs local intensity-dependent normalisation (LIN)}
\usage{lin(object,X=NA,Y=NA,alpha=0.3,iter=2,scale=TRUE,weights=NA,bg.corr="subtract",...)}
           \arguments{\item{object}{object of class \dQuote{marrayRaw}}
           \item{X}{matrix with x-coordinates of spots. If X=NA, columns on array are used
            as proxies for the location in x-direction}
           \item{Y}{matrix with y-coordinates of spots. If Y=NA, rows on array are used
            as proxies for the location in y-direction}
           \item{alpha}{smooting parameter for local regression}
           \item{iter}{number of iterations in the LIN procedure}
           \item{scale}{scale parameter for smooting in Y-direction of the array in respect to smoothing in 
                        X-direcction. If \code{scale=TRUE}, standard deviations are used. }
    	   \item{weights}{matrix of weights for  local regression. 
                          Rows correspond to the spotted probe sequences, columns to arrays in the batch.
                          These may be derived from the matrix of  spot quality weights as defined 
                          for \dQuote{maRaw} objects.}
	   \item{bg.corr}{backcorrection method (for \dQuote{marrayRaw} objects)  :
			\dQuote{none} or \dQuote{subtract}(default).}
         \item{...}{Further arguments for \code{locfit} function.}		
}         
\details{
LIN  is based on the same normalisation scheme as OLIN, but does not  incorporate 
optimisation of  model  parameters. The function \code{lin} can serve for comparison.
Alternatively, it can be used to enforce a conservative model fit.

The smoothing parameter  \code{alpha} controls the neighbourhood size \emph{h} of local fitting. 
It
specifies the fraction of points that are included in the neighbourhood and, thus, has a value between 0 and 1.
 Larger \code{alpha} values lead to smoother fits. 

If the normalisation should be based on set of genes assumed to be not differentially expressed (\emph{house-keeping
genes}), weights can be used for local regression. In this case, all weights should be  set to zero except for
the house-keeping genes for which weights are set to one. In order to achieve a reliable regression, it is important, however, that there is a sufficient number of house-keeping genes that cover the whole expression range
and are spotted accross the whole array.   


}


\value{Object of class \dQuote{marrayNorm} with normalised logged ratios}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\references{
\enumerate{
\item M.Futschik and T.Crompton (2004) \emph{Model selection and efficiency testing for normalization of cDNA microarray data},
\strong{Genome Biology}, 5:R60
}}
\seealso{\code{\link[marray]{maNorm}}, \code{\link[locfit]{locfit}},  \code{\link{olin}},\code{\link{oin}}}
\examples{



# LOADING DATA
  data(sw)
  data(sw.xy)

# LOCAL INTENSITY-DEPENDENT NORMALISATION 
 norm.lin <- lin(sw,X=sw.xy$X,Y=sw.xy$Y)

# MA-PLOT OF NORMALISATION RESULTS OF FIRST ARRAY
 plot(maA(norm.lin)[,1],maM(norm.lin)[,1],main="LIN")
 
# CORRESPONDING MXY-PLOT
   mxy.plot(maM(norm.lin)[,1],Ngc=maNgc(norm.lin),Ngr=maNgr(norm.lin),
                Nsc=maNsc(norm.lin),Nsr=maNsr(norm.lin),main="LIN")


}
\keyword{utilities}
\keyword{regression}





