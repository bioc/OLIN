\name{oin}
\alias{oin} 
\title{Optimised intensity-dependent normalisation of two-colour microarrays} 		
\description{This functions performs optimised  intensity-dependent normalisation (OLIN).}
\usage{oin(object,alpha=seq(0.1,1,0.1),weights=NA,bg.corr="subtract",...)}
           \arguments{\item{object}{object of class \dQuote{marrayRaw} or \dQuote{marrayNorm}}
                      \item{alpha}{vector of alpha parameters that are tested in the GCV procedure}
	              \item{weights}{matrix of weights for  local regression. 
                          Rows correspond to the spotted probe sequences, columns to arrays in the batch.
                          These may be derived from the matrix of  spot quality weights as defined 
                          for \dQuote{marrayRaw} objects.}
		     \item{bg.corr}{backcorrection method (for \dQuote{marrayRaw} objects)  :
			\dQuote{none} or \dQuote{subtract}(default).}
                    \item{...}{Further arguments for \code{locfit} function.}	
}         
\details{
The function \code{oin} is  based on iterative local regression of logged fold changes
 in respect to average logged spot intensities. It incorporates optimisation of the smoothing parameter  \code{alpha} 
  that controls the neighbourhood size \emph{h} of local fitting. The parameter \code{alpha} 
specifies the fraction of points that are included in the neighbourhood and thus has a value between 0 and 1.
 Larger \code{alpha} values lead to smoother fits. 

If the normalisation should be based on set of genes assumed to be not differentially expressed (\emph{house-keeping
genes}), weights can be used for local regression. In this case, all weights should be  set to zero except for
the house-keeping genes for which weights are set to one. In order to achieve a reliable regression, it is important, however, that there is a sufficient number of house-keeping genes that are distributed over the whole expression range
and spotted accross the whole array.

In contrast to OLIN and OSLIN, the OIN scheme does
not correct for spatial dye  bias. It can, therefore, be used if the assumption of random spotting does not hold.
}


\value{Object of class \dQuote{marrayNorm} with normalised logged ratios}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link[marray]{maNorm}}, \code{\link[locfit]{locfit}},
  \code{\link[locfit]{gcv}}, \code{\link{olin}}
,\code{\link{lin}}, \code{\link{ino}}}
\examples{


# LOADING DATA
  data(sw)

# OPTIMISED INTENSITY-DEPENDENT NORMALISATION
 norm.oin <- oin(sw)

# MA-PLOT OF NORMALISATION RESULTS OF FIRST ARRAY
 plot(maA(norm.oin)[,1],maM(norm.oin)[,1],main="OIN")
 
# CORRESPONDING MXY-PLOT
  mxy.plot(maM(norm.oin)[,1],Ngc=maNgc(norm.oin),Ngr=maNgr(norm.oin),
                Nsc=maNsc(norm.oin),Nsr=maNsr(norm.oin),main="OIN")

#
}
\keyword{utilities}
\keyword{regression}





