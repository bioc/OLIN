\name{ino}
\alias{ino}
\title{Intensity-dependent normalisation of two-colour microarrays} 		
\description{This functions performs intensity-dependent normalisation based on local regression by locfit.}
\usage{ino(object,alpha=0.3,weights=NA,bg.corr="sub",...)}
           \arguments{\item{object}{object of class \dQuote{marrayRaw}
           or \dQuote{marrayNorm} }
           \item{alpha}{smoothing parameter}
           \item{weights}{matrix of weights for  local regression. 
                          Rows correspond to the spotted probe sequences, columns to arrays in the batch.
                          These may be derived from the matrix of  spot quality weights as defined 
                          for \dQuote{maRaw} objects.}
	 \item{bg.corr}{backcorrection method (for \dQuote{marrayRaw} objects)  :
			\dQuote{none} or \dQuote{sub}(default).}
          \item{...}{Further arguments for \code{locfit} function.}	
}         
\details{The function \code{ino} regresses the average logged fold changes (\emph{M}) with respect to the average
logged spot intensity (\emph{A}). The residuals of this fit are the normalised logged fold changes.
The parameter \code{alpha} 
specifies the fraction of points that are included in the neighbourhood and thus has a value between 0 and 1.
Larger \code{alpha} values lead to smoother fits.  }


\value{Object of class \dQuote{marrayNorm} with normalised logged ratios}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{maNorm}}, \code{\link{locfit.raw}},\code{\link{olin}}, \code{\link{oin}}, \code{\link{lin}} }
\examples{


# LOADING DATA
  data(sw)

# INTENSITY-DEPENDENT NORMALISATION
  norm.ino <- ino(sw)

# MA-PLOT OF NORMALISATION RESULTS OF FIRST ARRAY
  plot(maA(norm.ino)[,1],maM(norm.ino)[,1],main="INO")
 
# CORRESPONDING MXY-PLOT
   mxy.plot(maM(norm.ino)[,1],Ngc=maNgc(norm.ino),Ngr=maNgr(norm.ino),
                Nsc=maNsc(norm.ino),Nsr=maNsr(norm.ino),main="INO")

}
\keyword{utilities}
\keyword{regression}





