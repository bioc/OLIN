\name{ma.matrix}
\alias{ma.matrix} 
\title{Calculation of moving average for a matrix} 		
\description{Using a sliding square window this function produces 
             the moving average for a matrix.}
\usage{ma.matrix(X,av="median",delta= 2,edgeNA=FALSE )}
\arguments{\item{X}{matrix}
           \item{av}{averaging by  \emph{mean} or \emph{median} (default)}
           \item{delta}{integer determining the size of the sliding square window 
                        (2*delta+1)x(2*delta+1).}
           \item{edgeNA}{treatment of edges of array: For \code{edgeNA=TRUE},
               averaged values of sliding windows are set to NA 
               if the corresponding 
               windows extend over the edges of the matrix.} 
}
\details{A square window with size (2*delta+1)x(2*delta+1) is moved over the entire
matrix and a new matrix is created with each value equals the average value in the corresponding
window. This procedure defines a local regression of zeroth order. 
}
\value{Matrix with average values of matrix \code{X}. }
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{ma.vector}}
\examples{

### LOADING DATA
data(sw)

### GENERATION OF MATRIX 
 Morig <- v2m(maM(sw)[,1],Ngc=maNgc(sw),Ngr=maNgr(sw),Nsc=maNsc(sw),Nsr=maNsr(sw),visu=TRUE)

### AVERAGING BY MA.MATRIX
 Mav <- ma.matrix(Morig,av="median",delta= 2,edgeNA=FALSE )

### VISUALISATION 
 m2v(Mav,Ngc=maNgc(sw),Ngr=maNgr(sw),Nsc=maNsc(sw),Nsr=maNsr(sw),visu=TRUE)

}
\keyword{utilities}
\keyword{regression}


