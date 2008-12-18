\name{v2m}	
\alias{v2m} 
\title{Converts  vector to matrix based on spot layout}
\description{This functions converts a vector to a matrix based on a given spot layout. Optionally,
it produces a 2D-plot.}
\usage{v2m(V,Ngc,Ngr,Nsc,Nsr,visu=FALSE,color.lim=c(-1,1),xlab="Columns",ylab="Rows",...) }
\arguments{
      \item{V}{vector of real values }
      \item{Ngc}{number of columns for the grid matrix}
      \item{Ngr}{number of rows for the grid matrix}
      \item{Nsc}{number of columns for the spot matrix}
      \item{Nsr}{number of rows for the spot matrix}
      \item{visu}{If FALSE, MXY plot is generated.} 
      \item{color.lim}{Limits of color range for MXY plot}
      \item{xlab}{label of x -axis of MXY plot}
      \item{ylab}{label of y-axis of MXY plot}
      \item{...}{Further optional parameters  for the \code{image} function generating the MXY plot}
}
\details{The function \code{v2m} converts a vector \code{V} (as e.g. derived by \code{maM(object)[,index]}) to a matrix representing
        the spatial distribution of the values of \code{V} across the array. 
         Note that this function assumes a specific mapping between the data points and the location of spot (i.e.
         the same mapping rule that is used for marrayRaw/marrayNorm objects.) The validity of this mapping should be 
         carefully checked (see also the documentation of packet \emph{marray}.)  
         The option for spatial visualisation
         is rather restricted to logged fold-changes as the corresponding colour range is centred around zero and
         follows the conventional colouring (green for negative, red for positive fold-changes). The MXY plot produced
         by \code{v2n} does not include a colour bar. To have a colour included, you can use \code{mxy.plot}.  
         }

\value{ A 2D-matrix with \code{(Ngc*Nsc)} columns and \code{(Ngr*Nsr)} is produced. This matrix represents the spatial
        distribution of the values of vector \code{V} given the print-layout. }
\author{Matthias E. Futschik, \url{http://itb.biologie.hu-berlin.de/~futschik}}
\seealso{\code{\link{mxy.plot}}, \code{\link{m2v}}, \code{\link[marray]{marrayRaw}}}
\examples{

# LOADING DATA NOT-NORMALISED
data(sw.olin) 
# CONVERSION FROM VECTOR TO MATRIX
M <- v2m(maM(sw.olin)[,1],Ngc=maNgc(sw.olin),Ngr=maNgr(sw.olin),
         Nsc=maNsc(sw.olin),Nsr=maNsr(sw.olin),visu=TRUE)

# BACK-CONVERSION FROM MATRIX TO VECTOR
V <- m2v(M,Ngc=maNgc(sw.olin),Ngr=maNgr(sw.olin),
             Nsc=maNsc(sw.olin),Nsr=maNsr(sw.olin),visu=TRUE)
}

\keyword{manip}



