\name{mxy.plot}	
\alias{mxy.plot} 
\title{Generation of MXY plots}
\description{This function produce a MXY plot including  a colour bar.}

\usage{mxy.plot(V,Ngc,Ngr,Nsc,Nsr,color.lim=c(-1,1),xlab="Columns",ylab="Rows",...) }
\arguments{\item{V}{vector of real values typically logged ratios \emph{M}. }
      \item{Ngc}{number of columns for the grid matrix}
      \item{Ngr}{number of rows for the grid matrix}
      \item{Nsc}{number of columns for the spot matrix}
      \item{Nsr}{number of rows for the spot matrix}
      \item{color.lim}{limits of color range for MXY plot}
      \item{xlab}{label of x -axis of MXY plot}
      \item{ylab}{label of y-axis of MXY plot}
      \item{...}{Further optional graphical parameter for the \code{image} function generating the MXY plot}
}
\details{Spotted microarrays have generally a grid layout of form with 
         \code{Ngc} columns and \code{Ngr} rows. Each block (or spot matrix)  
         of the grid corresponds to a specific pin used for spotting.
         The blocks have generally \code{Nsc} columns  and \code{Nsr} rows.  
         The function \code{mxy.plot} generates a  2D-plot (MXY-plot) of the values of \emph{M} across the
         array. \emph{M} is given in form of the vector V. 
         Note that this function assumes a specific mapping between the data points and the location of spot (i.e.
         the same mapping rule that is used for marrayRaw/marrayNorm objects (see the documentation of packet marray)
        The colour range of the MXY plot is centred around zero and
         follows the conventional colouring (green for negative, red for positive fold-changes). For a separate visualisation\
         of the two channels, see function \code{fgbg.visu}.}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{v2m}}, \code{\link{m2v}}, \code{\link{fgbg.visu}}, \code{\link{image}}, \code{\link[marray]{marrayRaw}}}
\examples{
# LOADING DATA
data(sw)
# PLOTTING
mxy.plot(maM(sw)[,1],Ngc=maNgc(sw),Ngr=maNgr(sw), Nsc=maNsc(sw),Nsr=maNsr(sw))

}
\keyword{hplot}


