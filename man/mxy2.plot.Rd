\name{mxy2.plot}	
\alias{mxy2.plot} 
\title{Generation of MXY plots based on spot coordinates}
\description{This function produce a MXY plot with a colour bar.
            In contrast to \code{mxy.plot}, the plot is  based on spot coordinates (instead on column and row 
            index as proxies for spot location). }

\usage{mxy2.plot(V,X,Y,Ngc,Ngr,Nsc,Nsr,color.lim=c(-1,1),xlab="X",ylab="Y",...) }
\arguments{\item{V}{vector of real values typically logged ratios \emph{M}. }
      \item{X}{vector of x coordinates of spot locations}
      \item{Y}{vector of y coordinates of spot locations}
      \item{Ngc}{number of columns for the grid matrix}
      \item{Ngr}{number of rows for the grid matrix}
      \item{Nsc}{number of columns for the spot matrix}
      \item{Nsr}{number of rows for the spot matrix}
      \item{color.lim}{limits of color range for MXY plot}
      \item{xlab}{label of x-axis of MXY plot}
      \item{ylab}{label of y-axis of MXY plot}
      \item{...}{Further optional graphical parameter for the \code{image} function generating the MXY plot}
}
\details{The function \code{mxy2.plot} can be used to plot the distribution of \code{V} across
the array. As \code{mxy.plot}, it mainly aims for the plotting of the distribution of
logged fold changes. It differs from \code{mxy.plot} in the representation of spot location.
The function \code{mxy.plot} uses the index of columns and rows as proxies for the spot location.
The gaps between the grid matrices (spotted by different pins) are, therefore, not reproduced
in the plot. A more accurate spatial plot is produced by \code{mxy2.plot}, which
is based on the coordinates of the first column and first raw of the array. Assuming a regular rectangular print layout,
gaps and the edges of the array are shown.}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{mxy.plot}}, \code{\link{v2m}}, \code{\link{m2v}}, \code{\link{fgbg.visu}}, \code{\link{image}}}
\examples{
# LOADING DATA
data(sw)
data(sw.xy)
# PLOTTING
mxy2.plot(maM(sw)[,1],X=sw.xy$X[,1],Y=sw.xy$Y[,1], Ngc=maNgc(sw),Ngr=maNgr(sw),
                   Nsc=maNsc(sw),Nsr=maNsr(sw))

}
\keyword{hplot}


