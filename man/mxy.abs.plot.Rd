\name{mxy.abs.plot}	
\alias{mxy.abs.plot} 
\title{Generation of MXY plots of absolute values}
\description{This function produce a MXY plot of absolute values of M including a colour bar.}

\usage{mxy.abs.plot(V,Ngc,Ngr,Nsc,Nsr,color.lim,color="red",xlab="Columns",ylab="Rows",...) }
\arguments{\item{V}{vector of positive values}
      \item{Ngc}{number of columns for the grid matrix}
      \item{Ngr}{number of rows for the grid matrix}
      \item{Nsc}{number of columns for the spot matrix}
      \item{Nsr}{number of rows for the spot matrix}
      \item{color.lim}{limits of color range for MXY plot}
      \item{color}{color to be used for plot: \dQuote{red} (default) or \dQuote{green}}
      \item{xlab}{label of x -axis of MXY plot}
      \item{ylab}{label of y-axis of MXY plot}
      \item{...}{Further optional graphical parameter for the \code{image} function generating the MXY plot}
}
\details{The function \code{mxy.abs.plot} is similar to function \code{mxy.plot}. Details can therefore
         be found at \code{\link{mxy.plot}}. Two differences, however, exist: First, \code{mxy.abs.plot} 
         plots the absolute value of V and second,  \dQuote{red} (default) or \dQuote{green} can be chosen
        as  colour of plotting. Hence,  \code{mxy.abs.plot} facilitates  the inspection of 
        spatial artifacts in single fluorescence channels.}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link{v2m}}, \code{\link{m2v}}, \code{\link{colorbar.mxy.abs}}, }\code{\link{fgbg.visu}}, \code{\link{image}}}
\examples{
# LOADING DATA
data(sw)
# PLOTTING OF ABSOLUTE LOGGED FOLD-CHANGES
mxy.plot(abs(maM(sw)[,1]),Ngc=maNgc(sw),Ngr=maNgr(sw),Nsc=maNsc(sw),Nsr=maNsr(sw))

# PLOTTING SPATIAL DISTRIBUTION OF SINGLE-CHANNEL INTENSITIES
mxy.abs.plot(maRf(sw)[,1],color.lim=c(0,10000),Ngc=maNgc(sw),Ngr=maNgr(sw),
             Nsc=maNsc(sw),Nsr=maNsr(sw))
mxy.abs.plot(maGf(sw)[,1],color.lim=c(0,10000),color="green",Ngc=maNgc(sw),Ngr=maNgr(sw),
             Nsc=maNsc(sw),Nsr=maNsr(sw))
}
\keyword{hplot}


