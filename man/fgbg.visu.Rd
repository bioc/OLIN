\name{fgbg.visu}   
\alias{fgbg.visu} 
\title{Visualisation of foreground and background  fluorescence spot intensities in both channels}
\description{This function generates 2D-plots of the foreground, background and background corrected
 fluorescence intensities of channel 1 and of channel 2, respectively. } 
\usage{fgbg.visu(obj,label)}
\arguments{\item{obj}{object of class \dQuote{marrayRaw}}
            \item{label}{character string for labelling. It will be added to the title of the first sub-plot.}
           }
\details{The function \code{fgbg.visu} produces 2D-representations of the foreground 
	 and  background intensities for both fluorescence channels (as stored in 
         \code{obj}). Additionally, a plot of the difference between fore- and
         background intensities is generated (\emph{background-corrected intensities}). 
         All intensities are log2-transformed. The colour range for plotting is defined by 0 and the maximum of 
         the logged intensity for each sub-graph separately.}
\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\seealso{\code{\link[marray]{marrayRaw}}}
\examples{
# LOADING RAW DATA 
data(sw)
# PLOTTING
fgbg.visu(sw[,1])
}
\keyword{hplot}




