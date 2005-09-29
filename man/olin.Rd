\name{olin}
\alias{olin} 
\title{Optimised local intensity-dependent normalisation of two-colour microarrays} 		
\description{This functions performs optimised local intensity-dependent normalisation (OLIN) and 
optimised scaled intensity-dependent normalisation (OSLIN).}
\usage{olin(object,X=NA,Y=NA,alpha=seq(0.1,1,0.1),iter=3,
            scale=c(0.05,0.1,0.5,1,2,10,20),OSLIN=FALSE,weights=NA,
            genepix=FALSE,bg.corr="subtract",...)}
           \arguments{\item{object}{object of class \dQuote{marrayRaw}
           or \dQuote{marrayNorm} corresponding to a single array or a batch of
           arrays.}
           \item{X}{matrix with x-coordinates of spots of the arrays in \code{object}. Each column
           includes the x-coodinates for the spots of one array.  If X=NA, columns on array are used
            as proxies for the location in x-direction}
           \item{Y}{matrix with y-coordinates of spots.  Each column
           includes the y-coodinates for the spots of one array.If Y=NA, rows on array are used
            as proxies for the location in y-direction}
           \item{alpha}{vector of alpha parameters that are tested in the GCV procedure}
           \item{iter}{number of iterations in the OLIN procedure}
           \item{scale}{vector of scale parameters that are tested in a GCV procedure for spatial regression. This 
             define the amount of smoothing in X-direction with respect to smoothing in Y-direction.}
           \item{OSLIN}{If OSLIN=TRUE, subsequent scaling of the range of M accross the array is performed.}
	   \item{weights}{matrix of (non-negative) weights for  local regression (see \code{\link{locfit}}).
             Rows correspond to the spotted probe sequences, columns to
             arrays in the batch.
	     If the weight of the corresponding spot equals zero, the
	     spot is not used in the normalisation procedures (except
	     the \code{genepix} argument is set to \code{TRUE}.)
	     If the weight matrix include negative values, these will be
	     set to zero. 
              These weight matrix may be derived from the matrix of  spot quality weights as defined 
                          for \dQuote{maRaw} objects (\code{weights=maW(object)}.
			  Weights can be also used if the normalisation should
                          be based on a set of selected genes that are
                          assumed to be not differentially expressed.}
			\item{genepix}{If \code{genepix} is set to
			\code{TRUE},  spot weights equal zero or larger are set
			to one for the local regression whereas negative
			spot with negative weights are not used   for the
			regression. The argument \code{genepix} should
			be set to \code{TRUE}, if
			\code{weights=maW(object)} is set and  spot
			quality weights derived by GenePix are stored in
			\code{maW(object)}.}
		      \item{bg.corr}{backcorrection method (for
		      \dQuote{marrayRaw} objects)  : \dQuote{none},
		      \dQuote{subtract}, \dQuote{half},
		      \dQuote{minimum}, \dQuote{movingmin}, \dQuote{edwards} or \dQuote{normexp}.}		
           \item{...}{Further arguments for \code{locfit} function.}		
}         
\details{
OLIN and OSLIN are based on iterative local regression and incorporate optimisation of model parameters.
 Local regression is performed using LOCFIT, which requires the user to choose a specific smoothing parameter \code{alpha} 
  that controls the neighbourhood size \emph{h} of local fitting. The parameter \code{alpha} 
specifies the fraction of points that are included in the neighbourhood and thus has a value between 0 and 1.
 Larger \code{alpha} values lead to smoother fits. 
Additionally, the setting of scale parameters  controls for distinct amount of smoothing in 
Y-direction compared to smoothing in X-direction.
The parameter \code{scale} can be of arbitrary value. 
The choice of model parameters \code{alpha} and \code{scale} for local regression is crucial for the efficiency and 
quality of normalization. To optimize the model parameters, a general cross-validation procedure (GCV) is applied.
The arguments \code{alpha} and \code{scale} define the parameters values
which are tested in the GCV. OSLIN comprises the OLIN procedure with a
subsequent optimized scaling of the range of logged intensity ratios
across the spatial dimensions of the array. Details concerning the
background correction methods can be found in the help page for \code{\link{backgroundCorrect2}}. 


Detailed information about OLIN and OSLIN can be found in the package documentation and in the  reference stated below. 
The weights argument specifies the influence of the single spots on the local regression. To exclude 
spots being used for the local regression (such as control spots), set their corresponding weight to zero. 
Note that OLIN and OSLIN
are based on the assumptions that most genes are not differentially expressed (or up- and down-regulation
is balanced) and that genes are randomly spotted across the array. If these assumptions are not valid, local
regression can lead to an underestimation of differential expression.  OSLIN is especially sensitive to violations  of these assumptions. However, this
sensitivity can be decreased if the minimal \code{alpha}-value is increased. Minimal \code{alpha} defines the 
smallest scale used for local regression. Increasing \code{alpha} can reduce the influence of localised 
artifacts as a larger fraction of  data points is included.  Alternative normalisation functions such
as \code{\link{oin}}, \code{\link{lin}} and \code{\link{ino}} might also be used for a more conservative fit.
 

If the normalisation should be based on set of genes assumed to be not differentially expressed (\emph{house-keeping
genes}), weights can be used for local regression. In this case, all weights are set to zero except for
the house-keeping genes for which weights are set to one. In order to achieve a reliable regression, it is important, however, that there is a sufficient number of house-keeping genes that are distributed over the whole expression range
and spotted accross the whole array.

It is also important to note that OLIN/OSLIN is fairly efficient in removing intensity- and spatial-dependent dye bias, so that normalised  data will look quite \dQuote{good} after normalisation independently of the true underlying data quality. Normalisation by local regression assumes smoothness of bias. Therefore, localised artifacts such as scratches, edge effects or bubbles should be avoided. Spots of these areas should be flagged (before normalisation is applied) to ensure data integrity. To stringently detect artifacts, the OLIN functions \code{fdr.int, fdr.spatial, p.int} and \code{p.spatial} can be used.}


\value{Object of class \dQuote{marrayNorm} with normalised logged ratios}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\references{
\enumerate{
\item M.Futschik and T.Crompton (2004) \emph{Model selection and efficiency testing for normalization of cDNA microarray data},
\strong{Genome Biology}, 5:R60
\item M.Futschik and T.Crompton (2005) \emph{OLIN: Optimized
      normalization, visualization and quality testing for two-channel
      microarray data}, \strong{Bioinformatics}, 21(8):1724-6    
    \item  OLIN web-page:
           \url{http://itb.biologie.hu-berlin.de/~futschik/software/R/OLIN}}}

       
\seealso{\code{\link{maNorm}}, \code{\link{locfit}}, \code{\link{gcv}}, \code{\link{oin}},
  \code{\link{lin}}}
\examples{


# LOADING DATA
  data(sw)
  data(sw.xy)

# OPTIMISED LOCAL INTENSITY-DEPENDENT NORMALISATION OF FIRST ARRAY
  norm.olin <- olin(sw[,1],X=sw.xy$X[,1],Y=sw.xy$Y[,1])

# MA-PLOT OF NORMALISATION RESULTS OF FIRST ARRAY
  plot(maA(norm.olin),maM(norm.olin),main="OLIN")
 
# CORRESPONDING MXY-PLOT
  mxy.plot(maM(norm.olin)[,1],Ngc=maNgc(norm.olin),Ngr=maNgr(norm.olin),
                Nsc=maNsc(norm.olin),Nsr=maNsr(norm.olin),main="OLIN")

# OPTIMISED SCALED LOCAL INTENSITY-DEPENDENT NORMALISATION
  norm.oslin <- olin(sw[,1],X=sw.xy$X[,1],Y=sw.xy$Y[,1],OSLIN=TRUE)
# MA-PLOT
  plot(maA(norm.oslin),maM(norm.oslin),main="OSLIN")
# MXY-PLOT
  mxy.plot(maM(norm.oslin)[,1],Ngc=maNgc(norm.oslin),Ngr=maNgr(norm.oslin),
                 Nsc=maNsc(norm.oslin),Nsr=maNsr(norm.oslin),main="OSLIN")

}
\keyword{utilities}
\keyword{regression}





