\name{olin}
\alias{olin} 
\title{Optimised local intensity-dependent normalisation of two-colour microarrays} 		
\description{This functions performs optimised local intensity-dependent normalisation (OLIN) and 
optimised scaled intensity-dependent normalisation (OSLIN).}
\usage{olin(object,X=NA,Y=NA,alpha=seq(0.05,1,0.1),iter=3),
            scale=c(0.05,0.1,0.5,1,2,10,20),scaling=FALSE)}
           \arguments{\item{object}{object of class \dQuote{marrayRaw}}
           \item{X}{matrix with x-coordinates of spots. If X=NA, columns on array are used
            as proxies for the location in x-direction}
           \item{Y}{matrix with y-coordinates of spots. If Y=NA, rows on array are used
            as proxies for the location in y-direction}
           \item{alpha}{vector of alpha parameters that are tested in the GCV procedure}
           \item{iter}{number of iterations in the OLIN procedure}
           \item{scaling}{If scaling=TRUE, a subsequent optimised scaling is performed}
           \item{scale}{vector of scale parameters that are tested in a GCV procedure.}
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
The arguments \code{alpha} and \code{scale} define the parameters values which are tested in the GCV. 
Detailed information about OLIN and OSLIN can be found in the package documentation and in the  reference stated below. 
Note that OLIN and OSLIN
are based on the assumptions that most genes are not differentially expressed (or up- and down-regulation
is balanced) and that genes are randomly spotted across the array. If these assumptions are not valid, local
regression can lead to an underestimation of differential expression.  OSLIN is especially sensitive to violations  of these assumptions. However, this
sensitivity can be decreased if the minimal \code{alpha}-value is increased. 
 

It is also important to note that OLIN/OSLIN is fairly efficient in removing intensity- and spatial-dependent dye bias, so that normalised  data will look quite \dQuote{good} after normalisation independently of the true underlying data quality. Normalisation by local regression assumes smoothness of bias. Therefore, localised artifacts such as scratches, edge effects or bubbles should be avoided. Spots of these areas should be flagged (before normalisation is applied) to ensure data integrity. To stringently detect artifacts, the OLIN functions \code{fdr.int, fdr.spatial, p.int} and \code{p.spatial} can be used.}


\value{Object of class \dQuote{marrayNorm} with normalised logged ratios}
\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\references{
\enumerate{
\item M.Futschik and T.Crompton (2004) \emph{Model selection and efficiency testing for normalization of cDNA microarray data},
\strong{Genome Biology}, 5:R60
\item  OLIN web-page: \url{http://itb.biologie.hu-berlin.de/~futschik/software/R/OLIN}}}
\seealso{\code{\link{maNorm}}, \code{\link{locfit}}, \code{\link{gcv}}}
\examples{


# LOADING DATA
  data(sw)
  data(sw.xy)

# OPTIMISED LOCAL INTENSITY-DEPENDENT NORMALISATION OF FIRST ARRAY
 norm.olin <- olin(sw[,1],X=sw.xy$X[,1],Y=sw.xy$Y[,1])

# MA-PLOT OF NORMALISATION RESULTS OF FIRST ARRAY
 plot(maA(norm.olin),maM(norm.olin),main="OLIN")
 
# CORRESPONDING MXY-PLOT
  Mtmp <- mxy.plot(maM(norm.olin)[,1],Ngc=maNgc(norm.olin),Ngr=maNgr(norm.olin),
                Nsc=maNsc(norm.olin),Nsr=maNsr(norm.olin),main="OLIN")

# OPTIMISED SCALED LOCAL INTENSITY-DEPENDENT NORMALISATION
  norm.oslin <- olin(sw[,1],X=sw.xy$X[,1],Y=sw.xy$Y[,1],scaling=TRUE)
# MA-PLOT
  plot(maA(norm.oslin),maM(norm.oslin),main="OSLIN")
# MXY-PLOT
  Mtmp <- mxy.plot(maM(norm.oslin)[,1],Ngc=maNgc(norm.oslin),Ngr=maNgr(norm.oslin),
                 Nsc=maNsc(norm.oslin),Nsr=maNsr(norm.oslin),main="OSLIN")

}
\keyword{utilities}
\keyword{regression}





