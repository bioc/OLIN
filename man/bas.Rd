\name{bas}
\alias{bas}
\title{Between-array scaling}
\description{This function performs an between-array scaling }
\usage{bas(obj,mode="var")}
\arguments{\item{obj}{object of \dQuote{marrayNorm}} 
           \item{mode}{mode of scaling. Default option is scaling of arrays  
                       to have the same within-array variance of 
                       logged ratios (\code{var}). Alternatively, \code{mad}
	         	\code{qq} can be used (see details)}
}



\details{The function \code{bsv} adjust the scale of  logged ratios (\code{M=(log2(Ch2)-log2(Ch1))}) 
between the different arrays stored in \code{obj}.

Following schemes (\code{mode}) are implemented:
\itemize{
 \item \code{mode="var"}:  Logged ratios \code{M} are scaled to show the same (within-array) 
variance for all arrays in the batch stored in \code{obj}. 
The variance is calculated using \code{\link{var}}.

\item \code{mode="mad"}:   The same procedure as for \code{mode="var"} is applied using, however,
 median absolute deviation (\code{\link{mad}}) as robust estimate for withing-array variance.

\item \code{mode="qq"}:  The \emph{quantile scaling} is using the same procedure as the quantile normalisation described
 by Bolstad et al. (2003). In brief: Given X is the matrix with logged ratios (column corresponding to arrays, rows to genes) 

\enumerate{ 
\item Sort each column  of X (independently) producing Xs, 
\item Replace values in each row of Xs  by the mean value of the row producing Xsm,
\item Rearrange the ordering  for each column of  matrix Xsm, 
so that it has the columns have same ordering as for the  original matrix X. 
}The last step yields the scaled logged ratios \code{M}.

}

}
\note{Between-array scaling should only be performed if it can be assumed that the different arrays have
      a similar  distribution of logged ratios. This has to be check on a case-by-case basis. Caution should
        be taken in the interpretation of results for arrays hybridised with  biologically divergent samples, 
        if between-array  scaling is applied.}

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}
\references{Bolstad et al., A comparison of normalization methods for high density oligonucleotide array data based on variance and bias,
\emph{Bioinformatics}, 19: 185-193, 2003}
\seealso{\code{\link[array]{marrayNorm}},\code{\link{var}},\code{\link{mad}}}

\examples{


# DISTRIBUTION OF M BEFORE SCALING
data(sw.olin)

col <- c("red","blue","green","orange")
M <- maM(sw.olin)

plot(density(M[,4]),col=col[4],xlim=c(-2,2))
for (i in 1:3){
  lines(density(M[,i]),col=col[i])
}


# SCALING AND VISUALISATION  
sw.olin.s <- bas(sw.olin,mode="var")
  
M <- maM(sw.olin.s)

plot(density(M[,4]),col=col[4],xlim=c(-2,2))
for (i in 1:3){
  lines(density(M[,i]),col=col[i])
}

}
\keyword{utilities}




