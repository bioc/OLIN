\name{sw}
\docTye{data}
\alias{sw}
\title{cDNA microarray data of  SW480/SW620 experiment}
\description{Gene expression in two cancer cell lines, SW480 and SW620, is compared. 
The SW480 cell line was derived from a colon tumour of a 50-year old male patient. 
The second cell line (SW620) originated from a lymph node metastasis of the same patient. 
Sharing the same genetic background, these cell lines serve as an model of cancer progression.

Target cDNA from SW480 was labelled with Cy3 whereas cDNA from SW620 was labelled with Cy5 using the amino-allyl labelling method. 
Both cDNA pools were co-hybridised on glass slides with 8448 spots. 
The spots consisted of 3986 distinct sequence-verified human cDNA clones (Research Genetics, release GF211) 
printed in duplicates, 84 spots from non-human cDNA clones and a further 154 control spots. 
Spots were printed by 4x4 pins. The experiment consisted of four replicated arrays derived 
from separate labelling reactions. Local background spot intensities were extracted by QuantArray software (version2.1).
Analysis showed that replicated spots were highly correlated (average Pearson correlation: 0.94). 
Since this may interfere with the efficiency testing performed (and to reduce
the size of the data set for illustration purpose), the replicated spots were not included here. 
Experimental details and further analysis can be found in Futschik et al. (2002).
}
\usage{data(sw)}
\format{An object of class \dQuote{marrayRaw}}
\source{The data was produced  and  provided by Sharon Pattison of the Cancer Genetics lab and Aaron Jeffs of
the Otago Genomics facility of the University of Otago, Dunedin, New Zealand.} 
\references{M. Futschik, A.Jeffs, S.Pattison, N.Kasabov, M.Sullivan, A.Merrie and A.Reeve, Gene expression profiling of metastatic and non-metastatic colorectal cancer cell-lines, \emph{Genome Letters}, vol.1, No.1, pp. 26-34, 2002}
\seealso{\code{\link{sw.olin}}}
\keyword{datasets}



