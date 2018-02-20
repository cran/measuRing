\name{colNarrow}
\alias{colNarrow}
\title{Narrow rings}
\description{This function can detect narrow rings in a sequence of tree-ring
widths in wood (TRWs). This and other in-package functions are
recursively implemented by \code{\link{multiDetect}}.}
\usage{colNarrow(rwidths, marker = 5)}
\arguments{
  \item{rwidths}{a dataframe with the ring widths such as that
produced by \code{\link{ringWidths}}.}
  \item{marker}{a number from 1 to 10. Those rings with scaled
averages greater than or equal to this argument
will be identified as narrow rings.}
}
\details{Each ring is averaged with those rings on either side
of it (t-1,t,t+1), and averages are divided by the highest
computed average in the sample; such quotients are scaled from
10 (the narrowest possible ring) to one (the broadest ring).}
\value{character vector with the columns in gray matrix corresponding
to the narrow rings (see \code{\link{ringDetect}},
\code{\link{multiDetect}}, and\code{\link{plotSegments}}).}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## (not run) Read one image section in package measuRing:
image1 <- system.file("P105_a.png", package="measuRing")    
## (not run) compute a gray matrix from RGB in the image:
gray <- imageTogray(image = image1,ppi=1000)
## (not run) Columns in gray matrix to be included/excluded:
Toinc <- c(196,202,387,1564) 
Toexc <- c(21,130,197,207,1444,1484)
## (not run) tree-ring widths:
rwidths <- ringWidths(gray,inclu = Toinc,exclu = Toexc,last.yr=2012)
##(not run) narrow rings:
narrows <- colNarrow(rwidths,marker = 8)
}
