\name{dataSegments}
\alias{dataSegments}
\title{Data segments}
\description{Segmented data sets required by function
\code{\link{plotSegments}}.}
\usage{dataSegments(image, segs = 1, ...)}
\arguments{
  \item{image}{Either path of an image section or an array
representing a gray matrix.}
  \item{segs}{number of image segments.}
  \item{\dots}{arguments to be passed to \code{\link{ringWidths}}.}
}

\value{a list with segmented sets of the gray matrix, the ring
borders, and the ring widths (see \code{\link{ringWidths}},
and \code{\link{plotSegments}}).}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## (not run) Read one image section in package measuRing:
image1 <- system.file("P105_a.tif", package="measuRing")    
## (not run) compute a gray matrix from its RGB:
gray <- imageTogray(image1)
## (not run) Columns in gray matrix to be included/excluded:
Toinc <- c(196,202,387,1564) 
Toexc <- c(21,130,197,207,1444,1484)
## (not run) segmented data:
segm <- dataSegments(image1,segs = 3)
lapply(segm,str)
attributes(segm)
}
