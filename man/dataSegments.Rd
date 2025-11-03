\name{dataSegments}
\alias{dataSegments}
\title{Data segments}
\description{Segmented data sets required by function
\code{\link{plotSegments}}.}
\usage{dataSegments(image, segs = 1, 
    ...)}
\arguments{
  \item{image}{Either path of an \code{image} section or an array
representing a gray matrix.}
  \item{segs}{number of \code{image} segments.}
  \item{\dots}{arguments to be passed to three functions:
\code{\link{ringWidths}}, \code{\link{ringBorders}},
and/or \code{\link{imageTogray}}.}
}

\value{a list with segmented sets of the gray matrix, the ring
borders, and the ring widths (see
\code{\link{plotSegments}}).}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (ORCID: <https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




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
