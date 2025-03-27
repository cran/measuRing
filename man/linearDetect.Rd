\name{linearDetect}
\alias{linearDetect}
\title{Linear detection}
\description{Function for developing linear detection of ring borders.}
\usage{linearDetect(smoothed, 
    origin = 0, darker = TRUE)}
\arguments{
  \item{smoothed}{a data frame with \code{smoothed} grays such as that
produced by \code{\link{graySmoothed}}.}
  \item{origin}{numeric. an \code{origin} in \code{smoothed} gray to find the
ring borders.}
  \item{darker}{logical. If TRUE the algorithm uses the
negative extremes on \code{smoothed} grays to detect the ring
borders. If FALSE the possitive extremes are used.}
}

\value{vector with column numbers in gray matrix of the detected
ring borders (see \code{\link{grayDarker}}, and
\code{\link{graySmoothed}}).}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (<https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
## (not run) Read one image section in package measuRing:
image1 <- system.file("P105_a.tif", package="measuRing")    
## (not run) smoothed gray:
smoothed <- graySmoothed(image1)
## linear detection:
borders <- linearDetect(smoothed)
str(borders)
}
