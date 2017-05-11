\name{grayDarker}
\alias{grayDarker}
\title{Gray extremes}
\description{This function can detect the extremes of the smoothed gray.}
\usage{grayDarker(smoothed, origin = 0, darker = TRUE)}
\arguments{
  \item{smoothed}{a data frame with the smoothed gray such
as that produced by
\code{\link{graySmoothed}}.}
  \item{origin}{an origin to find the extremes.}
  \item{darker}{logical. If TRUE the function finds the
negative extremes. If FALSE the possitive
extremes are detected.}
}

\value{vector with the columns in gray matrix corresponding to
the extremes (see \code{\link{graySmoothed}} and
\code{\link{linearDetect}}).}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## (not run) read one image section:
image1 <- system.file("P105_a.png", package="measuRing")    
## (not run) gray matrix from RGB in image:
gray <- imageTogray(image = image1,ppi = 1000)
## (not run) smoothed gray:
smoothed <- graySmoothed(gray)
## (not run) column numbers of possitive and negative extremes:
posit <- grayDarker(smoothed,darker=FALSE)
nega <- grayDarker(smoothed,darker=TRUE)
str(nega)
}
