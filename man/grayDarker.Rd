\name{grayDarker}
\alias{grayDarker}
\title{Gray extremes}
\description{This function can detect the extremes of the \code{smoothed} gray.}
\usage{grayDarker(smoothed, 
    origin = 0, darker = TRUE)}
\arguments{
  \item{smoothed}{a data frame with the \code{smoothed} gray such
as that produced by
\code{\link{graySmoothed}}.}
  \item{origin}{an \code{origin} to find the extremes.}
  \item{darker}{logical. If TRUE the function finds the
negative extremes. If FALSE the possitive
extremes are detected.}
}

\value{vector with the columns in gray matrix corresponding to
the extremes (see \code{\link{graySmoothed}} and
\code{\link{linearDetect}}).}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (<https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (<https://orcid.org/0000-0001-7348-6695>)}




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
