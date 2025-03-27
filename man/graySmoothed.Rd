\name{graySmoothed}
\alias{graySmoothed}
\title{Smoothed gray}
\description{Averaging, detrending, and smoothing of the columns in a gray matrix. }
\usage{graySmoothed(image, all = FALSE, 
    ...)}
\arguments{
  \item{image}{character or matrix. Either path of an \code{image} section
or an array representing a gray matrix.}
  \item{all}{logical. If TRUE the column numbers and
moving averages are added to the output.}
  \item{\dots}{arguments to be passed to \code{\link{imageTogray}}.}
}

\value{data frame with the smoothed grays. If \code{all} is
TRUE then the output is extended with the columns in
gray matrix, and moving averages.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (<https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
## (not run) Read one image section in package measuRing:
image1 <- system.file("P105_a.png", package="measuRing")    
## (not run) the smoothed gray:
smoothed <- graySmoothed(image1,ppi=1000)
## (not run) Plot of the smoothed gray:        
Smooth <- ts(smoothed)
main. <- 'Smoothed gray'
plot(Smooth,xlab = 'Column', main=main.,
     ylab = 'Smoothed gray',col = 'gray')

}
