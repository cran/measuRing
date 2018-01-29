\name{multiDetect}
\alias{multiDetect}
\title{Recursive detection}
\description{This function can recursively measure tree-ring widths in one or
several image sections.}
\usage{multiDetect(img.nm, ...)}
\arguments{
  \item{img.nm}{Character or matrix. Vector of paths to the image
sections or list of gray matrices.}
  \item{\dots}{Further arguments to be passed to \code{\link{ringDetect}}
and \code{\link{ringSelect}}.}
}
\details{The function can implement most of
the in-package routines for ring-border
detection, see arguments in
\code{\link{ringDetect}} and
\code{\link{ringSelect}}. Objects of type
\code{multiDetect} can be updated with new
argumenst, see examples.}
\value{list of \code{\link{ringDetect}} objects.}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## Paths to three image sections in the package:
img <- system.file(c("P105_a.tif",
                     "P105_b.tif",
                     "P105_d.tif"),
                   package="measuRing")

## Recursive detection (arbitrary ring borders and formation years
## are included):
mrings <- multiDetect(img,
                      inclu = list(c(1:40),c(1:30),c(1:41),c(1:32)),
                      last.yr = list(2014, 2013, 2012, 2011),
                      auto.det = c(FALSE,TRUE,FALSE,TRUE),
                      plot = FALSE)
str(mrings)

## Updating the mrings object with new arguments: 
mrings1 <- update(mrings,
                  exclu = list(c(1:4),c(1:4),c(1:4),c(1:4)),
                  last.yr = 2016)

}
