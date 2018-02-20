\name{multiDetect}
\alias{multiDetect}
\title{Multiple detection of TRWs}
\description{This function recursively detects TRWs in sets of scanned images
of wood or gray matrices.}
\usage{multiDetect(image, ...)}
\arguments{
  \item{image}{\code{character} or \code{list}. Vector of Paths to the
image files or set of gray matrices.}
  \item{\dots}{Further arguments to be passed to \code{\link{ringDetect}}
and \code{\link{ringSelect}}.}
}

\value{\code{list}. Set of \code{\link{ringDetect}} calls.}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## Paths to three image sections in the package:
img <- system.file(c("P105_a.tif",
                     "P105_b.tif",
                     "P105_d.tif"),
                   package="measuRing")

## Recursive detection. Arbitrary ring borders and different years
## of formation of last rings in the images years are specified:
mrings <- multiDetect(img,
                      inclu = list(c(1:40),c(1:30),c(1:41)),
                      last.yr = list(2014, 2013, 2012),
                      auto.det = c(FALSE,TRUE,FALSE),
                      plot = FALSE)
str(mrings)

## Updating the call in mrings using new arguments: 
mrings1 <- update(mrings,
                  exclu = list(c(1:4),c(1:4),c(1:4)),
                  last.yr = 2016)

}
