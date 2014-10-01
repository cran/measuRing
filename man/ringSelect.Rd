\name{ringSelect}
\alias{ringSelect}
\title{Border selection}
\description{Selection of pixels on central axis of plots of gray matrix in image segments to include or exclude ring borders. Grafical devices from either ringDetect or plotSegments should be active. Pixels are both: identified and stored  by left-clicking the mouse over central axis of gray image; pixel numbers of just added borders are highlighted on gray image. Graphical devices are sequentially closed by right-clicking the mouse. After a graphical device has been closed,  graphical device of the following segment is activated, and visual selection on such a new segment can be performed. Each of the processes: Inclusion and exclusion, must be performed independently.  }
\usage{ringSelect(ringDetect, type)}
\arguments{
  \item{ringDetect}{Object. output from ringDetect.}
  \item{type}{Character. set type = 'inc' to include pixels, or type = 'exc' to avoid pixels.}
}

\value{Vector with pixel numbers or irng borders to be included or excluded from analysis.}

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
## Output from ring.detect will be stored in object count1 (see example in ringDetect).
count1 <- ringDetect(
    image = system.file("P105_a.tif", package="measuRing"),
    ppi = 10^3,
    last.yr = 2012,
    segs = 2,
    method = 'linear',
    inclu = NA,
    exclu = NA,
    tit = 'measuRing example')
## output produces two figures for including or excluding ring borders.

## ## uncomment and run:        
##  inc <- ringSelect(count1,'inc') #choose pixels on image segments to include.
##  ## vector inc is used to evaluate ringDetect again:
##  count2 <- ringDetect(
##      image = system.file("P105_a.tif", package="measuRing"),
##      ppi = 10^3,
##      last.yr = 2012,
##      segs = 2,
##      method = 'linear',
##      inclu = inc, ## Argument inclu contains now values in object inc
##      exclu = NA,
##      tit = 'measuRing example')
##  summary(count2$pixelTypes) #included pixels were added to pixelTypes data.
##  exc <- ringSelect(count2,'exc') #choose pixels on image segments to exclude.
##  ## Both vectors: inc and exc are used to evaluate ringDetect again:
##  count3 <- ringDetect(
##      image = system.file("P105_a.tif", package="measuRing"),
##      ppi = 10^3,
##      last.yr = 2012,
##      segs = 2,
##      method = 'linear',
##      inclu = inc, ## Argument inclu contains now values in object inc
##      exclu = exc, ## Argument exclu contains now values in object exc
##      tit = 'measuRing example')
}
