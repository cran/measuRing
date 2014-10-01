\name{lagIngray}
\alias{lagIngray}
\title{Maximum-lag window}
\description{Lag corresponding with first maximum relative of auto-correlation function between smoothed grays. This value is used as window of moving average to center smoothed grays (see graySmoothed).}
\usage{lagIngray(gray)}
\arguments{
  \item{gray}{Matrix. Gray matrix from imageTogray.}
}

\value{Computed lag.}

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
image = system.file("P105_a.tif", package="measuRing")
gray <- imageTogray(image = image,p.row = 1)
window <- lagIngray(gray)
}
