\name{linearDetect}
\alias{linearDetect}
\title{Linear detection}
\description{Function for developing linear detection of ring borders.}
\usage{linearDetect(data2, origin = 0)}
\arguments{
  \item{data2}{Data frame. Smoothed grays from graySmoothed.}
  \item{origin}{Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection arround zero.}
}

\value{Vector. Gray columns of ring borders. }

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
       image = system.file("P105_a.tif", package="measuRing")
       gray <- imageTogray(image = image,p.row = 1)
       smoothed <- graySmoothed(gray,ppi = 10^3)
       borders <- linearDetect(smoothed)
## Ring borders should be visually controlled with ringDetect.        
}
