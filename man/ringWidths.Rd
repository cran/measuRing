\name{ringWidths}
\alias{ringWidths}
\title{Ring widths }
\description{This function computes tree-ring widths and formation years. }
\usage{ringWidths(pixtypes, last.yr)}
\arguments{
  \item{pixtypes}{Data frame. Included and excluded borders from pixelTypes}
  \item{last.yr}{Numeric. Year of formation of newest ring in image section.}
}

\value{Data frame with two columns: formation year (year) and rind-widths (delta).}

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
image = system.file("P105_a.tif", package="measuRing")
gray <- imageTogray(image = image,p.row = 1)
smoothed <- graySmoothed(gray,ppi = 10^3)
borders <- linearDetect(smoothed,origin = 0)
pixtypes <- pixelTypes(smoothed,borders)
rwidths <- ringWidths(pixtypes,last.yr = 2012)
## Ring witdhs should be visually controlled with
 # ringDetect (see also example in ringSelect).        
}
