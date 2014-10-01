\name{pixelTypes}
\alias{pixelTypes}
\title{Selection types}
\description{Included or excluded ring borders. Pixel distances from left border of image section are specified. }
\usage{pixelTypes(data2, borders, over = NA, inclu = NA, exclu = NA)}
\arguments{
  \item{data2}{Data frame. Smoothed grays from graySmoothed.}
  \item{borders}{Vector. Border numbers from linearDetect.}
  \item{over}{Numeric. Overriding constant on smoothed grays below which automatically detected borders are avoided; over = NA avoids overriding process.}
  \item{inclu}{Vector. Gray columns to be considered as ring borders. Default inclu = NA produces no selection of borders. See ringSelect.}
  \item{exclu}{Vector. Gray columns of previously selected ring borders to be excluded from analysis. Default exclu = NA produces no exclusion of borders. See ringSelect.}
}

\value{Data frame with three columns: Distances (mm) from left border of image section (distance), smoothed grays (cent) and pixel types: included,excluded, or automatic (pixtypes)}

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
image = system.file("P105_a.tif", package="measuRing")
gray <- imageTogray(image = image,p.row = 1)
smoothed <- graySmoothed(gray,ppi = 10^3)
borders <- linearDetect(smoothed,origin = 0)
pixtypes <- pixelTypes(smoothed,borders)
summary(pixtypes)
## Pixel types should be visually controlled with ringDetect.        
 
}
