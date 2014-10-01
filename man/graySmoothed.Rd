\name{graySmoothed}
\alias{graySmoothed}
\title{Smoothed grays}
\description{Averaging, detrending and smoothing of columns in gray matrix. }
\usage{graySmoothed(gray, ppi)}
\arguments{
  \item{gray}{Matrix. Gray matrix. See imageTogray}
  \item{ppi}{Numeric. Image resolution in points per inch.}
}

\value{Data frame with six columns: pixel number (N_pixel), gray value (V_pixel), moving average (m.av), centered gray (cent), smoothed gray (smooth), and distances (mm) from left border of image section (distance).}

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
image = system.file("P105_a.tif", package="measuRing")
gray <- imageTogray(image = image,p.row = 1)
smoothed <- graySmoothed(gray,ppi = 10^3)
summary(smoothed)
}
