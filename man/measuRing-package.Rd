\name{measuRing-package}
\alias{measuRing-package}
\alias{measuRing}
\docType{package}
\title{Detection and Control of Tree-Ring Widths on Scanned Image Sections of Dendrochronological Samples}
\description{This package assists in the identification of ring borders on scanned image sections from dendrochronological samples. It processes the image section and computes luminance data from images producing a matrix of gray values and a time series of smoothed gray values. Luminance data is plotted on segmented images for users to perform both: visual identification of ring borders, or control of automatic detection. The package provides functions to visually include/exclude ring borders on the R graphical device, or automatically detect ring borders using a linear detection algorithm. This algorithm detects ring borders according to negative extreme values in the smoothed time-series of gray values.}
\details{
\tabular{ll}{Package: \tab measuRing\cr
Type: \tab Package\cr
Title: \tab Detection and Control of Tree-Ring Widths on Scanned Image Sections of Dendrochronological Samples\cr
Version: \tab 0.1\cr
Date: \tab 2014-10-01\cr
Author: \tab Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>\cr
Maintainer: \tab Wilson Lara <wilarhen@gmail.com>\cr
License: \tab GPL-3\cr
Depends: \tab tiff, zoo, pastecs\cr
LazyData: \tab TRUE\cr}
}
\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}

\keyword{ package }


