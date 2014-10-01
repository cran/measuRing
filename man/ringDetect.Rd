\name{ringDetect}
\alias{ringDetect}
\title{Detection and measurement of tree-ring widths}
\description{This function reads an image from file folder, develops automatic detection of tree ring-widths (trw), and provides image segments to visually control detection by excluding and including trw.}
\usage{ringDetect(image, ppi, last.yr, rgb = c(0.3, 0.6, 0.1), p.row = 1, 
    origin = 0, over = NA, method = NA, inclu = NA, exclu = NA, 
    segs = 1, ratio = NULL, tit = "")}
\arguments{
  \item{image}{Character. Image name in working directory. File should be compressed in tagged image file rormat (.tif) and contain three channels: red, green, and blue (rgb). Agument image should contain file extension:.tif.}
  \item{ppi}{Numeric. Image resolution in points per inch.}
  \item{last.yr}{Numeric. Year of formation of newest ring in image section.}
  \item{rgb}{Numeric vector. Three fractions, all of them adding to one, to combine red, green, and blue channels.}
  \item{p.row}{Numeric. Proportion of rows in image section.}
  \item{origin}{Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection arround zero.}
  \item{over}{Numeric. Overriding constant on smoothed grays below which automatically detected borders are avoided; over = NA avoids overriding process.}
  \item{method}{Character. Method to be used in automatic detection of ring borders. Currently only linear detection is implemented (method = 'linear'). Default method = NA avoids automatic detection. See linearDetect.}
  \item{inclu}{Vector. Columns in gray matrix to be considered as ring borders. Default inclu = NA produces no selection of pixels. See ringSelect.}
  \item{exclu}{Vector. Columns in gray matrix of those previously identified ring borders to be excluded from analysis. Default exclu = NA produces no exclusion of borders. See ringSelect.}
  \item{segs}{Numeric. Number of image segments.}
  \item{ratio}{Vector. Two values givind aspect ratio of plot (p.row, and width). Values corresponding to aspect ratio of display prevent distortion in segmented plots. See also dev.new().}
  \item{tit}{Title of image segments. Default tit = "" avoids title.}
}

\value{Image segments and list with three objects: data frame with the three-ring widths (see ringWidths), data frame with pixel types (see pixelTypes),and gray matrix (see imageTogray).}

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
count1 <- ringDetect(
    image = system.file("P105_a.tif", package="measuRing"),
    ppi = 10^3,
    last.yr = 2012,
    segs = 2,
    method = 'linear',
    inclu = NA,
    exclu = NA,
    tit = 'measuRing example')
## output produces two figures for including or excluding ring borders (see ringSelect)
}
