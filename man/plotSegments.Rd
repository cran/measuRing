\name{plotSegments}
\alias{plotSegments}
\title{Image segments}
\description{This function splits gray matrix and smoothed grays in desired number of image segments for developing visual control in tree-ring detection.}
\usage{plotSegments(gray, pixtypes, last.yr, origin = 0, over = NA, 
    segs = 1, ratio = NULL, tit = "")}
\arguments{
  \item{gray}{Matrix. Gray matrix from imageTogray}
  \item{pixtypes}{Data frame. Included and excluded borders from pixelTypes}
  \item{last.yr}{Numeric. Year of formation of newest ring in image section.}
  \item{origin}{Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection arround zero.}
  \item{over}{Numeric. Overriding constant on smoothed grays below which automatically detected borders are avoided; over = NA avoids overriding process.}
  \item{segs}{Numeric. Number of segments to split gray image in segmented plot.}
  \item{ratio}{Vector. Two values givind aspect ratio of plot (height, and width). Values corresponding to aspect ratio of display prevent distortion in segmented plots. See also dev.new().}
  \item{tit}{Title of plot. Default tit = '' avoids title in plot.
,...##<< further arguments to modify plot; see function par().}
}

\value{A number of image segments as indicated in segs.}

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
image = system.file("P105_a.tif", package="measuRing")
gray <- imageTogray(image = image,p.row = 1)
smoothed <- graySmoothed(gray,ppi = 10^3)
borders <- linearDetect(smoothed,origin = 0)
pixtypes <- pixelTypes(smoothed,borders)
plotSegments(gray,pixtypes,last.yr = 2012,segs = 3)
## output prints three plots
## Ring borders in segmented images should be visually controlled with ringDetect.        
 
}
