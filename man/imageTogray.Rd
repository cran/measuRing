\name{imageTogray}
\alias{imageTogray}
\title{Gray matrix}
\description{Matrix of luminances from combining three channels: red, green, and blue (rgb) of image-section.}
\usage{imageTogray(image, rgb = c(0.3, 0.6, 0.1), p.row = 1)}
\arguments{
  \item{image}{Character. Image name in working directory. File sould be compressed in tagged image file format (.tif) and contain rgb channels.  Character name should contain file extension:.tif.}
  \item{rgb}{Numeric vector. Three fractions, all of them adding to one, to combine red, green, and blue channels.}
  \item{p.row}{Numeric. Proportion of rows in image section.}
}

\value{Gray matrix. }

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
image = system.file("P105_a.tif", package="measuRing")
gray <- imageTogray(image = image,p.row = 1)
}
