\name{grayDarker}
\alias{grayDarker}
\title{Darker grays}
\description{Identification of darker grays on smoothed gray.}
\usage{grayDarker(data2, origin = 0)}
\arguments{
  \item{data2}{Data frame. Smoothed grays from graySmoothed.}
  \item{origin}{Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection of borders arround zero.}
}

\value{Vector. Gray columns of darker grays from automatic detection on gray matrix. }

\author{Wilson Lara <wilarhen@gmail.com>, Carlos Sierra <csierra@bgc-jena.mpg.de>}




\examples{
image = system.file("P105_a.tif", package="measuRing")
gray <- imageTogray(image = image,p.row = 1)
smoothed <- graySmoothed(gray,ppi = 10^3)
darker <- grayDarker(smoothed)
length(darker)   
}
