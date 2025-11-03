\name{plotSegments}
\alias{plotSegments}
\title{Image segments}
\description{One or several plots of consecutive segments of gray matrix and
smoothed grays.}
\usage{plotSegments(image, ratio = NULL, 
    marker = NULL, col.marker = "red", 
    tit = TRUE, plot = TRUE, 
    ...)}
\arguments{
  \item{image}{character or matrix. Either path of an \code{image} section
or an array representing a gray matrix.}
  \item{ratio}{NULL or vector with two values representing
the aspect of the plots (height, and width). If
NULL the default aspect in \code{par()} is
used.}
  \item{marker}{NULL or a number from 1 to 10 as explained
in \code{\link{colNarrow}}. If numeric then
three kind of markers are indicated: those
narrow rings with averages major than
\code{marker}, chronological markers
(decades, centuries, and millenia), and the
column numbers in gray matrix of the ring
borders. The markers are highlighted with
the color in \code{col.marker}. If NULL no
markers are highlighted.}
  \item{col.marker}{color of the markers.}
  \item{tit}{logical or character. A title for the
plots. If TRUE the main title is the \code{image}
name. For more than 1 segment the main title
ends with the segment number.}
  \item{plot}{logical. If TRUE the \code{image} segments are
plotted.}
  \item{\dots}{arguments to be passed to four functions:
\code{\link{dataSegments}}, \code{\link{ringWidths}},
\code{\link{ringBorders}}, and/or
\code{\link{imageTogray}}.}
}

\value{the \code{image} segments and a list such as that produced by
\code{\link{dataSegments}}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (ORCID: <https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## (not run) Read one image sample in folder of package measuRing:
image1 <- system.file("P105_a.tif", package="measuRing")        
## column numbers to be included/avoided:
Toinc <- c(196,202,387,1564) 
Toexc <- c(21,130,197,207,1444,1484)        
##(not run) Plotting of five image segments:
plots <- plotSegments(image1,rgb=c(0.5,0,0.5),last.yr=2011,
    marker=8,segs=3,inclu = Toinc,exclu = Toexc)
## plots <- plotSegments(rwidths,segs = 4,marker=8)
## (not run) kill all the image segments:
graphics.off()
    
}
