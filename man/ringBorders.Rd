\name{ringBorders}
\alias{ringBorders}
\title{Ring borders}
\description{This function can find the ring borders in a gray matrix.}
\usage{ringBorders(image, auto.det = TRUE, 
    darker = TRUE, origin = 0, 
    inclu = NULL, exclu = NULL, 
    ...)}
\arguments{
  \item{image}{character or matrix. Either path of an \code{image} section
or an array ##representing a gray matrix.}
  \item{auto.det}{logical. If TRUE the linear detection is
implemented (see
\code{\link{linearDetect}}).}
  \item{darker}{logical. If TRUE the algorithm uses the
negative extremes on smoothed grays to
detect the ring borders. If FALSE the
possitive extremes are used.}
  \item{origin}{numeric. an \code{origin} in smoothed gray to
find the ring borders.}
  \item{inclu}{NULL or vector with column numbers in gray
matrix, other than those automatically
detected, to be considered as ring borders.If
NULL no column numbers are included.}
  \item{exclu}{NULL or vector with column numbers in gray}
  \item{\dots}{arguments to be passed to \code{\link{imageTogray}}.}
}

\value{a data frame with the smoothed grays and the identified
ring borders (see \code{\link{grayDarker}},
\code{\link{graySmoothed}}, and
\code{\link{linearDetect}}).}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (ORCID: <https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## (not run) Read one image sample in folder of package
## measuRing:
image1 <- system.file("P105_a.tif", package="measuRing")        
## column numbers in gray matrix to be included/avoided:
Toinc <- c(196,202,387,1564) 
Toexc <- c(21,130,197,207,1444,1484)        
##(not run) the ring borders:
borders <- ringBorders(image1,inclu = Toinc,exclu = Toexc)
str(borders)
##(not run) Plot of smoothed grays with the ring borders:
Smooth <- ts(borders[,1])
inclupix <- subset(borders,borders\%in\%TRUE)
inclucol <- as.numeric(rownames(inclupix))
xyborders <- data.frame(column=inclucol,smooth=inclupix[,1])
y.lim <- c(-0.05,0.05)
main. <- 'Ring borders'
{plot(Smooth,xlab = 'Column',ylab = 'Smoothed gray',
      main=main.,col = 'darkgoldenrod1')
 points(xyborders[,1],xyborders[,2],pch=19,cex=0.5,col='orangered')}
}
