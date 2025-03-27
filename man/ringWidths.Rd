\name{ringWidths}
\alias{ringWidths}
\title{Ring widths }
\description{This function can compute the ring widths (mm) from the ring
borders detected on an \code{image} section.}
\usage{ringWidths(image, last.yr = NULL, 
    ...)}
\arguments{
  \item{image}{character or matrix. Either path of an \code{image} section
or an array representing a gray matrix.}
  \item{last.yr}{year of formation of the newest ring. If
NULL then the rings are numbered from one
(right) to the number of detected rings
(left).}
  \item{\dots}{arguments to be passed to two functions:
\code{\link{ringBorders}}, and/or
\code{\link{imageTogray}}.}
}

\value{data frame with the ring widths.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (<https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
## (not run) Read one image section:
image1 <- system.file("P105_a.tif", package="measuRing")       
## (not run) columns in gray matrix to be included/excluded:
Toinc <- c(196,202,387,1564) 
Toexc <- c(21,130,197,207,1444,1484)
## (not run) tree-ring widths
rwidths <- ringWidths(image1,inclu = Toinc,exclu = Toexc,last.yr=NULL)
str(rwidths)
##plot of computed tree-ring widths:
maint <- 'Hello ring widths!'
plot(rwidths,type='l',col = 'red',main = maint,
     xlab = 'Year',ylab = 'Width (mm)')    
}
