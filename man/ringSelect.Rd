\name{ringSelect}
\alias{ringSelect}
\title{Visual selection}
\description{This function can include and exclude ring borders in plot outputs
from \code{\link{ringDetect}} or \code{\link{plotSegments}}. The
function is mapped by \code{\link{multiDetect}} for recursive
processing of image sections.}
\usage{ringSelect(rdetect, any.col = TRUE)}
\arguments{
  \item{rdetect}{a list containing data frames of ring widths and ring
borders such as that produced by
\code{\link{ringDetect}}.}
  \item{any.col}{logical. If FALSE only those column numbers in
gray matrix previouly identified as ring borders
can be selected.}
}
\details{Columns in gray matrix are either
identified and stored by left-clicking the
mouse over the central axis of a gray image
in the plot output; pixel numbers of just
added ring borders are highlighted on the
gray raster. The raphics devices are
sequentially closed by right-clicking the
mouse. After a graphics device has been
closed, the graphics device of the following
segment is activated, and visual selection
on such a new segment can be
performed. Closing the graphics devices with
other procedures will stop the selection of
ring borders. This detection process can be
recursively developed on several image
section using \code{\link{multiDetect}}.}
\value{vector with column numbers in gray matrix of the
identified ring borders.}
\references{Lara W., F. Bravo, and
S. Carlos. 2015. measuRing: An R package to
measure tree-ring widths from scanned images
Dendrochronologia, 34: 43-50.}
\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (ORCID: <https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Read one image in package folder:
image1 <- system.file("P105_a.tif", package="measuRing")
## (not run) Initial diagnostic:
detect1 <- ringDetect(image1,segs=2,marker=7)
##
## (not run) Choose other columns in gray matrix (see ringSelect);
## (not run) graphical devices from ringDetect should be active!
## (not run) Including columns:
##
## (uncomment and run):
## Toinc <- ringSelect(detect1)
## detect1 <- update(detect1, inclu = Toinc)
##
## (not run)  ring borders to be excluded:
## (uncomment and run):
## Toexc <- ringSelect(detect1,any.col = FALSE)
## detect1 <- update(detect1, exclu=Toexc)
## (not run) kill previous plot:
graphics.off()
}
