\name{ringSelect}
\alias{ringSelect}
\title{Visual selection}
\description{This function is used to visually include/exclude ring borders;
consequently, graphics devices from \code{\link{ringDetect}} or
\code{\link{plotSegments}} should be active.}
\usage{ringSelect(rdetect, any.col = TRUE)}
\arguments{
  \item{rdetect}{a list containing data frames of ring widths and
ring borders such as that produced by
\code{\link{ringDetect}}.}
  \item{any.col}{logical. If FALSE only those column numbers
in gray matrix previouly identified as ring
borders can be selected.}
}
\details{Columns in gray matrix are either identified and stored
by left-clicking the mouse over the central axis of gray image;
pixel numbers of just added ring borders are highlighted on such
a gray raster. The raphics devices are sequentially closed by
right-clicking the mouse. After a graphics device has been
closed, the graphics device of the following segment is
activated, and visual selection on such a new segment can be
performed. Closing the graphics devices with other procedures
will stop the selection of ring borders.}
\value{vector with column numbers in gray matrix of the
identified ring borders.}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




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
