\name{ringDetect}
\alias{ringDetect}
\title{Single Detection of TRWs}
\description{This function assists in the detection of TRW (mm) in a scanned
\code{image} (.tif or .png) or gray matrix, evaluating other required
functions and plotting the outputs in graphics devices. The
function can be combined with \code{\link{ringSelect}} to visually
detected TRWs. Nevertheless, the complete measurement procedure of
TRW with the package can be performed by \code{\link{multiDetect}}.}
\usage{ringDetect(image, ...)}
\arguments{
  \item{image}{\code{character} or
\code{matrix}. Vector of path to
the \code{image} section or a gray matrix
such as that produced by
\code{\link{imageTogray}}.}
  \item{\dots}{arguments to be passed to other
functions, see section of Details.}
}
\details{The scanned sample should
correspond to a horizontal window of wood
with the bark side located towards the left
area of the \code{image}, and the pit side towards
the right. The \code{image} section may not
necessarily contain both bark and pit, see
images of the Examples. The \code{image} should
contain Red, Gren, and Blue channels (rgb)
and be compressed in any of two file
formats: tif or png. These are easily
obtained by scanning wood samples with a
conventional scanner and extracting a
horizontal \code{image} section. Five functions
are internally implemented:
\code{\link{plotSegments}},
\code{\link{dataSegments}},
\code{\link{ringWidths}},
\code{\link{ringBorders}},
and/or\code{\link{imageTogray}}. These are
controlled using the following arguments:
\itemize{ \item\code{ppi = NULL}: \code{image}
resolution in points per inch. If
\code{NULL} the \code{ppi} is extracted
from the \code{image} attributes. If the
\code{ppi} is not embedded in the \code{image},
then thos argument should be provided;
\item\code{rgb = c(0.3,0.6,0.1)}: vector of
three fractions, all of them adding to one,
to combine the rgb into a gray
matrix. Defaults correspond to the
rgb-standard in the luminosity function
(Russ, 2006): green light contributes the
most to the intensity perceived by humans,
and blue light the least; \item\code{p.row
= 1}: Proportion of rows in the central
portion of the gray matrix to be processed;
\item{last.yr = NULL}: \code{NULL} or
\code{integer}. Year of formation of the
newest ring. If \code{NULL} then the rings
are numbered from one (right) to the number
of detected rings
(left);\item\code{auto.det = TRUE}:
\code{logical}. If TRUE then an algorithm
for automatic detection is implemented, see
\code{\link{linearDetect}};
\item\code{darker = TRUE}:
\code{logical}. If \code{TRUE} then the
algorithm uses the negative extremes on
smoothed grays to detect the ring
borders. If \code{FALSE} the positive
extremes are used instead;
\item\code{origin = 0}: An origin along
central portion of the smoothed gray to
find the ring borders. This value could
help to avoid noisy areas during the visual
detection process; \item\code{inclu =
NULL}: \code{NULL} or vector with column
numbers in the gray matrix, other than
those automatically detected, to be
considered as ring borders.  If \code{NULL}
no column numbers are included;
\item\code{exclu = NULL}: \code{NULL} or
vector with column numbers in gray matrix
of those ring borders to be excluded from
the analysis. If \code{NULL}, no ring
borders are excluded; \item\code{plot =
TRUE}: \code{logical}. If \code{TRUE} then
a plot is produced; \item\code{segs = 1}:
Number of \code{image} segments to be plotted;
\item\code{ratio = NULL}: \code{NULL} or
\code{numeric} vector of two values
representing the aspect ratio of the plots
(height, and width). If \code{NULL} default
in \code{\link{par}} is used;
\item\code{marker = NULL}: \code{NULL} or
\code{integer} vector with any value from 1
to 10. The rings are averaged with those
rings on either side of it and the averages
are scaled from ten (the narrowest possible
ring) to one (the broadest ring). The
narrow rings with averages larger than
\code{marker} as well as other
chronological markers (decades, centuries,
and millennia), are highlighted with red
pinpricks; \item\code{col.marker = 'red'}:
color of the markers; \item\code{tit =
TRUE}: \code{logical} or
\code{character}. A title for the plots. If
\code{TRUE} the main title is the \code{image}
name. For more than 1 segment the main
title ends with the segment number. This
argument does not work in
\code{\link{multiDetect}}.} If
users run R from Interactive Development
Environments (IDE) aiming to segment the
\code{image} section (\code{segs > 1}), they
should be sure that such environments
support multiple graphics devices. If the
argument \code{image} is a gray matrix,
then other arguments passed to
\code{\link{imageTogray}} will be
ignored. The function can be combined with
\code{\link{ringSelect}} to visually
include/exclude ring borders in the plot
output, see examples in the
\code{ringSelect} function. See
\code{\link{multiDetect}} for recursive
implementation of this function.

references<< \itemize{\item Lara W.,
F. Bravo, and S. Carlos. 2015. measuRing:
An R package to measure tree-ring widths
from scanned images.  Dendrochronologia,
34: 43-50; \item Russ, J.C., 2006. The
Image Processing Handbook, Fifth
Edition. CRC Press, Boca Raton, 817 pp.}}
\value{list of data frames with ring widths and ring
borders such as these produced by
\code{\link{ringWidths}}, and \code{\link{ringBorders}}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (<https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
image1 <- system.file("P105_a.tif", package="measuRing")
## (not run) Initial diagnostic:
detect1 <- ringDetect(image1,segs=3)
## (not run) Updating ringDetect to chage arguments;
## and flagged rings
detect1 <- update(detect1,marker=8) 
## (not run) Some noise in smoothed gray can be avoided
## by moving the origin: 
detect1 <- update(detect1,origin = -0.03)
## (not run) columns 21 and 130 are not considered now.
##
## (not run) Choose other columns in gray matrix (see ringSelect);
## (not run) graphical devices from ringDetect should be active!
## (not run) Including columns:
## (uncomment and run):
## detect1 <- update(detect1)
## Toinc <- ringSelect(detect1)
## detect1 <- update(detect1, inclu = Toinc)
## or, include the next columns: 
Toinc <- c(202,387,1564) 
detect1 <- update(detect1,inclu = Toinc)        
## (not run) Object detec1 is updated with Toinc;
##
## (not run)  ring borders to be excluded:
## (uncomment and run):
## detect1 <- update(detect1)
## Toexc <- ringSelect(detect1,any.col = FALSE)
## detect1 <- update(detect1,exclu=Toexc)
## or, exclude the nex columns: 
Toexc <- c(208,1444,1484)
detect1 <- update(detect1,exclu = Toexc)        
##
## (not run) Final arguments:
detect2 <- update(detect1,last.yr=2011,marker = 8)
str(detect2)
##
## (not run) kill previous plot:
graphics.off()
##
## (not run) Tree-ring widths and attributes:
rings <- detect2$'ringWidths'
##
## (not run) Plot of the tree-ring witdths:        
maint <- 'Hello ring widths!'
plot(rings,ylab = 'width (mm)',type='l',col = 'red',main=maint)
}
