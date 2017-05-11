\name{crossRings}
\alias{crossRings}
\title{dplR crossdating }
\description{Three crossdating functions in \code{\link{dplR}} package are
implemented.}
\usage{crossRings(mdr, tocmp = 1, match.str = c(1, 1), from.to = c(1, 
    2), fun = "corr", ...)}
\arguments{
  \item{mdr}{\code{list} of \code{\link{multiDetect}}
objects.}
  \item{tocmp}{\code{numeric} or \code{character}. Either sample
position in list or sample name in the \code{mdr} list
to be cross-dated.}
  \item{match.str}{\code{numeric} vector with two character
positions in the the sample names to be
matched with vector \code{from.to}, with
matched samples being used to develop the
cross-dating process. This argument is set to
c(1,1) when any of the arguments in
\code{tocmp} or \code{from.to} are
\code{numeric}.}
  \item{from.to}{\code{numeric} vector or \code{character}
pattern with the form c(from,to). numeric
values specify column positions in list
\code{mdr}. if character then this argument is
a complementary pattern to tocmp.}
  \item{fun}{\code{character}. Any among three functions in
dplR to be implemented: \code{\link{corr.rwl.seg}}
('corr'), \code{\link{ccf.series.rwl}} ('ccf'), and
\code{\link{spag.plot}} ('spag').}
  \item{\dots}{arguments to be passed to the implemented function.}
}

\value{output of selected function.}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## Image path:
setwd(system.file(package="measuRing"))
## list of tif files
path. <- list.files(path=getwd(),pattern='.tif')
## two images from path.
alltf <- gsub('.tif','',path.)[2:4]
## Recursive processing (mapping) of both images with multidetect
allim <- Map(function(x)multiDetect(x, auto.det = TRUE,
                                    last.yr = -1,plot = FALSE,
                                    segs = 7, rgb = c(0,0,1),
                                    marker = 6),alltf)
str(allim)
## ccf plot
crossRings(allim,tocmp = 'P105_b', match.str = c(1,3),
           from.to = c('_c','_d'),
           ## from.to = c('_c','_d'),
           fun = 'corr',
           seg.length =8,bin.floor = 0,lag.max = 2)

}
