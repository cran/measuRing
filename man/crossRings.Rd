\name{crossRings}
\alias{crossRings}
\title{dplR crossdating }
\description{Three crossdating functions in \code{\link{dplR}} package are
implemented.}
\usage{crossRings(mdr, to.date = 1, ncol = 1:length(mdr), fun = "corr", 
    ...)}
\arguments{
  \item{mdr}{\code{list} of \code{\link{multiDetect}} objects.}
  \item{to.date}{\code{numeric} or \code{character}. Either sample
position in list or sample name in the \code{mdr}
list to be cross-dated.}
  \item{ncol}{\code{numeric} or
\code{character}. Positions or names of the
columns in \code{mdr} used to crossdate the
sample.}
  \item{fun}{\code{character}. Any among three functions in
\code{\link{dplR}} to be implemented:
\code{\link{corr.rwl.seg}} ('corr'),
\code{\link{ccf.series.rwl}} ('ccf'), and
\code{\link{spag.plot}} ('spag').}
  \item{\dots}{arguments to be passed to the \code{\link{dplR}} function.}
}

\value{output of selected function.}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
wd <- getwd()
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
## ccf plot
crossRings(allim,
           fun = 'ccf',
           seg.length = 10,
           bin.floor = 0,
           lag.max = 2)
setwd(wd)
}
