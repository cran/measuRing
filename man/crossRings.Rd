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
## Paths to three image sections in the package:
img <- system.file(c("P105_a.tif",
                     "P105_b.tif",
                     "P105_d.tif"),
                   package="measuRing")

## Recursive detection:
mrings <- multiDetect(img,
                      last.yr = 2013,
                      auto.det = TRUE,
                      plot = FALSE)

## corr analysis
crossRings(mrings,
           fun = 'corr',
           seg.length = 10,
           bin.floor = 0,
           lag.max = 2,
           make.plot = FALSE)
}
