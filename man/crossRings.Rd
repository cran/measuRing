\name{crossRings}
\alias{crossRings}
\title{dplR crossdating }
\description{This function implements routines to crossdate TRWs.}
\usage{crossRings(mdr, smp = 1, 
    ncol = 1:length(mdr), 
    fun = "corr", ...)}
\arguments{
  \item{mdr}{\code{list}. Set of detected TRWs such
as that produced by \code{\link{multiDetect}}.}
  \item{smp}{\code{numeric} or \code{character}. Position or name
in the set of the sample being crossdated.}
  \item{ncol}{\code{numeric} or
\code{character}. Positions or names in the
set of the TRWs used to crossdate the
sample.}
  \item{fun}{\code{character}. Function
to be implemented. Three functions can be used:
\code{\link[dplR]{corr.rwl.seg}} ('corr'),
\code{\link[dplR]{ccf.series.rwl}} ('ccf'), and
\code{\link[dplR]{spag.plot}} ('spag').}
  \item{\dots}{arguments to be passed to the function.}
}

\value{output of selected function.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (<https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (<https://orcid.org/0000-0001-7348-6695>)}




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
