\name{reduceList}
\alias{reduceList}
\title{ring-width object reduction}
\description{ring-width objects are reduced to dplR chronologies.}
\usage{reduceList(mls, name.ls = "ringWidths", 
    empty.rm = TRUE)}
\arguments{
  \item{mls}{\code{List}. Object from \code{\link{multiDetect}}}
  \item{name.ls}{Character. name of the list to be
reduced.}
  \item{empty.rm}{Logical. Remove empty lists.}
}

\value{data frame  in wide format with the ring widths.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Carlos Sierra [aut] (ORCID: <https://orcid.org/0000-0003-0009-4169>), Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Paths to three image sections in the package:
img <- system.file(c("P105_a.tif",
                     "P105_b.tif",
                     "P105_d.tif"),
                   package="measuRing")

## Recursive detection (arbitrary ring borders and formation years
## are included):
mrings <- multiDetect(img,
                      inclu = list(c(1:40),c(1:30),c(1:41)),
                      last.yr = list(2014, 2013, 2012),
                      auto.det = c(FALSE,TRUE,FALSE),
                      plot = FALSE)
## Reducing the processed ring withs 
wide <- reduceList(mrings)
tail(wide)
}
