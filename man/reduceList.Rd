\name{reduceList}
\alias{reduceList}
\title{ring-width object reduction}
\description{ring-width objects are reduced to dplR chronologies.}
\usage{reduceList(mls, name.ls = "ringWidths", empty.rm = TRUE)}
\arguments{
  \item{mls}{\code{List}. Mapped outputs from implementations
\code{\link{ringDetect}} or \code{\link{multiDetect}}
functions.}
  \item{name.ls}{Character. name of the list to be
reduced.}
  \item{empty.rm}{Logical. Remove empty lists.}
}

\value{data frame  in wide format with the ring widths.}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
    wd <- getwd()
    ## Image path:
    setwd(system.file(package="measuRing"))
    ## list of tif files
    path. <- list.files(path=getwd(),pattern='.tif')
    ## two images from path.
    alltf <- gsub('.tif','',path.)[1:2]
    ## Recursive processing (mapping) of both images with multidetect
    allim <- Map(function(x)multiDetect(x, auto.det = TRUE,
                                        last.yr = -1,plot = FALSE,
                                        segs = 7, rgb = c(0,0,1),
                                        marker = 6),alltf)
    str(allim)
    ## Reducing processed ring withs 
    wide <- reduceList(allim)
    tail(wide)
setwd(wd)

}
