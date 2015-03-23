\name{multiDetect}
\alias{multiDetect}
\title{Multiple detection}
\description{This function provides tools to recursively detect the ring
borders in multiple image sections.}
\usage{multiDetect(pattern, is.png = FALSE, from = 1, to = "all", inclu.dat = NULL, 
    ...)}
\arguments{
  \item{pattern}{character with a common pattern in the names of
the image sections.}
  \item{is.png}{logical. If FALSE the tif images in
working directory are processed.}
  \item{from}{character with a complementary pattern, or
position number in folder, of the initial image to
be processed.}
  \item{to}{character with a complementary pattern, or
position number in folder, of the final image to
be processed. If this argument is 'all' then all
the images matching the argument in pattern are
processed.}
  \item{inclu.dat}{a data frame such as that contained in
the ouput of this same function with the
column numbers to be updated.}
  \item{\dots}{arguments to be passed to \code{\link{ringDetect}}
(ppi, last.yr, rgb, p.row, auto.det, darker, origin,
inclu, exclu, and plot), or to
\code{\link{plotSegments}}(segs, marker, col.marker,
ratio, and tit).}
}
\details{Users running R from IDEs and aiming to develop
visual control on several image segments should be sure
that such environments support multiple graphics devices
(see \code{\link{ringDetect}}).}
\value{list with three data frames: the tree-ring widths, the
column numbers of the detected ring borders, and the narrow
rings (see \code{\link{ringBorders}},
\code{\link{ringDetect}}, and \code{\link{plotSegments}}).}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## (not run) Set working directory:
setwd(system.file(package="measuRing"))
## (not run) List the tif images the folder:
list.files(path=getwd(),pattern='.tif')

## (not run) run multiDetect:
## -provide at least one argument of ringDetect
tmp <- multiDetect(pattern = 'P105',last.yr=2012,plot = FALSE)
##
## (not run) Excluding/changing some column numbers in tmp:
dd <- tmp$colNames
ddtest <- dd #to be compared with final outputs
dd[dd$year\%in\%1999:2012,] <- NA
tail(dd,20)
tmp1 <- update(tmp,inclu=dd,auto.det=FALSE)
dm <- tmp1$colNames
dmtest <- dm #to be compared with final outputs
##
## (not run)changing columns from tmp with visual control
## -choose five or six rings at the bark side and later
## exclude any one of them:
tmp2 <- update(tmp,plot=TRUE,to='_a',segs = 1,auto.det=FALSE)
dm2 <- tmp2$colNames
newm <- merge(dm,dm2,by='year',all.x=TRUE)
dm[,'P105_a'] <- newm[,ncol(newm)]
tmp3 <- update(tmp,inclu=dm,plot=FALSE,auto.det=FALSE)
dm3 <- tmp3$colNames
## compare initial and final columns in gray matrix
tail(ddtest,15)
tail(dmtest,15)
tail(dm3,15)
}
