\name{lagIngray}
\alias{lagIngray}
\title{First-local lag}
\description{This function can compute the lag of the first local on the
auto-correlation function (acf) of smoothed grays.}
\usage{lagIngray(image, acf = FALSE, ...)}
\arguments{
  \item{image}{character or matrix. Either path of an image section
or an array representing a gray matrix.}
  \item{acf}{logical. If TRUE the output is extended with the
acf.}
  \item{\dots}{arguments to be passed to \code{\link{imageTogray}}.}
}

\value{constant value of the first local on the acf of the
smoothed gray. If acf is TRUE then the computed acf is
added to the output (see \code{\link{linearDetect}}, and
\code{\link{graySmoothed}}).}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## (not run) Read one image sample in folder of package measuRing:
image1 <- system.file("P105_a.tif", package="measuRing")
##(not run) First local in the acf of smoothed grays:       
local1 <- lagIngray(image1,acf = TRUE)        
##(not run) Plot of first local over the acf: 
Flocal <- local1[['local']]
Clocal <- ts(local1[['acf']][Flocal,],start=Flocal)
acf <- ts(local1[['acf']],start=1)    
{plot(acf,type='h',col='gray',xlab='Lag',main='First local lag')
points(Clocal,pch=19,cex=0.5)}
}
