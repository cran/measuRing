\name{imageTogray}
\alias{imageTogray}
\title{Gray matrix}
\description{This function can compute a gray matrix from the RGB in an
image section. Such an image section can be compressed in
either portable network graphics format (png) or tagged image
file format (tif).    }
\usage{imageTogray(image, ppi = NULL, rgb = c(0.3, 0.6, 0.1), p.row = 1)}
\arguments{
  \item{image}{character. path of an image section.}
  \item{ppi}{NULL or integer. If NULL the image resolution
in points per inch is extracted from attributes
in \code{image}. If this attribute is not
embedded then users should provide it}
  \item{rgb}{vector with three fractions, all of
them adding to one, to combine RGB channels into gray
matrix.}
  \item{p.row}{proportion of rows of gray matrix to be
processed.}
}

\value{a gray matrix containing the image reflectances.}

\author{Wilson Lara, Carlos Sierra, Felipe Bravo}




\examples{
## (not run) Read two image sections in package measuRing:
image1 <- system.file("P105_a.tif", package="measuRing")
image2 <- system.file("P105_a.png", package="measuRing")
## (not run) compute a gray matrix:
gray <- imageTogray(image1)
## (not run) - the ppi is embedded in the image:
attributes(gray)
## (not run) but, the ppi is not embedded in image2:
## - imageTogray will return an error:
## (uncoment and run):
## gray2 <- imageTogray(image2)
## attributes(gray2)
## - the ppi should be provided (i.e. ppi = 1200):
gray3 <- imageTogray(image2,ppi = 1200)
attributes(gray3)
##(not run) a plot of the gray matrix        
xrange <- range(0:ncol(gray)) + c(-1,1)
yrange <- range(0:nrow(gray)) + c(-1,1)    
{plot(xrange,yrange,xlim=xrange,ylim=yrange,xlab='',
      ylab='',type='n',asp=0)
rasterImage(gray,xrange[1],yrange[1],xrange[2],yrange[2])}    
}
