multiDetect <- structure(function# Recursive detection
### This function can recursively measure tree-ring widths in one or
### several image sections.
                         ##details<<The function can implement most of
                         ##the in-package routines for ring-border
                         ##detection, see arguments in
                         ##\code{\link{ringDetect}} and
                         ##\code{\link{ringSelect}}. Objects of type
                         ##\code{multiDetect} can be updated with new
                         ##argumenst, see examples.
(
    img.nm,##<<Character or matrix. Vector of paths to the image
           ##sections or list of gray matrices.
    ... ##<<Further arguments to be passed to \code{\link{ringDetect}}
        ##and \code{\link{ringSelect}}.
) {
    
    umplmp <- function(img.nm,
                       ...){
        furt <- list(...)
        img.nm. <-  gsub('\\..*','',img.nm)
        min <- 'Including'
        mex <- 'Excluding'
        lor <- 'ring borders in'
        autoGen <- do.call(
            ringDetect,
            list(img.nm,
                 tit=paste(min, lor, img.nm.,'...'),...))
        if(!'plot'%in%names(furt) || furt[['plot']])
        {
            incGen <- ringSelect(autoGen)
            if('inclu'%in%names(furt)){
                incGen <- unique(c(furt$'inclu',incGen))
            }
            autoGen <- update(
                autoGen,
                inclu = incGen,
                tit=paste(mex, lor, img.nm.,'...'))
            excGen <- ringSelect(autoGen,
                                 any.col = FALSE)
            if('exclu'%in%names(furt))
                excGen <- unique(c(furt$'exclu',excGen))
            autoGen <- update(autoGen,
                              inclu = incGen,
                              exclu = excGen,
                              plot=FALSE)}
        
        return(autoGen)}
    mapping <- Map(function(x,
                            ...)
        umplmp(x,
               ...),img.nm,
        ...)
    mapping <- c(mapping, call = sys.call()) 
    return(mapping)
###list of \code{\link{ringDetect}} objects.
}
, ex=function(){
    ## Paths to three image sections in the package:
    img <- system.file(c("P105_a.tif",
                         "P105_b.tif",
                         "P105_d.tif"),
                       package="measuRing")

    ## Recursive detection (arbitrary ring borders and formation years
    ## are included):
    mrings <- multiDetect(img,
                          inclu = list(c(1:40),c(1:30),c(1:41),c(1:32)),
                          last.yr = list(2014, 2013, 2012, 2011),
                          auto.det = c(FALSE,TRUE,FALSE,TRUE),
                          plot = FALSE)
    str(mrings)

    ## Updating the mrings object with new arguments: 
    mrings1 <- update(mrings,
                      exclu = list(c(1:4),c(1:4),c(1:4),c(1:4)),
                      last.yr = 2016)

})
