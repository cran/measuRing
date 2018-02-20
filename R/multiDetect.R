multiDetect <- structure(function# Multiple detection of TRWs
### This function recursively detects TRWs in sets of scanned images
### of wood or gray matrices.
(
    image,##<< \code{character} or \code{list}. Vector of Paths to the
          ##image files or set of gray matrices.
    ... ##<<Further arguments to be passed to \code{\link{ringDetect}}
        ##and \code{\link{ringSelect}}.
) {
    
    umplmp <- function(image,
                       ...){
        furt <- list(...)
        image. <- basename(image)
        image. <-  gsub('\\..*','',image.)
        min <- 'Including'
        mex <- 'Excluding'
        lor <- 'ring borders in'
        autoGen <- do.call(
            ringDetect,
            list(image,
                 tit=paste(min, lor, image.,'...'),...))
        if(!'plot'%in%names(furt) || furt[['plot']])
        {
            incGen <- ringSelect(autoGen)
            if('inclu'%in%names(furt)){
                incGen <- unique(c(furt$'inclu',incGen))
            }
            autoGen <- update(
                autoGen,
                inclu = incGen,
                tit=paste(mex, lor, image.,'...'))
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
               ...),image,
        ...)
    mapping <- c(mapping, call = sys.call()) 
    return(mapping)
###\code{list}. Set of \code{\link{ringDetect}} calls.
}
, ex=function(){
    ## Paths to three image sections in the package:
    img <- system.file(c("P105_a.tif",
                         "P105_b.tif",
                         "P105_d.tif"),
                       package="measuRing")

    ## Recursive detection. Arbitrary ring borders and different years
    ## of formation of last rings in the images years are specified:
    mrings <- multiDetect(img,
                          inclu = list(c(1:40),c(1:30),c(1:41)),
                          last.yr = list(2014, 2013, 2012),
                          auto.det = c(FALSE,TRUE,FALSE),
                          plot = FALSE)
    str(mrings)

    ## Updating the call in mrings using new arguments: 
    mrings1 <- update(mrings,
                      exclu = list(c(1:4),c(1:4),c(1:4)),
                      last.yr = 2016)

})
