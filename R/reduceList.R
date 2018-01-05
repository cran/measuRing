reduceList <- structure(
    function#ring-width object reduction
### ring-width objects are reduced to dplR chronologies.
(
    mls,##<<\code{List}. Mapped outputs from implementations
        ##\code{\link{ringDetect}} or \code{\link{multiDetect}}
        ##functions.
    name.ls = 'ringWidths',##<< Character. name of the list to be
                           ##reduced.
    empty.rm = TRUE ##<< Logical. Remove empty lists.
) {
    rw <- lapply(mls,function(x)
        x[[name.ls]])
    rwm <- Reduce(function(...)
        merge(..., all=T), rw)
    rownames(rwm) <- rwm[,1]
    rwm <- rwm[,2:ncol(rwm)]
    rwn <- names(na.omit(apply(
        rwm,2,function(x)
            mean(x,na.rm = TRUE))))
    if(empty.rm)
        rwm <- rwm[,rwn]
    if(!is.null(names(rwm)))
        rwm <- rwm[,order(names(rwm))]
    
    return(rwm)
###data frame  in wide format with the ring widths.
    
    
} , ex=function(){
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

})
