reduceList <- structure(
    function#ring-width object reduction
### ring-width objects are reduced to dplR chronologies.
(
    mls,##<<\code{List}. Object from \code{\link{multiDetect}}
    name.ls = 'ringWidths',##<< Character. name of the list to be
                           ##reduced.
    empty.rm = TRUE ##<< Logical. Remove empty lists.
) {
    mls <- mls[!names(mls)%in%'call']
    rw <- lapply(mls,function(x)
        x[[name.ls]])
    rwm <- Reduce(function(...)
        merge(..., by = 'year', all=T), rw)
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
})
