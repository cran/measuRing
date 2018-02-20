crossRings <- structure(function#dplR crossdating 
### This function implements routines in \code{\link{dplR}} to
### crossdate TRWs.
                        ##<<This function is a wrapper to implement
                        ##three crossdating functions in
                        ##\code{\link{dplR}}:
                        ##\code{\link{corr.rwl.seg}} ,
                        ##\code{\link{ccf.series.rwl}}, and
                        ##\code{\link{spag.plot}}.
(
    mdr,##<<\code{list}. Set of detected TRWs such
        ##as that produced by \code{\link{multiDetect}}.
    smp = 1, ##<<\code{numeric} or \code{character}. Position or name
             ##in the set of the sample being crossdated.
    ncol = 1:length(mdr),##<<\code{numeric} or
                         ##\code{character}. Positions or names in the
                         ##set of the TRWs used to crossdate the
                         ##sample.
    fun = 'corr',##<<\code{character}. Function in \code{\link{dplR}}
                 ##to be implemented. Three functions can be used:
                 ##\code{\link{corr.rwl.seg}} ('corr'),
                 ##\code{\link{ccf.series.rwl}} ('ccf'), and
                 ##\code{\link{spag.plot}} ('spag').
    ...##<< arguments to be passed to the \code{dplR} function.
) {

    nl <- reduceList(mdr)
    flt <- nl[,smp]
    names(flt) <- rownames(nl)
    if(is.numeric(smp))
        smp <- names(nl)[smp]
        
        if(is.numeric(ncol))
            ncol <- na.omit(names(nl)[ncol])
            ncol <- unique(c(ncol,smp))
            ncol <- ncol[order(ncol)] #
            
            arSel <- function(fun,...){
                mx <- list(...)
                fca <- lapply(fun,function(x)
                    names(formals(x)))
                nfr <- unlist(fca)
                sel <- mx[names(mx)%in%nfr]
                return(sel)}
            
            if(fun == 'spag') fun <- 'spag.plot'
            if(fun == 'ccf') fun <- 'ccf.series.rwl'
            if(fun == 'corr') fun <- 'corr.rwl.seg'
            
            argus <- arSel(fun,...)
            
            ## fns <- c('spag.plot',
            ##          'ccf.series.rwl',
            ##          'corr.rwl.seg')
            ## fun <- fns[grepl(fun, fns)]
            
            if(fun == 'ccf.series.rwl')
                argus <- c(argus,
                           list(rwl = nl,
                                series = smp,
                                main = smp))
                if(fun%in%c('spag.plot','corr.rwl.seg'))
                    argus <- c(argus,
                               list(rwl = nl))
                    outp <- do.call(fun,argus)
                    return(outp)    
###output of selected function.
                    
                    
} , ex=function(){
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
})
