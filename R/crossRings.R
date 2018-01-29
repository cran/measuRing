crossRings <- structure(function#dplR crossdating 
### Three crossdating functions in \code{\link{dplR}} package are
### implemented.
                        ##<<This function is a wrapper to implement
                        ##three crossdating functions in
                        ##\code{\link{dplR}} package:
                        ##\code{\link{corr.rwl.seg}} ,
                        ##\code{\link{ccf.series.rwl}}, and
                        ##\code{\link{spag.plot}}.
(
    mdr,##<<\code{list} of \code{\link{multiDetect}} objects.
    to.date = 1, ##<<\code{numeric} or \code{character}. Either sample
                 ##position in list or sample name in the \code{mdr}
                 ##list to be cross-dated.
    ncol = 1:length(mdr),##<<\code{numeric} or
                         ##\code{character}. Positions or names of the
                         ##columns in \code{mdr} used to crossdate the
                         ##sample.
    fun = 'corr',##<<\code{character}. Any among three functions in
                 ##\code{\link{dplR}} to be implemented:
                 ##\code{\link{corr.rwl.seg}} ('corr'),
                 ##\code{\link{ccf.series.rwl}} ('ccf'), and
                 ##\code{\link{spag.plot}} ('spag').
    ...##<< arguments to be passed to the \code{\link{dplR}} function.
) {

    nl <- reduceList(mdr)
    flt <- nl[,to.date]
    names(flt) <- rownames(nl)
    if(is.numeric(to.date))
        to.date <- names(nl)[to.date]
        
        if(is.numeric(ncol))
            ncol <- na.omit(names(nl)[ncol])
            ncol <- unique(c(ncol,to.date))
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
                                series = to.date,
                                main = to.date))
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
