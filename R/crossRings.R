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
    mdr,##<<\code{list} of \code{\link{multiDetect}}
        ##objects.
    tocmp = 1,##<<\code{numeric} or \code{character}. Either sample
              ##position in list or sample name in the \code{mdr} list
              ##to be cross-dated.
    match.str = c(1,1),##<< \code{numeric} vector with two character
                       ##positions in the the sample names to be
                       ##matched with vector \code{from.to}, with
                       ##matched samples being used to develop the
                       ##cross-dating process. This argument is set to
                       ##c(1,1) when any of the arguments in
                       ##\code{tocmp} or \code{from.to} are
                       ##\code{numeric}.
    from.to = c(1,2),##<< \code{numeric} vector or \code{character}
                     ##pattern with the form c(from,to). numeric
                     ##values specify column positions in list
                     ##\code{mdr}. if character then this argument is
                     ##a complementary pattern to tocmp.
    fun = 'corr',##<<\code{character}. Any among three functions in
                 ##dplR to be implemented: \code{\link{corr.rwl.seg}}
                 ##('corr'), \code{\link{ccf.series.rwl}} ('ccf'), and
                 ##\code{\link{spag.plot}} ('spag').
    ...##<< arguments to be passed to the implemented function.
) {
    
    ## mdr = allcounts4
    ## mdr = mixd[[4]]
    
    ## tocmp. <- 'P16008_b'
    ## fun. <- 'spag'
    ## fun. <- 'corru'
    ## fun. <- 'ccf'
    ## tocmp = tocmp.
    ## match.str = c(1,1)
    ## ## from.to = c('_a','_d')
    ## from.to = c(1:40)
    ## fun = fun.
    
    if(!is.null(names(mdr)))
        mdr <- mdr[order(names(mdr))]
        
        n.tocmp <- is.character(tocmp)
        n.from <- is.character(from.to)
        
        PGen <- 1:length(mdr)
        names(PGen) <- names(mdr)
        if(!n.tocmp)tocmp <- names(PGen[tocmp])
        if(!is.null(from.to))
            if(!n.from)from.to <- names(PGen[from.to])
            if(is.null(from.to))
                from.to <- names(PGen)
                
                fpatr <- function(x,p)x[grep(x,pattern = p)]
                patr <- substr(tocmp,match.str[1],match.str[2])
                nmw <- from.to
                if(n.from)
                    nmw <- fpatr(names(PGen),patr)
                    
                    p12 <- unique(unlist(sapply(
                        from.to,function(x)fpatr(nmw,x))))
                    nm <- nmw[grep(p12[1],nmw)[1]:grep(p12[length(p12)],nmw)]
                    
                    nt <- unique(c(nm,tocmp))
                    nt <- nt[order(nt)]
                    
                    if(is.data.frame(mdr))
                        nl <- mdr[,nt]
                        if(!is.data.frame(mdr))
                            nl <- reduceList(mdr[nt])
                            
                            flt <- nl[,tocmp]
                            names(flt) <- rownames(nl)
                            nt. <- nt[!nt%in%tocmp]
                            inp <- nl[,names(nl)%in%nt.]
                            
                            if(!is.data.frame(inp))
                                stop('master series is not data.frame')
                                
                                delete.na <- function(inp, n=0) {
                                    log <- apply(inp, 1, function(x)all(is.na(x)))
                                    inp[!log, ]}
                                inp <- delete.na(inp)
                                
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
                                
                                if(fun == 'ccf.series.rwl')
                                    argus <- c(argus,list(rwl = inp, series = flt, main = tocmp))
                                    if(fun%in%c('spag.plot','corr.rwl.seg'))
                                        argus <- c(argus,list(rwl = nl))
                                        outp <- do.call(fun,argus)
                                        return(outp)
###output of selected function.
                                        
                                        
} , ex=function(){
    ## Image path:
    setwd(system.file(package="measuRing"))
    ## list of tif files
    path. <- list.files(path=getwd(),pattern='.tif')
    ## two images from path.
    alltf <- gsub('.tif','',path.)[2:4]
    ## Recursive processing (mapping) of both images with multidetect
    allim <- Map(function(x)multiDetect(x, auto.det = TRUE,
                                        last.yr = -1,plot = FALSE,
                                        segs = 7, rgb = c(0,0,1),
                                        marker = 6),alltf)
    str(allim)
    ## ccf plot
    crossRings(allim,tocmp = 'P105_b', match.str = c(1,3),
               from.to = c('_c','_d'),
               ## from.to = c('_c','_d'),
               fun = 'corr',
               seg.length =8,bin.floor = 0,lag.max = 2)
    
})
