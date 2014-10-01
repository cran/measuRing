ringSelect <- structure(
    function#Border selection
    ###Selection of pixels on central axis of plots of gray matrix in image segments to include or exclude ring borders. Grafical devices from either ringDetect or plotSegments should be active. Pixels are both: identified and stored  by left-clicking the mouse over central axis of gray image; pixel numbers of just added borders are highlighted on gray image. Graphical devices are sequentially closed by right-clicking the mouse. After a graphical device has been closed,  graphical device of the following segment is activated, and visual selection on such a new segment can be performed. Each of the processes: Inclusion and exclusion, must be performed independently.  
    (
        ringDetect,##<<Object. output from ringDetect.
        type ##<<Character. set type = 'inc' to include pixels, or type = 'exc' to avoid pixels.
    )
    {
        gray <- ringDetect$imageTogray
        pixtypes <- ringDetect$pixelTypes
        f.inclu <- function(gray,pixtypes,type){
            segs <- length(dev.list())
            if(segs==0)
                stop('there are no image segments; run ringDetect or plotSegments')
            f.rown <- function(x)as.numeric(rownames(x))
            rowpix <- f.rown(pixtypes)
            range. <- function(n)c(0,n)
            yrange <- range.(ncol(gray))
            xrange <- range(rowpix)
            included <- pixtypes[pixtypes[,"pixtype"]%in%c('automatic','included'),]
            rowinc <- f.rown(included)
            fseleca <- function(pixtypes){
                par(mfrow=c(2,1), mar=c(3,2,3,2), oma=c(2,2,0,0),xpd=NA)
                par(mfg=c(2,1))
                xy <- with(pixtypes,
                           identify(rowpix,rep(yrange[2]/2,length(rowpix)),rowpix,
                                    tolerance=0.1,cex=0.55,col='black',srt=90))
                return(xy)}
            fexcleca <- function(pixtypes){
                par(mfrow=c(2,1), mar=c(3,2,3,2), oma=c(2,2,0,0),xpd=NA)
                par(mfg=c(2,1))
                xy <- with(included,
                           identify(rowinc,rep(yrange[2]/2,length(rowinc)),rowinc,
                                    tolerance=0.1,cex=0.55,col='red',srt=90))
                ex <- as.numeric(rownames(included[xy,]))
                return(ex)}
            xy <- list()
            for(i in 1:segs)
                repeat{
                    dev.set(which=dev.list()[(segs+1)-i])
                    if(length(dev.list())==1){
                        dev.set(which=dev.cur())}
                    if(type=='inc')xy[[i]] <- fseleca(pixtypes)
                    if(type=='exc')xy[[i]] <- fexcleca(pixtypes)
                    print(xy[[i]])
                    dev.off(which=dev.cur())
                    break()}
            selected <- list()
            for(i in 1:segs)selected[[i]] <- xy[[i]]
            exclu <- unlist(selected)
            return(exclu)}
        return(f.inclu(gray,pixtypes,type))
### Vector with pixel numbers or irng borders to be included or excluded from analysis.
    }
,
    ex=function(){
        ## Output from ring.detect will be stored in object count1 (see example in ringDetect).
        count1 <- ringDetect(
            image = system.file("P105_a.tif", package="measuRing"),
            ppi = 10^3,
            last.yr = 2012,
            segs = 2,
            method = 'linear',
            inclu = NA,
            exclu = NA,
            tit = 'measuRing example')
        ## output produces two figures for including or excluding ring borders.

        ## ## uncomment and run:        
        ##  inc <- ringSelect(count1,'inc') #choose pixels on image segments to include.
        ##  ## vector inc is used to evaluate ringDetect again:
        ##  count2 <- ringDetect(
        ##      image = system.file("P105_a.tif", package="measuRing"),
        ##      ppi = 10^3,
        ##      last.yr = 2012,
        ##      segs = 2,
        ##      method = 'linear',
        ##      inclu = inc, ## Argument inclu contains now values in object inc
        ##      exclu = NA,
        ##      tit = 'measuRing example')
        ##  summary(count2$pixelTypes) #included pixels were added to pixelTypes data.
        ##  exc <- ringSelect(count2,'exc') #choose pixels on image segments to exclude.
        ##  ## Both vectors: inc and exc are used to evaluate ringDetect again:
        ##  count3 <- ringDetect(
        ##      image = system.file("P105_a.tif", package="measuRing"),
        ##      ppi = 10^3,
        ##      last.yr = 2012,
        ##      segs = 2,
        ##      method = 'linear',
        ##      inclu = inc, ## Argument inclu contains now values in object inc
        ##      exclu = exc, ## Argument exclu contains now values in object exc
        ##      tit = 'measuRing example')
    }
)
