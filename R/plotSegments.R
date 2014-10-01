plotSegments <- structure(
    function #Image segments
    ###This function splits gray matrix and smoothed grays in desired number of image segments for developing visual control in tree-ring detection.
    (
        gray, ##<< Matrix. Gray matrix from imageTogray
        pixtypes, ##<< Data frame. Included and excluded borders from pixelTypes
        last.yr, ##<< Numeric. Year of formation of newest ring in image section.
        origin = 0, ##<<Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection arround zero.
        over = NA, ##<<Numeric. Overriding constant on smoothed grays below which automatically detected borders are avoided; over = NA avoids overriding process.
        segs = 1,##<<Numeric. Number of segments to split gray image in segmented plot.
        ratio = NULL,##<<Vector. Two values givind aspect ratio of plot (height, and width). Values corresponding to aspect ratio of display prevent distortion in segmented plots. See also dev.new().
        tit = ''##<<Title of plot. Default tit = '' avoids title in plot.
        ## ,...##<< further arguments to modify plot; see function par().
     )
    {
        TRW <- ringWidths(pixtypes,last.yr)
        scale <- pixtypes[1,'distance'] ## (mm)
        f.chunk<- function(x,segs)
            {split(x,
                   factor(sort(rank(x)%%segs)))}
        f.rown <- function(x)as.numeric(rownames(x))
        pixels. <- f.chunk(f.rown(pixtypes),segs)
        f.split <- function(x,y){
            l <- list();for(i in 1:length(y))
                l[[i]] <- subset(x,f.rown(x)%in%y[[i]])
            return(l)}
        pixtypes. <- f.split(pixtypes,pixels.)
        TRW. <- f.split(TRW,pixels.)
        if(all(is.na(TRW)==TRUE))
            {TRW. <- list();for(i in 1:segs)TRW.[[i]] <- NA}
        range. <- lapply(pixels.,range)
        gray.<- list()
        for(i in 1:segs)gray.[[i]] <- gray[pixels.[[i]],]
        ffig <- function(gray,pixtypes,TRW,scale,origin,over,ratio,tit){
            scale <- pixtypes[1,'distance'] ## (mm)
            range. <- function(n)c(0,n)
            f.rown <- function(x)as.numeric(rownames(x))
            rowpix <- f.rown(pixtypes)
            included <- pixtypes[pixtypes[,"pixtype"]%in%c('automatic','included'),]
            rowinc <- f.rown(included)
            colnodes='lightgray'
            colnodes1='white'
            xrange <- range(rowpix)
            yrange <- range.(ncol(gray)) 
            if(is.null(ratio)) dev.new()
            if(!is.null(ratio))dev.new(width = ratio[1],height = ratio[2])
            par(mfrow=c(2,1), mar=c(3,2,3,2), oma=c(2,2,0,0),xpd=NA)
            f.series <- function(pixtypes,xlab.,ylab.){
                with(pixtypes,
                     plot(rowpix,cent,
                          xlim=xrange,axes=FALSE,type='n',xlab=xlab.,ylab=ylab.,
                          cex.lab = 0.8))}
            f.image <- function(pixtypes,xlab.,ylab.){
                with(pixtypes,
                     plot(rowpix,rep(yrange[2]/2,length(rowpix)),
                          xlim=xrange,ylim=yrange,axes=FALSE,type='n',
                          xlab=xlab.,ylab=ylab.,asp = 3,cex.lab = 0.8))}
                f.nodes <- function(pixtypes){
                par(mfg=c(1,1));f.series(pixtypes,'','')
                par(mfg=c(2,1));f.image(pixtypes,'','')
                lim2 <- grconvertY(yrange[2]/2,'user','ndc')
                par(mfg=c(1,1));f.series(pixtypes,'','')
                lim1 <- grconvertY(lim2,'ndc','user')
                with(included,
                     segments(rowinc,cent,rowinc,lim1,col=colnodes,lty=1,lwd=0.3))}
            if(length(rowinc)!=0)f.nodes(pixtypes)
            par(mfg=c(1,1));f.series(pixtypes,'','Smoothed gray')
            with(pixtypes,
                    lines(rowpix,cent,xlim=xrange,col='gray50'))
            with(pixtypes,
                 lines(xrange,rep(origin,2),lwd=0.3,lty=1,col=colnodes))
            if(!is.na(over)) with(pixtypes,
                     lines(xrange,rep(over,2),lwd=0.3,lty=2,col=colnodes))
            f.labels1 <- function(pixtypes){
                with(included,
                     points(rowinc,cent,cex=0.35,pch=19,col='black'))
                plus. <- 0.5*strheight(scale,units='user',cex=NULL)
                with(included,
                     text(rowinc,
                          cent + plus.,rowinc,cex=0.6,adj=c(0,0.5),col='gray40',srt=90))}
            if(length(rowinc)!=0)f.labels1(pixtypes)
            axis(2,col='gray',cex.axis = 0.7,cex.main = 0.7)
            par(mfg=c(2,1));f.image(pixtypes,'Column','Row')
            rasterImage(t(gray),
                        xrange[1],yrange[1],xrange[2],yrange[2],interpolate=FALSE)
            axis(2,col='gray',cex.axis = 0.7,cex.main = 0.7)
            axis(1,col='gray',cex.axis = 0.7,cex.main = 0.7)
            with(pixtypes,
                 lines(xrange,rep(yrange[2]/2,2),lwd=0.4,lty=1,col=colnodes1))
            f.labels2 <- function(included){
                with(included,segments(rowinc,rep(ncol(gray)/2,nrow(included)),
                                       rowinc,rep(ncol(gray), nrow(included)),
                                       col=colnodes1,lty=1,lwd=0.4))
                with(included,
                     points(rowinc,rep(yrange[2]/2,nrow(included)),cex=0.35,pch=19,
                            col='black'))}
            if(length(rowinc)!=0)f.labels2(included)
            par(mfg=c(1,1));f.series(pixtypes,'','')
            plus. <- 0.5*strheight(scale,units='user',cex=NULL)
            if(length(rowinc)!=0)with(included,
                         segments(rowinc,rep(min(pixtypes[,'cent']) - 2*plus.,length(rowinc)),
                                  rowinc,rep(min(pixtypes[,'cent']) - 6*plus.,length(rowinc)),
                         col='white',lty=1,lwd=0.3))
            if(length(rowinc)!=0)with(TRW,
                         text(f.rown(TRW),rep(min(pixtypes[,'cent']) - 4*plus.,nrow(TRW)),
                          year,cex=0.6,adj=c(0.5,0.5),col=colnodes,srt=90))
                              par(mfg=c(2,1));f.image(pixtypes,'','')
            par(oma=c(1.5,1,0,0))
            libname <- 'measuRing V0.1; Printing time:'
            mtext(paste(libname,
                        " ",format(Sys.time(), "%Y-%m-%d %H:%M")),
                  cex=0.5, line=0, side=SOUTH<-1, adj=0, col='gray50',outer=TRUE)
            par(oma=c(0,0,3,0))
            title(tit,outer=TRUE,cex.main=1,col.main='black')
        }
        for(i in segs:1)ffig(gray.[[i]],pixtypes.[[i]],TRW.[[i]],scale,origin,over,ratio,tit)
        ###A number of image segments as indicated in segs.
    }
 ,
     ex=function(){
         image = system.file("P105_a.tif", package="measuRing")
         gray <- imageTogray(image = image,p.row = 1)
         smoothed <- graySmoothed(gray,ppi = 10^3)
         borders <- linearDetect(smoothed,origin = 0)
         pixtypes <- pixelTypes(smoothed,borders)
         plotSegments(gray,pixtypes,last.yr = 2012,segs = 3)
         ## output prints three plots
         ## Ring borders in segmented images should be visually controlled with ringDetect.        
 
     }
 )
