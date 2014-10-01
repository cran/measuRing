linearDetect <- structure(
    function#  Linear detection
### Function for developing linear detection of ring borders.
    (
        data2,##<<Data frame. Smoothed grays from graySmoothed.
        origin = 0 ##<<Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection arround zero.

    )
    {
        turneg <- grayDarker(data2,origin)
        ini <- data2[turneg,]
        avelum <- data2[,c('distance','cent')]
        f.rown <- function(x)as.numeric(as.character(rownames(x)))
        difit <- c(f.rown(ini)[1],diff(f.rown(ini)))
        lsa <- list()
        for(i in 1:length(difit))lsa[[i]] <- rep(i,difit[i])
        inflec <- unlist(lsa)
        avelumi <- avelum[1:length(inflec),]
        avelumi[,'inflav'] <- inflec
        avelumi[,'difs'] <- c(diff(avelumi[,'cent']),NA)
        avelumii <- na.omit(avelumi[avelumi[,'difs']<0,])
        avelumii[,'abs.'] <- abs(avelumii[,'difs'])
        f.mini <- function(x)x[which.min(abs(x-origin))]
        infl <- tapply(avelumii[,'cent'],avelumii[,'inflav'],
                       function(x)f.mini(x))
        borders <- f.rown(avelum[avelum[,'cent']%in%infl,])
        return(borders)
###Vector. Gray columns of ring borders. 
    }
,
    ex=function(){
        image = system.file("P105_a.tif", package="measuRing")
        gray <- imageTogray(image = image,p.row = 1)
        smoothed <- graySmoothed(gray,ppi = 10^3)
        borders <- linearDetect(smoothed)
 ## Ring borders should be visually controlled with ringDetect.        
    }
)
