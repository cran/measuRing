grayDarker <- structure(
    function#Darker grays
    ###Identification of darker grays on smoothed gray.
    (
        data2, ##<<Data frame. Smoothed grays from graySmoothed.
        origin = 0 ##<<Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection of borders arround zero.      
    )
    {
        turnp <- turnpoints(data2[,'smooth'])[['tppos']] #CRAN::pastecs
        avelum <- data2[,c('distance','cent')]
        inidet0 <- avelum[turnp,]
        inidet <- na.omit(inidet0[inidet0[,'cent']<0,])
        f.rown <- function(x)as.numeric(rownames(x))
        return(f.rown(inidet))
        ###Vector. Gray columns of darker grays from automatic detection on gray matrix. 
    }
,
    ex=function(){
        image = system.file("P105_a.tif", package="measuRing")
        gray <- imageTogray(image = image,p.row = 1)
        smoothed <- graySmoothed(gray,ppi = 10^3)
        darker <- grayDarker(smoothed)
        length(darker)   
    }
)
