lagIngray <- structure(
    function#Maximum-lag window
    ###Lag corresponding with first maximum relative of auto-correlation function between smoothed grays. This value is used as window of moving average to center smoothed grays (see graySmoothed).
    (
       gray ##<< Matrix. Gray matrix from imageTogray.
    )
    {
        average.ingray <- function(gray){
        e <- apply(gray, 1, function(x) exp(mean(log(x))))
        rowN <- as.numeric(rownames(data.frame(gray)))
        data2 <- data.frame(N_pixel=rowN,V_pixel=e)
        return(data2)}
        data2 <- average.ingray(gray)
        ser1 <- ts(data2[,'V_pixel'],1:nrow(data2))
        acfs <- acf(ser1,lag.max=dim(gray)[1]/3,plot=FALSE)
        acfs1 <- data.frame(acf=acfs$acf)
        acfs1[,'dif'] <- c(NA,diff(acfs1$acf))                   
        f.lab <- function(i,data){
            ifelse(data[i,'dif']>0,1,0)}
        acfs1[,'flags'] <- sapply(1:nrow(acfs1),f.lab,acfs1)
        acfs1[,'cumflags'] <- c(NA,cumsum(acfs1$flags[2:nrow(acfs1)]))
        cumacf <- cumsum(acfs1$flags[2:nrow(acfs1)])
        Mode <- function(x) {
            ux <- unique(x)
            ux[which.max(tabulate(match(x,ux)))]}
        lim1 <- Mode(cumacf[cumacf>0])
        acfs1[,'window2'] <- with(acfs1,ifelse(cumflags>0&cumflags<=lim1,2,NA))
        window2 <- na.omit(acfs1)                   
        maxacf <- with(window2,max(acf))                   
        lagsel <- as.numeric(rownames(window2[window2$acf==maxacf,]))
        ###Computed lag.
    }
,
    ex=function(){
        image = system.file("P105_a.tif", package="measuRing")
        gray <- imageTogray(image = image,p.row = 1)
        window <- lagIngray(gray)
    }
)
