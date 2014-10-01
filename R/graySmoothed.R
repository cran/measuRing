graySmoothed <- structure(
    function#Smoothed grays
    ###Averaging, detrending and smoothing of columns in gray matrix. 
    (
        gray, ##<< Matrix. Gray matrix. See imageTogray
        ppi ##<<Numeric. Image resolution in points per inch.        
    )
    {
        average.ingray <- function(gray){
        e <- apply(gray, 1, function(x) exp(mean(log(x))))
        rowN <- as.numeric(rownames(data.frame(gray)))
        data2 <- data.frame(N_pixel=rowN,V_pixel=e)
        return(data2)}
        data2 <- average.ingray(gray)
        lagsel <- lagIngray(gray)
        scale <- 25.4/ppi ## (mm)
        data2$m.av <- rollapply(data2[,'V_pixel'],lagsel,mean,fill='extend') #CRAN::zoo
        data2$cent <-data2[,'V_pixel']-data2$m.av
        data2$smooth <- smooth(na.omit(data2$cent),twiceit=FALSE)
        data2$distance <- with(data2,scale*N_pixel)
        return(data2)
        ###Data frame with six columns: pixel number (N_pixel), gray value (V_pixel), moving average (m.av), centered gray (cent), smoothed gray (smooth), and distances (mm) from left border of image section (distance).
    }
,
    ex=function(){
        image = system.file("P105_a.tif", package="measuRing")
        gray <- imageTogray(image = image,p.row = 1)
        smoothed <- graySmoothed(gray,ppi = 10^3)
        summary(smoothed)
    }
)
