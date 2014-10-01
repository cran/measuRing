pixelTypes <- structure(
    function# Selection types
    ### Included or excluded ring borders. Pixel distances from left border of image section are specified. 
    (
        data2,##<<Data frame. Smoothed grays from graySmoothed.
        borders, ##<<Vector. Border numbers from linearDetect.
        over = NA, ##<<Numeric. Overriding constant on smoothed grays below which automatically detected borders are avoided; over = NA avoids overriding process.
        inclu = NA, ##<<Vector. Gray columns to be considered as ring borders. Default inclu = NA produces no selection of borders. See ringSelect. 
        exclu = NA##<<Vector. Gray columns of previously selected ring borders to be excluded from analysis. Default exclu = NA produces no exclusion of borders. See ringSelect.
    )
    {
        ini1 <- borders
        inclu <- unlist(inclu)
        exclu <- unlist(exclu)
        confl <- exclu[exclu%in%inclu] 
        inclu <- setdiff(inclu,confl)
        ininoinc <- setdiff(ini1,inclu)
        ininoincexc <- setdiff(ininoinc,exclu)
        ini <- data2[borders,]
        avelum <- data2[,c('distance','cent')]
        f.rown <- function(x)as.numeric((rownames(x)))
        if(is.na(over))over <- 1.1*min(data2[,'cent'])
        automatic0 <- avelum[ininoincexc,]
        automatic <- f.rown(na.omit(automatic0[automatic0[,'cent']>over,]))
        cases <- list(included=inclu,excluded=exclu,automatic=automatic)
        f.type <- function(x,type){
            dtype <- data.frame(pixtype=rep(type,length(x)))
            dtype[,'N_pixel'] <- x
            return(dtype)}
        typelist <- list()
        for(i in 1:length(cases))
            typelist[[i]] <- f.type(cases[[i]],names(cases[i]))
        types <- do.call(rbind,typelist)
        avelum[,'N_pixel'] <- f.rown(avelum)
        namelum <- c('distance','cent','pixtype')
        avelum <- merge(avelum,types,all=TRUE)[,namelum]
        avelum1 <- avelum[!is.na(avelum[,'distance']),]
        return(avelum1)
        ###Data frame with three columns: Distances (mm) from left border of image section (distance), smoothed grays (cent) and pixel types: included,excluded, or automatic (pixtypes)
    }
,
    ex=function(){
        image = system.file("P105_a.tif", package="measuRing")
        gray <- imageTogray(image = image,p.row = 1)
        smoothed <- graySmoothed(gray,ppi = 10^3)
        borders <- linearDetect(smoothed,origin = 0)
        pixtypes <- pixelTypes(smoothed,borders)
        summary(pixtypes)
        ## Pixel types should be visually controlled with ringDetect.        
 
    }
)
