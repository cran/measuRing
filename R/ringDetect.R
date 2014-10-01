ringDetect <- structure(
    function# Detection and measurement of tree-ring widths
### This function reads an image from file folder, develops automatic detection of tree ring-widths (trw), and provides image segments to visually control detection by excluding and including trw.
    (
        image,##<<Character. Image name in working directory. File should be compressed in tagged image file rormat (.tif) and contain three channels: red, green, and blue (rgb). Agument image should contain file extension:.tif.
        ppi, ##<<Numeric. Image resolution in points per inch.
        last.yr,##<<Numeric. Year of formation of newest ring in image section.
        rgb = c(0.3,0.6,0.1), ##<<Numeric vector. Three fractions, all of them adding to one, to combine red, green, and blue channels.        
        p.row = 1, ##<<Numeric. Proportion of rows in image section.
        origin = 0, ##<<Numeric. Constant on smoothed gray to focus border detection. Default origin = 0 focus detection arround zero.
        over = NA, ##<<Numeric. Overriding constant on smoothed grays below which automatically detected borders are avoided; over = NA avoids overriding process.
        method = NA,##<<Character. Method to be used in automatic detection of ring borders. Currently only linear detection is implemented (method = 'linear'). Default method = NA avoids automatic detection. See linearDetect.
        inclu = NA, ##<<Vector. Columns in gray matrix to be considered as ring borders. Default inclu = NA produces no selection of pixels. See ringSelect. 
        exclu = NA,##<<Vector. Columns in gray matrix of those previously identified ring borders to be excluded from analysis. Default exclu = NA produces no exclusion of borders. See ringSelect.
        segs = 1,##<<Numeric. Number of image segments.
        ratio = NULL,##<<Vector. Two values givind aspect ratio of plot (p.row, and width). Values corresponding to aspect ratio of display prevent distortion in segmented plots. See also dev.new().
        tit = ''##<<Title of image segments. Default tit = "" avoids title.
        )
    {
        gray <- imageTogray(image,rgb,p.row)
        lagsel <- lagIngray(gray)
        data2 <- graySmoothed(gray,ppi)
        turneg <- grayDarker(data2,origin)
        borders <- c()
        if(is.character(method)&&method == 'linear') borders <- linearDetect(data2,origin)
        pixtypes <- pixelTypes(data2,borders,over,inclu,exclu)
        rwidths <- ringWidths(pixtypes,last.yr)        
        plotSegments(gray,pixtypes,last.yr,origin,over,segs,ratio,tit)
        return(list(ringWidths=rwidths,pixelTypes=pixtypes,imageTogray=gray))
        ### Image segments and list with three objects: data frame with the three-ring widths (see ringWidths), data frame with pixel types (see pixelTypes),and gray matrix (see imageTogray).
    }
,
    ex=function(){
        count1 <- ringDetect(
            image = system.file("P105_a.tif", package="measuRing"),
            ppi = 10^3,
            last.yr = 2012,
            segs = 2,
            method = 'linear',
            inclu = NA,
            exclu = NA,
            tit = 'measuRing example')
        ## output produces two figures for including or excluding ring borders (see ringSelect)
    }
)
