imageTogray <- structure(
    function#Gray matrix
    ### Matrix of luminances from combining three channels: red, green, and blue (rgb) of image-section.
    (
        image,##<<Character. Image name in working directory. File sould be compressed in tagged image file format (.tif) and contain rgb channels.  Character name should contain file extension:.tif.
        rgb = c(0.3,0.6,0.1), ##<<Numeric vector. Three fractions, all of them adding to one, to combine red, green, and blue channels.
        p.row = 1 ##<<Numeric. Proportion of rows in image section.
    )
    {
        tree <- readTIFF(image,native=FALSE) #R CRAN::tiff
        if(length(dim(tree))<3|dim(tree)[3]!=3)
            stop('three channels (RGB) are required')
        ttree <- aperm(tree,c(2,1,3))
        R <- ttree[,,1];G <- ttree[,,2];B <- ttree[,,3]
        gray <- rgb[1]*R + rgb[2]*G + rgb[3]*B
        width. <- round(p.row*dim(gray)[2]/2)
        l2 <- round(dim(gray)[2]/2) + width.
        l1 <- round(dim(gray)[2]/2) - width.
        gray <- gray[1:dim(gray)[1],l1:l2]
        return(gray)
        ###Gray matrix. 
    }
,
    ex=function(){
        image = system.file("P105_a.tif", package="measuRing")
        gray <- imageTogray(image = image,p.row = 1)
    }
)
