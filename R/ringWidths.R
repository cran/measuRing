ringWidths <- structure(
    function#Ring widths 
    ### This function computes tree-ring widths and formation years. 
    (
        pixtypes, ##<< Data frame. Included and excluded borders from pixelTypes
        last.yr##<<Numeric. Year of formation of newest ring in image section.       
    )
    {
        finald <- pixtypes[pixtypes[,"pixtype"]%in%c('automatic','included'),]
        if(nrow(finald)==0)return(NA)
        else{
            f.label <- function(x){
                x[,'item'] <- c(1:nrow(x))
                x[,'growth'] <- with(x,(max(distance) - distance))
                year1 <- last.yr+ 1
                x[,'year'] <- with(x,year1-item)
                x[,'delta'] <- with(x,c(rev(diff(rev(growth))),0))
                x <- x[1:(nrow(x)-1),c('year','delta')]}
            return(f.label(finald))}
            ###Data frame with two columns: formation year (year) and rind-widths (delta).
    }
,
    ex=function(){
        image = system.file("P105_a.tif", package="measuRing")
        gray <- imageTogray(image = image,p.row = 1)
        smoothed <- graySmoothed(gray,ppi = 10^3)
        borders <- linearDetect(smoothed,origin = 0)
        pixtypes <- pixelTypes(smoothed,borders)
        rwidths <- ringWidths(pixtypes,last.yr = 2012)
        ## Ring witdhs should be visually controlled with
         # ringDetect (see also example in ringSelect).        
    }
)
