#' @name createCWPGrid
#' @title createCWPGrid
#' @description Creates a CWP grid spatial object
#' 
#' @param size an integer code corresponding to the grid size (referred as code
#' A in the CWP Handbook)
#' @param res a string matching one of the accepted resolution values. Accepted 
#' resolutions values are '10min_x_10min', '20min_x_20min', '30min_x_30min',
#' '30min_x_1deg', '1deg_x_1deg', '5deg_x_5deg', '10deg_x_10deg', '20deg_x_20deg',
#' '30deg_x_30deg'"
#' @param xmin xmin of the output grid
#' @param ymin ymin of the output grid
#' @param xmax xmax of the output grid
#' @param ymax ymax of the output grid
#' @param parallel run in parallel
#' @param ... parallel options
#' @return an object of class "SpatialPolygonsDataFrame"
#' 
#' @references 
#'   CWP Handbook - http://www.fao.org/fishery/cwp/handbook/G/en
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
createCWPGrid <- function(size = NULL, res = NULL,
                          xmin = NULL, ymin = NULL, xmax = NULL, ymax = NULL,
                          parallel = FALSE, ...){
  
  applyHandler <- if(parallel) parallel::mclapply else lapply
  
  #reference resolutions
  grids <- data.frame(
    size = c(1,2,3,4,5,6,7,8,9),
    lat = c(1/6, 1/3, 0.5, 0.5, 1, 5, 10, 20, 30),
    lon = c(1/6, 1/3, 0.5, 1, 1, 5, 10, 20, 30),
    res = c("10min_x_10min", "20min_x_20min","30min_x_30min", "30min_x_1deg",
            "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg", "30deg_x_30deg")
  )
  
  #bbbox
  xmin <- if(is.null(xmin)) -180 else xmin
  ymin <- if(is.null(ymin)) -90 else ymin
  xmax <- if(is.null(xmax)) 180 else xmax
  ymax <- if(is.null(ymax)) 90 else ymax
  
  #select grid resolution
  if(!is.null(size)){
    grid <- grids[grids$size == size,]
  }else{
    if(!is.null(res)){
      grid <- grids[grids$res == res,]
    }else{
      stop(sprintf("Please provide either the grid size (CWP A code) or the explicit resolution. Accepted resolutions values are %s",
                   paste(paste0("'",grids$res,"'"), collapse=", ")))
    }
  }
  
  #special case of 20deg resolution
  if(grid$size == 8){
    ymin = -80
    ymax = 80
  }
  
  #create grid
  eckp4s <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  llp4s <- "+init=epsg:4326"
  llcrs <- CRS(llp4s)
  r <- raster(extent(matrix( c(xmin, ymin, xmax,  ymax), nrow=2)),
              nrow=length(seq(ymin,ymax, grid$lat))-1, ncol=length(seq(xmin,xmax, grid$lon))-1, 
              crs = llp4s)            
  r[] <- 1:ncell(r)
  sp <- as(r, "SpatialPolygonsDataFrame")
  proj4string(sp) <- llcrs
  
  #densify adding vertices each minute
  sp <- addVertices(sp, each = 1/60, parallel = parallel, ...)
  
  #attributes (including grid coding)
  idx <- 0
  attrs <- do.call("rbind", applyHandler(sp@polygons, function(poly){
    labpt <- slot(poly, "labpt")
    quadrant <- paste0(ifelse(labpt[2]<0,"S","N"), ifelse(labpt[1]<0,"W","E"))
    quadrant_id <- switch(quadrant, "NE" = 1L, "SE" = 2L, "SW" = 3L, "NW" = 4L)
    corner_lon <- sprintf("%03.f", as.integer(min(abs(bbox(poly)[1L,]))))
    corner_lat <- sprintf("%02.f", as.integer(min(abs(bbox(poly)[2L,]))))
    gridcode <- paste0(grid$size, quadrant_id, corner_lat, corner_lon)
    
    cwp.idx <- NA
    if(grid$size < 5){
	    m.bbox <- bbox(poly)
      m <- as.integer(floor(m.bbox))
      if(m[4]==m[2]) m[4] <- m[4]+1
      if(m[3]==m[1]) m[3] <- m[3]+1
      mr <- raster(extent(matrix(m, nrow=2)), nrow=length(seq(m[2],m[4], grid$lat))-1, ncol=length(seq(m[1], m[3], grid$lon))-1, crs = NA)
      mr[] <- 1:ncell(mr)
      mr.sp <- as(mr, "SpatialPolygonsDataFrame")
      mr.seq <- as.integer(sapply(mr.sp@polygons, slot, "ID"))
      mr.seq <- switch(quadrant,
                       "SE" = as.character(mr.seq),
                       "NW" = as.character(rev(mr.seq)),
                       "NE" = as.character(unlist(rev(split(mr.seq, ceiling(seq_along(mr.seq)*grid$lon))))),
                       "SW" = as.character(unlist(rev(split(rev(mr.seq), ceiling(seq_along(rev(mr.seq))*grid$lon))))))
      spChFIDs(mr.sp) <- mr.seq
      pt <- SpatialPoints(coords = matrix(labpt,1,2))
      mr.sp.idx <- as.integer(over(pt, mr.sp))
      cwp.idx <- as.integer(slot(mr.sp[mr.sp.idx,]@polygons[[1]],"ID"))
      gridcode <- paste0(gridcode, cwp.idx)
    }
    
    df <- data.frame(GRIDTYPE = grid$res, QUADRANT = quadrant, X_COORD = labpt[1], Y_COORD = labpt[2], 
                     CWP_A = grid$size, CWP_B = quadrant_id, CWP_C = corner_lat, CWP_D = corner_lon, CWP_E = cwp.idx,
                     CWP_CODE = gridcode, SURFACE = gArea(spTransform(SpatialPolygons(Srl = list(poly), proj4string = llcrs), eckp4s)))
    return(df)
  }, ...))
  
  sp@data <- attrs
  return(sp)
}
