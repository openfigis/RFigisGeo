#' @name reallocate
#' @title reallocate
#' @description Computes a spatial reallocation of statistics
#

#' @param x object of class "data.frame" giving statistics reported for a given area
#' @param y object of class "data.frame" giving intersections between 2 or more types
#'        of areas, the intersecting surface and possibly giving a probability field
#' @param area.x object of class "character" giving the name of the x-field representing
#'        the source area (typically an area code)
#' @param area.y object of class "character" giving the name of the y-field representing
#'        the source area in object provided in argument y (typically an area code)
#' @param by.x object of class "character" (possibly a vector) giving the name(s) of
#'        optional additional x-fields to be mapped
#' @param by.y object of class "character" (possibly a vector) giving the name(s) of
#'        optional additional y-fields to be mapped
#' @param data object of class "character" giving the name of the x-field representing
#'        the numerical values to reallocate
#' @param warea object of class "character" giving the name of the y-field representing
#'        the intersecting surface. NULL by default
#' @param wprob object of class "character" giving the name of the y-field representing
#'        an additional probability value. NULL by default
#' @param aggregates object of class "character" (possibly a vector) giving the name(s) of
#'        the y-field(s) that represent to target geographic dimension. Default is NULL (in
#'        which case the raw computations will be returned, with no aggregation at all)
#' @return an object of class "data.frame"
#' 
#' @note At least a value has to be provided for 'warea' (area reallocation) or 'wprob'
#'       (probabilistic reallocation). Both reallocation can be combined together.
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
reallocate <- function(x, y,
                       area.x, area.y, by.x = NULL, by.y = NULL, data,
                       warea = NULL, wprob = NULL,
                       aggregates = NULL){
  
	if(is.null(warea) && is.null(wprob)){
		stop("Value is required for 'warea' (areal reallocation) or 'wprob' (probabilistic reallocation) or both (if combined)")
	}
	
		
	#area weight
	if(is.null(warea)){
    warea <- "warea"
		y[,warea] <- rep(1L, nrow(y))
	}else{
		if(!is.numeric(y[,warea])){
			y[,warea] <- switch(class(y[,warea]),
				"character" = as.numeric(y[,warea]),
				"factor" = as.numeric(as.character(y[,warea]))
			)
		}
	}
	
	#probability weight
	if(is.null(wprob)){
    wprob <- "wprob"
		y[,wprob] <- rep(1L, nrow(y))
	}else{
		if(!is.numeric(y[,wprob])){
			y[,wprob] <- switch(class(y[,wprob]),
				"character" = as.numeric(y[,wprob]),
				"factor" = as.numeric(as.character(y[,wprob]))
			)
		}
	}
	
	#pre-calculations
	precalc <- cbind(y, w = y[,wprob] * y[,warea])	
	names.x <- area.x
	names.y <- area.y
	if(!is.null(by.x)) names.x <- c(names.x, by.x)
	if(!is.null(by.y)) names.y <- c(names.y, by.y)
	if(length(names.y) == 1){
		key <- precalc[,names.y]
	}else{
		key <- apply(precalc[,names.y], 1L, paste,collapse=",")
	}
	wkey <- paste("(",key,")",sep="")
	precalc <- cbind(precalc, wkey)
	precalc <- do.call("rbind", lapply(unique(precalc[, "wkey"]),
					function (x) {
						isubset <- precalc[precalc[,"wkey"] == x,]
						wsum <- rep(sum(isubset$w),nrow(isubset))
						cbind(isubset,wsum)
					}							
			))
	
	#merge & reallocate
	if(!is.numeric(x[,data])){
		x[,data] <- switch(class(x[,data]),
				"character" = as.numeric(x[,data]),
				"factor" = as.numeric(as.character(x[,data]))
		)
	}
	df <- merge(x = x, y = precalc, by.x = names.x, by.y = names.y)
	spread <- df[,data] * df$w / df$wsum
	result <- cbind(df, spread)
	
	#aggregate data by target
	if(!is.null(aggregates)){
		keys <- c(aggregates, names(x)[!(names(x) %in% c(area.x, data))])
		keyNames <- c(keys, "spreadValue")
		if(area.y %in% aggregates){
			keys <- c(area.x, keys[keys != area.y])
			keyNames <- c(area.y, keys[keys != area.x], "spreadValue")
		}
		result <- aggregate(result$spread, by = as.list(result[keys]), FUN = "sum")
		colnames(result) <- keyNames
	}
	
	return(result)
}
