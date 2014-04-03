# Reallocation.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Spatial REAllocation of statistical Data (SPREAD) algorithm 
# Creation Date: 2014/02/06
# Revision Date: 2014/04/02
#=======================

# Computes a spatial reallocation of statistics
#
# Arguments:
# - x: a data.frame giving statistics reported for a given area
# - y: a data.frame giving intersections between 2 or more types of areas, the intersecting surface
#	   and possibly a giving a probability field
# - area.x: name of the x-field representing the source area (typically an area code)
# - area.y: name of the y-field representing the target area (typically an area code)
# - by.x: name(s) of optional additional x-fields to be mapped
# - by.y: name(s) of optional additional y-fields to be mapped
# - data: name of the x-field representing the numerical values to reallocate
# - warea: name of the y-field representing the intersecting surface
# - wprob: name of the y-field representing an additional probability value. NULL by default
# - aggregates : name(s) of the y-field(s) that represent to target geographic dimension
#
reallocate <- function(x, y, area.x, area.y, by.x = NULL, by.y = NULL, data, warea, wprob = NULL, aggregates = NULL){
	
	#probability weight
	if(is.null(wprob)){
		prob <- rep(1L, nrow(y))
	}else{
		if(!is.numeric(y[,wprob])){
			prob <- switch(class(y[,wprob]),
				"character" = as.numeric(y[,wprob]),
				"factor" = as.numeric(as.character(y[,wprob]))
			)
		}else{
			prob <- y[,wprob]
		}
	}
	
	#pre-calculations
	if(!is.numeric(y[,warea])){
		y[,warea] <- switch(class(y[,warea]),
			"character" = as.numeric(y[,warea]),
			"factor" = as.numeric(as.character(y[,warea]))
		)
	}
	precalc <- cbind(y, w = prob * y[,warea])	
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
		keys <- c(aggregates, names(data)[!(names(data) %in% c(area.x, data))])
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
