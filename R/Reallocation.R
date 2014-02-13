# Reallocation.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Spatial REAllocation of statistical Data (SPREAD) algorithm 
# Creation Date: 2014/02/06
# Revision Date: 2014/02/13
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
#
reallocate <- function(x, y, area.x, area.y, by.x = NULL, by.y = NULL, data, warea, wprob = NULL){
	
	#probability weight
	if(is.null(wprob)){
		prob <- rep(1L, nrow(y))
	}else{
		prob <- as.numeric(levels(y[,wprob]))
	}
	
	#pre-calculations
	if(is.factor(y[,warea])) y[,warea] <- as.numeric(as.character(y[,warea]))
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
	if(is.factor(x[,data])) x[,data] <- as.numeric(as.character(x[,data]))
	df <- merge(x = x, y = precalc, by.x = names.x, by.y = names.y)
	spread <- df[,data] * df$w / df$wsum
	result <- cbind(df, spread)
	return(result)
}
