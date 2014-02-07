# Reallocation.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Spatial REAllocation of statistical Data (SPREAD) algorithm 
# Creation Date: 2014/02/06
#=======================

# Computes a spatial reallocation of statistics
#
# Arguments:
# - x: a data.frame giving statistics reported for a given area
# - y: a data.frame giving intersections between 2 or more types of areas, the intersecting surface
#	   and possibly a giving a probability field
# - by.x: name of the x-field representing the source area (typically an area code)
# - by.y: name of the y-field representing the source area (typically an area code) 
# - data: name of the x-field representing the numerical values to reallocate
# - warea: name of the y-field representing the intersecting surface
# - wprob: name of the y-field representing an additional probability value. NULL by default
#
reallocate <- function(x, y, by.x, by.y, data, warea, wprob = NULL){
	
	#probability weight
	if(is.null(wprob)){
		prob <- rep(1L, nrow(y))
	}else{
		prob <- y[,wprob]
	}
	
	#pre-calculations
	if(is.factor(y[,warea])) y[,warea] <- as.numeric(levels(y[,warea]))
	precalc <- cbind(y, w = prob * y[,warea])
	wsum <- unlist(lapply(unique(precalc[, by.y]),
					function (x) {
						isubset <- precalc[precalc[,by.y] == x,]
						wsum <- sum(isubset$w)
						rep(wsum, nrow(isubset))
					}							
			))
	precalc <- cbind(precalc, wsum)
	
	#merge & reallocate
	if(is.factor(x[,data])) x[,data] <- as.numeric(levels(x[,data]))
	df <- merge(x = x, y = precalc, by.x = by.x, by.y = by.y)
	spread <- df[,data] * df$w / df$wsum
	result <- cbind(df, spread)
	return(result)
}
