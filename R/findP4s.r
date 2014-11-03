# findP4s.R
# Authors:
#  Norbert Billet <norbert.billet@ird.fr>
#  Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Creation Date: 2013/11/14
# Revision Date: -
#=======================

# Find a proj4 definition from a srsName
#
# Arguments:
# - srsName: name of the spatial reference system
# - morphToESRI: use the ESRI WKT representation
# Value:
#  List of proj4 defnitions where the WKT GEOGCS attribute is equal to the required srsName
#
# Comment:
#  We actually except lat/long coordinates systems due to confusion in output of different WFS vendors
#
findP4s <- function(srsName, morphToESRI=FALSE) {
  
  if (missing(srsName)) {
    stop("please provide a spatial reference system name")
  }
  proj.lst <- as.character(projInfo("proj")$name)
  #we remove the latlong proj for compatibility with sp
  proj.lst <- proj.lst[proj.lst != "latlong" & proj.lst != "latlon"]
  #build combinations of know proj and datum
  proj.datum.grd <- expand.grid(proj=proj.lst, datum=as.character(projInfo("datum")$name), stringsAsFactors=FALSE)
  #remove the carthage datum which make my system crash
  proj.datum.grd <- proj.datum.grd[proj.datum.grd$datum != "carthage", ]
  #function to ask WKT representation
  getShowWkt <- function(x) {
    res <- try(showWKT(paste("+proj=", x[1], " +datum=", x[2], sep=""), morphToESRI=morphToESRI), silent=TRUE)
    if (class(res) == "try-error") {
      return(NA)
    } else {
      return(res)
    }
  }
  #ask WKT for all projection
  GCS <- apply(proj.datum.grd, 1, FUN=getShowWkt)	
  GCS.df <- data.frame(proj=proj.datum.grd$proj, datum=proj.datum.grd$datum, WKT=GCS, stringsAsFactors=FALSE)
  #keep only valids
  GCS.df <- GCS.df[! is.na(GCS.df$WKT),]
  #the pattern to find
  pattern <- paste("GEOGCS[\"", srsName, "\"", sep="")
  #search for pattern
  GCS.df <- GCS.df[substr(tolower(GCS.df$WKT), 1, nchar(pattern)) == tolower(pattern),]
  #keep only first SRS in case of identical WKT representation
  GCS.df <- GCS.df[!duplicated(GCS.df$WKT),]
  if (nrow(GCS.df) > 0) {
    #return the proj4 definition
    return(paste("+proj=", GCS.df$proj, " +datum=", GCS.df$datum, sep=""))  
  } else {
    #not found, return NA
    return(NA)
  }	
}