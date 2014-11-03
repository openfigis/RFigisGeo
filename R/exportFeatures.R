# exportFeatures.R
# Authors:
#  Emmanuel Blondel <emmanuel.blondel@fao.org>
#  Norbert Billet <norbert.billet@ird.fr>
#
# Creation Date: 2013/11/14
# Revision Date: -
#=======================

# Export sp object to well-known formats
#
# Arguments:
# - features: the sp object to export
# - outputFormat: the format of the output file, by default "SHAPE-ZIP" (a zipped shapefile)
# - file.path: the base path where to export the features
# - file.name: the name of the output file
#
# Notes:
# - only supported for GML and SHAPE-ZIP
#
exportFeatures <- function(features, outputFormat = "SHAPE-ZIP", file.path = NULL, file.name = NULL){
  
  uuid <- UUIDgenerate()
  if(is.null(file.name)){
    file.name <- uuid
  }
  if(is.null(file.path)){
    file.path <- file.path(tempdir(),uuid)
    dir.create(file.path)
  }
  
  #manage output formats
  if(outputFormat == "SHAPE-ZIP"){
    writeOGR(features, file.path, file.name, driver="ESRI Shapefile")
    outputFile<-paste(file.path,"/",file.name,".zip",sep="")
    shapefiles <- list.files(file.path, pattern = file.name, full.names=TRUE)
    zip(zipfile=outputFile, flags="-r9Xj", files=shapefiles) # requires R_ZIPCMD to be set in linux OS.
    
  }else if(outputFormat == "GML"){
    outputFile <- paste(file.path, "/", file.name, ".gml", sep="")
    writeOGR(features, dsn = outputFile, layer = file.name, driver="GML")
    
  }else{
    stop("Unsupported output format")
  }
  
  if (!file.exists(outputFile)) {
    outputFile <- NA
    stop("Error when creating ", outputFormat)
  }
  
  return(outputFile)
}