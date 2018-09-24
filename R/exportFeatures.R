#' @name exportFeatures
#' @title exportFeatures
#' @description Export sp object to well-known formats
#' 
#' @param features the sp object to export
#' @param outputFormat the format of the output file, by default "SHP"
#' @param tozip object of class "logical" indicating if a zip should be created.
#'        Default is FALSE.
#' @param file.path the base path where to export the features
#' @param file.name the name of the output file
#'
#' @note only supported for GML and SHP
#'
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'         Norbert Billet \email{norbert.billet@@ird.fr}
#'
exportFeatures <- function(features, outputFormat = "SHP", tozip = FALSE,
                           file.path = NULL, file.name = NULL){
  
  uuid <- UUIDgenerate()
  if(is.null(file.name)){
    file.name <- uuid
  }
  if(is.null(file.path)){
    file.path <- file.path(tempdir(),uuid)
    dir.create(file.path)
  }
  
  output <- NULL
  
  #manage output formats
  if(outputFormat == "SHP"){
    
    gdalIconv <- .Call("RGDAL_CPL_RECODE_ICONV", PACKAGE="rgdal")
    if(gdalIconv) setCPLConfigOption("SHAPE_ENCODING", NULL)
    
    writeOGR(features, file.path, file.name, driver="ESRI Shapefile", overwrite_layer=T)
    writeEncFile <- function(extension, encoding = "UTF-8"){
      encFile <- file(paste(file.name, extension, sep="."))
      writeLines(encoding, encFile, sep="")
      unlink(encFile)
    }
    writeEncFile("cst")
    writeEncFile("cpg")
 
    shapefiles <- list.files(file.path, pattern = file.name, full.names=TRUE)
    
    if(tozip){
      outputFile<-paste(file.path,"/",file.name,".zip",sep="")
      zip(zipfile=outputFile, flags="-r9Xj", files=shapefiles) # requires R_ZIPCMD to be set in linux OS.
      unlink(shapefiles)
      output <- outputFile
    }else{
      output <- shapefiles
    }

  }else if(outputFormat == "GML"){
    outputFile <- paste(file.path, "/", file.name, ".gml", sep="")
    writeOGR(features, dsn = outputFile, layer = file.name, driver="GML", encoding = "UTF-8")
    if(tozip){
      outputZipFile <- paste(file.path, "/", file.name, ".zip", sep="")
      zip(zipfile=outputZipFile, flags="-r9Xj", files=outputFile) # requires R_ZIPCMD to be set in linux OS.
      unlink(outputFile)
      output <- outputZipFile
    }else{
      output <- outputFile
    }
  }else{
    stop("Unsupported output format")
  }
  
  if (length(list.files(file.path, pattern = file.name, full.names = TRUE)) == 0) {
    stop("Error when creating ", outputFormat)
  }
  
  return(output)
}
