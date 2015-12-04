#basic logger facility

logger <- function(type, text){
  print(sprintf("[%s] %s", type, text))
}

logger.info <- function(text){
  logger("INFO", text)
}

logger.warn <- function(text){
  logger("WARNING", text)
}

logger.error <- function(text){
  logger("ERROR", text)
}