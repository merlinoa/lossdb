#' returns the weighted average developemt factors
#' 
#' @param triangle an object of class triangle defined by `ChainLadder` package
#' @increase logical just the order of the vector
#' 
#' @examples
dev_wtd <- function(triangle, increasing = TRUE) {
  dev <- ata(triangle)
  if(increasing == TRUE) {
    wtd <- attr(dev, "vwtd") 
    names(wtd) <- colnames(dev)
  } else {
    wtd <- rev(attr(dev, "vwtd"))
    names(wtd) <- rev(colnames(dev))
  }
  wtd
}

# should create generic function exhibit that can work with class 'ata' and others
# exhibit function should have option for selected dev factors