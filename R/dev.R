#' returns the weighted average developemt factors
#' 
#' @param triangle an object of class triangle defined by `ChainLadder` package
#' @param increasing logical just the order of the vector
#' 
#' @examples
#' tri <- as.triangle(recovery_ldf, origin = "origin", dev = "dev", value = "paid_loss_only")
#' dev_wtd(tri)
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