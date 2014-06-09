#' Generic similar to summary
#' 
#' Exhibit defines certain ways to arrange values in
#' the \code{reserve} package for presentation in a report.
#' It is similar to summary, but tailored to fit nicely
#' on a single sheet of paper.  A typical result from
#' executing the \code{exhibit} function is a data frame in
#' which each row represents an origin period and each
#' column represents a some aggregate claim loss amount.
#' 
#' @param data objext to turn into exhibit
#' @param ... additional arguments
#' 
#' @export
#' 
#' @examples
#' library(ChainLadder)
#' tri <- as.triangle(recovery_ldf, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#' dev_tri <- ata(tri)
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075, NA))
#' 
#' # with tail factor selected
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075, 1.1))
exhibit <- function(data, ...) UseMethod("exhibit")

# I would like to incorporate xtable or knitr package so my tables
# look nicer.  These are ok, but not nice enough for actual report.

#' exhibit generic method for class ata from ChainLadder package
exhibit.ata <- function(ata, selection = NULL) {
  xhbt <- as.data.frame(ata[1:nrow(ata), 1:ncol(ata)])
  final_col_name <- paste0(substr(colnames(xhbt[, ncol(xhbt), drop = FALSE]), 3, 3), "-Ult.")
  final_col <- data.frame(rep(NA, times = nrow(xhbt)))
  names(final_col) <- final_col_name
  xhbt <- cbind(xhbt, final_col)
  xhbt <- rbind(xhbt, 
                smpl = c(attr(ata, "smpl"), NA),
                wtd = c(attr(ata, "vwtd"), NA),
                sel = selection)
  format(xhbt, digits = 3, nsmall = 3)
}

#' exhibit generic method for class triangle from ChainLadder package
exhibit.triangle <- function(tri) {
  noquote(format(tri, big.mark = ","))
}