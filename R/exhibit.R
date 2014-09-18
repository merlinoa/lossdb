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
exhibit <- function(data, ...) UseMethod("exhibit")

# I would like to incorporate xtable or knitr package so my tables
# look nicer.  These are ok, but not nice enough for actual report.


#' Return a triangle for age to age development factors
#' 
#' @param ata object of class ata generated from \code{ChainLadder} package
#' @param selection optional selected development factors
#' 
#' @keywords internal
#' @method exhibit ata
#' 
#' @export
#' @examples
#' library(ChainLadder)
#' tri <- as.triangle(recovery_ldf, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#' dev_tri <- ata(tri)
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075, NA))
#' 
#' # with tail factor selected
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075, 1.1))
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
  format(round(xhbt, 3), digits = 3, nsmall = 3)
}

#' Returns a cleaner development triangle for use in reports
#' 
#' @param tri object of class triangle generated from \code{ChainLadder} package
#' 
#' @keywords internal
#' @method exhibit triangle
#' 
#' @export
exhibit.triangle <- function(tri) {
  xhbt <- format(tri[1:nrow(tri), 1:ncol(tri)], big.mark = ",")
  xhbt <- as.data.frame(xhbt)
  names(xhbt) <- attr(tri, "dimnames")[[2]]
  xhbt
}

#' Returns a ratio triangle for use in reports
#' 
#' @param tri object of class ratio and triangle generated from \code{ChainLadder} package
#' 
#' @keywords internal
#' @method exhibit ratio
#' 
#' @export
exhibit.ratio <- function(tri) {
  xhbt <- as.data.frame(tri[1:nrow(tri), 1:ncol(tri)])
  xhbt <- format(round(xhbt, 3), digits = 3, nsmall = 3)
  xhbt
}