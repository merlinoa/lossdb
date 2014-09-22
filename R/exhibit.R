#' Generic similar to summary
#' 
#' \code{exhibit} defines certain default ways to arrange 
#' objects returned by the package \code{ChainLadder} package
#' and \code{loss_db} packages for presentation in a report.  
#' A typical result from executing the \code{exhibit} function
#' is a data frame in which each row represents an origin 
#' period and each column represents a some aggregate claim 
#' loss amount.
#' 
#' @param data object to turn into exhibit
#' @param ... additional arguments
#' 
#' @export
exhibit <- function(data, ...) UseMethod("exhibit")


#' Return a triangle for age to age development factors
#' 
#' @param ata object of class ata generated from \code{ChainLadder} package
#' @param selection optional selected development factors
#' @param tail_column optional column for development factor of 
#' most mature age to ultimate
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
exhibit.ata <- function(ata, selection = NULL, tail_column = FALSE) {
  
  # extract development table from ata object
  xhbt <- as.data.frame(ata[1:nrow(ata), 1:ncol(ata)])
  
  if (tail_column) {
    # create name for final column
    final_col_name <- paste0(ncol(xhbt) + 1, "-Ult.")
  
    # create final column
    final_col <- data.frame(rep(NA, times = nrow(xhbt)))
    names(final_col) <- final_col_name
  
    # add final column to exhibit
    xhbt <- cbind(xhbt, final_col)
    xhbt <- rbind(xhbt, 
                smpl = c(attr(ata, "smpl"), NA),
                wtd = c(attr(ata, "vwtd"), NA),
                sel = selection)
  } else {
    xhbt <- rbind(xhbt,
                  smpl = c(attr(ata, "smpl")),
                  wtd = c(attr(ata, "vwtd")),
                  sel = selection)
  }
  
  # format the values for presentation
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


#' Returns glmReserve summary information in a data frame
#'
#' @param glmReserve object of class glmReserve
#' 
#' @keywords internal
#' @method exhibit glmReserve
#' 
#' @export
exhibit.glmReserve <- function(glmReserve) {
  xhbt <- glmReserve$summary
  latest_first_ay <- glmReserve$Triangle[1, ncol(glmReserve$Triangle)]
  first_ay <- c("Latest" = latest_first_ay, 1, "Ultimate" = latest_first_ay, "IBNR" = 0, "S.E." = NA, "CV" = NA)
  xhbt <- rbind(first_ay, xhbt)
  xhbt[, c(1, 3, 4, 5)] <- format(round(xhbt[, c(1, 3, 4, 5)], 0), big.mark = ",")
  xhbt[, c(2, 6)] <- round(xhbt[, c(2, 6)], 3)
  xhbt
}