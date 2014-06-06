#' Generic similar to summary
#' 
#' Exhibit defines certain ways to arrange values in
#' the `reserve` package for presentation in a report.
#' It is similar to summary, but tailored to fit nicely
#' on a single sheet of paper...
#' 
#' @param data objext to turn into exhibit
#' @param ... additional arguments
#' 
#' @examples
#' tri <- as.triangle(recovery_ldf, origin = "origin", dev = "dev", value = "paid_loss_only")
#' dev_tri <- ata(tri)
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075))
exhibit <- function(data, ...) UseMethod("exhibit")

exhibit.ata <- function(ata, selection = NULL) {
  attr(ata, "sel") <- selection
  rbind(summary(ata), sel = selection)
}
# just getting started.  Needs lots of thought