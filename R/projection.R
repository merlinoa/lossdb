#' Project Ultimate Losses
#' 
#' Simple chainladder projection using weighted age to age development factors
#' 
#'@param ldf data frame of class loss_df
#'@param value column or sum of columns with the same type attribute
#'@param recovery column or sum of columns with the same recovery type attribute
#'@param tail optional tail development factor
#'
#'
#'@examples
#'projection(recovery_ldf, value = "paid")
#'projection(recovery_ldf, value = "paid_loss_only")
#'projection(recovery_ldf, value = "incurred")
#'projection(recovery_ldf, value = "incurred", recovery = "incurred_recovery", tail = 1.05)
projection <- function(ldf, value, recovery = NULL, tail = 1.0) {
  if (length(value) != 1) stop("value must be of length 1")
  proj_df <- get_col(df = ldf, type = c("origin", "dev"))
  types <- c("paid", "incurred")
  # value to be projected
  if (value %in% types) {
    proj_df$value <- sum_type(df = ldf, type = value)      
  } else {
    proj_df$value <- ldf[, value]
  }
  # recovery
  if (!is.null(recovery)) {
    reco <- list()
    recovery_types <- c("paid_recovery", "incurred_recovery")
    for (i in seq_along(recovery)) {
      if (all(recovery %in% recovery_types)) {
        reco[[i]] <- sum_type(df = ldf, type = recovery[i])
      } else if (all(recovery %in% names(ldf))){
        reco[[i]] <- ldf[, recovery[i]]
      } else {
        stop("recovery values not found")
      }
    }
    if (length(recovery) > 1) {
      total_recovery <- rowSums(as.data.frame(reco))
    } 
    if (length(recovery) == 1) {
      total_recovery <- unlist(reco)
    }
  }
  # create value net recovery if necessary
  if (!is.null(recovery)) {
    proj_df$total <- proj_df$value - total_recovery
  } else {
    names(proj_df) <- c("origin", "dev", "total")
  }
  tri <- as.triangle(proj_df, origin = "origin", dev = "dev", value = "total")
  wtd <- attr(ata(tri), "vwtd")
  wtd <- c(wtd, tail)
  cum_dev <- cumprod(wtd[length(wtd):1])
  latest <- getLatestCumulative(tri)
  if (is.null(recovery)) {
    smry <- data.frame(latest, cum_dev)
  } else {
    smry <- cbind(summary(ldf, values = c(value, recovery)), latest, cum_dev)
  }
  smry$ultimate <- smry$latest * smry$cum_dev
  return(smry)
}