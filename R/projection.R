#' Project Ultimate Losses
#' 
#' Simple chainladder projection using weighted age to age development factors
#' 
#'@param ldf data frame of class loss_df
#'@param value column or sum of columns with the same type attribute to project
#'@param tail optional tail development factor
#'
#'@examples
#'projection(recovery_ldf, value = "paid")
#'projection(recovery_ldf, value = "incurred")
#'projection(recovery_ldf, value = "incurred", recovery = "incurred_recovery", tail = 1.05)
projection <- function(ldf, value, recovery = NULL, tail = 1.0) {
  proj_df <- get_col(df = ldf, type = c("origin", "dev"))
  types <- c("paid", "incurred", "paid_recovery", "incurred_recovery")
  # value to be projected
  if (value %in% types) {
    proj_df$value <- sum_type(df = ldf, type = value)
  } else {
    proj_df$value <- ldf[, value]
  }
  # recovery
  if (!is.null(recovery)) {
    if (recovery %in% types) {
      proj_df$recovery <- sum_type(df = ldf, type = recovery)
    } else if (recovery %in% names(ldf)){
      proj_df$recovery <- ldf[, recovery]
    } else {
      stop("recovery values not found")
    }
  }
  # create value net recovery column if necessary
  if ("recovery" %in% names(proj_df)) {
    proj_df$total <- proj_df$value - proj_df$recovery
  } else {
    names(proj_df) <- c("origin", "dev", "total")
  }
  tri <- as.triangle(proj_df, origin = "origin", dev = "dev", value = "total")
  wtd <- attr(ata(tri), "vwtd")
  wtd <- c(wtd, tail)
  latest <- getLatestCumulative(tri)
  projection <- as.data.frame(latest)
  cumwtd <- cumprod(wtd[length(wtd):1])
  projection$cum_dev <- cumwtd
  projection$ultimate <- projection$latest * projection$cum_dev
  return(projection)
}