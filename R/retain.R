#' Return retained amounts by claim
#' 
#' @param ldf data frame of class loss_df
#' @param value
#' @param recovery
#' 
#' @export
#' 
#' @examples
#' head(retain(recovery_ldf, value = "paid", recovery = c("paid_excess250", "sal_sub")))
#' 
#' head(retain(recovery_ldf, value = "paid", recovery = "paid_recovery"))
retain <- function(ldf, value, recovery = NULL) {
  if (length(value) != 1) stop("value must be of length 1")
  proj_df <- get_col(df = ldf, type = c("origin", "dev"))
  # value to be projected
  if (value %in% types$values) {
    proj_df$value <- sum_type(df = ldf, type = value)      
  } else {
    proj_df$value <- ldf[, value]
  }
  # recovery
  if (!is.null(recovery)) {
    for (i in intersect(recovery, types$recovery)) {
      proj_df[, i] <- sum_type(df = ldf, type = recovery[i])
    } 
    for (j in intersect(recovery, names(ldf))){
      proj_df[, j] <- ldf[, j, drop = FALSE]
    }
  }
  if (length(proj_df) > 3) {
    proj_df$net <- proj_df$value - apply(proj_df[, 4:ncol(proj_df), drop = FALSE], 1, sum, na.rm = TRUE)
  }
  proj_df
}