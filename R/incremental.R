#' Transforms cumulative loss payments into incrmental
#' 
#' @param ldf object of class loss_df
#' @param col column to transform from cumulative to incremental
#' 
#' @export
#' 
#' @examples
#' increamental(recover_ldf, value = "paid")
cum2incr <- function(ldf, value) {
  # extract columns for transformation
  if (length(value) != 1) stop("value must be of length 1")
  cum <- select_value(ldf, value)
  incr <- data.frame(get_col(ldf_data, type = c("id", "dev")), cum)
  incr <- carry_attr(ldf, ldf2 = incr)
  
  # use reshape2 to cast the cumulative values
  incr2 <- dcast(incr, get_colname(ldf, "id") ~ get_colname(ldf, "dev"),
                value.var = names(incr)[3])

  # identify oldest calendar period
  # incremental not to be calculated if a record of the origin period has not been maintained
  # since the origin period.
  min(ldf$calendar)
  incr
}