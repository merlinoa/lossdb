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
  incr <- get_col(df = ldf, type = c("id", "dev", value))
  
  incr <- dcast(, claim.number.2 ~ dev, value.var = value)


  # identify oldest calendar period
  # incremental not to be calculated if a record of the origin period has not been maintained
  # since the origin period.
  min(ldf$calendar)

}