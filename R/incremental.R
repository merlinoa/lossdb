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
  cols <- get_col(df = ldf, type = c("id", "origin", "dev", value))
  cols
}