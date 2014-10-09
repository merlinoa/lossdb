#' returns the sum of the selected loss_df type
#' 
#' @param ldf a data frame of class loss_df
#' @param type the type to be summed up. `type` options are `paid`, `incurred`, `paid_recovery`, and `incurred_recovery`. 
#' 
#' @export
#' 
#' @examples
#' sum_type(ldf = recovery_ldf, type = "paid")
sum_type <- function(ldf, type) {  
  type_cols <- get_col(ldf, type, drop = FALSE)
  total <- apply(type_cols, 1, sum, na.rm = TRUE)
  total
}

#' returns a vector for total gross paid
#' 
#' @param ldf a data frame of class loss_df
#' 
#' @export
#' 
#' @examples
#' paid(recovery_ldf)
paid <- function(ldf) {
  sum_type(ldf, type = "paid")
}

#' returns a vector for total gross incuured
#' 
#' @param ldf a data frame of class loss_df
#' 
#' @export
#' 
#' @examples
#' incurred(recovery_ldf)
incurred <- function(ldf) {
  sum_type(ldf, type = "paid_recovery")
}

#' returns a vector for total paid recovery
#' 
#' @param ldf a data frame of class loss_df
#' 
#' @export
#' 
#' @examples
#' paid_recovery(recovery_ldf)
paid_recovery <- function(ldf) {
  sum_type(ldf, type = "paid_recovery")
}

#' returns a vector for total incurred recovery
#' 
#' @param ldf a data frame of class loss_df
#' 
#' @export
#' 
#' @examples
#' incurred_recovery(recovery_ldf)
incurred_recovery <- function(ldf) {
  sum_type(ldf, type = "incurred_recovery")
}

#' Totals the type attribues `paid`, `incurred`, `paid_recovery`, `incurred_recovery`
#'
#' @param ldf object of type loss_df
#' 
#' @examples
#' full_ldf(ldf_data)
type_totals <- function(ldf) {
  # summarize `type` by adding columns of same type together
  type <- intersect(attr(ldf, "type"), types$all)
  
  totals <- sapply(type, function(x) sum_type(ldf, type = x))
  
  totals
}