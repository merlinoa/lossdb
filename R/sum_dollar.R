#' returns the sum of the selected loss_df type
#' 
#' @param ldf a data frame of class loss_df
#' @param dollar the dollar type to be summed up. `dollar` options are `paid`, 
#' `incurred`, `paid_recovery`, and `incurred_recovery`. 
#' 
#' @export
#' 
#' @examples
#' sum_dollar(ldf = ldf_data, dollar = "paid")
sum_dollar <- function(ldf, dollar) {  
  if (length(dollar) != 1 || length(intersect(detail$dollar, dollar))) {
    dollar_cats <- unname(unlist(detail$dollar))
    stop(paste0("dollar must be one of the following: ", dollar_cats))
  }
  if (dollar %in% detail$dollar$loss) {
    dollar_cols <- ldf[ , intersect(names(ldf), attr(ldf, "detail")$dollar$loss[[dollar]]), drop = FALSE]
  } else {
    dollar_cols <- ldf[ , intersect(names(ldf), attr(ldf, "detail")$dollar$recovery[[dollar]]), drop = FALSE]
  }
  apply(dollar_cols, 1, sum, na.rm = TRUE)
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
  sum_dollar(ldf, dollar = "paid")
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
  sum_dollar(ldf, dollar = "incurred")
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
  sum_dollar(ldf, dollar = "paid_recovery")
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
  sum_dollar(ldf, dollar = "incurred_recovery")
}