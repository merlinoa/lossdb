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

#' returns the loss df with each type attribute summed up
#' 
#' @param ldf a data frame of class loss_df
#' @param sum_types the types to be summed up.  Possible `sum_types` are `paid`, `incurred`, `paid_recovery`, and `incurred_recovery`. 
#' 
#' @export
#' 
#' @examples
#' by_type(ldf = recovery_ldf, sum_types = "paid")
#' by_type(ldf = recovery_ldf, sum_types = c("paid", "incurred")
sum_by_type <- function(ldf, sum_types) {
  smry_df <- get_col(df = ldf, type = c("id", "origin", "dev"))
  # summed types
  summed_types <- lapply(sum_types, function(y) sum_type(df = ldf, type = y))
  smry_df <- cbind(smry_df, as.data.frame(summed_types))
  names(smry_df) <- c(get_colname(df = ldf, type = c("origin", "dev")), sum_types)
  smry_df
}


#' returns the total gross paid amounts per claim
#' 
#' @param ldf a data frame of class loss_df
#' 
#' @export
#' 
#' @examples
#' paid(recovery_ldf)
paid <- function(ldf) {
  sum_by_type(ldf, sum_types = "paid")
}

#' returns the total gross incurred amounts per claim
#' 
#' 
#' @param ldf a data frame of class loss_df
#' 
#' @export
#' 
#' @examples
#' paid(recovery_ldf)
incurred <- function(ldf) {
  sum_by_type(ldf, sum_types = "paid")
}

