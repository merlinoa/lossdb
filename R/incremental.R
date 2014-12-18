#' Transforms cumulative dollar value in a loss_df object into incremental
#' 
#' @param ldf object of class loss_df
#' @param dollar column to transform from cumulative to incremental. Can be of any of
#' the allowable `dollar` types for class \code{loss_df} or any of the columns in the `dollar`
#' type for class \code{loss_df}.
#' @param include_all logical. default = FALSE.  Whether or not to calculate incremental
#' losses for development years where data from prior development year is missing.
#' 
#' @details \code{incremental} turns cumulative loss values into incemental amounts.
#' Claims that are not reported until after the first development period after their
#' origin are given an incremental payment in the first at the time they are first reported.
#' 
#' @export
#' 
#' @examples
#' incremental(test_df, dollar = "paid")
#' 
#' # nexy two compare the effect of `include_all` argument
#' incremental(test_df_incomplete, dollar = "paid")
#' incremental(test_df_incomplete, dollar = "paid", include_all = TRUE)
incremental <- function(ldf, dollar, include_all = FALSE) {
  if (!is.loss_df(ldf)) stop("`ldf` must be of class `loss_df`")
  if (length(dollar) != 1) stop("value must be of length 1")
  
  # extract columns for transformation to incremental
  cum <- select_ldf(ldf, values = c("id", "origin", "dev", dollar))
  
  # identify oldest origin period.
  # will be used to add zeros for claims not reported in their
  # first development period
  oldest_origin <- min(ldf$calendar) - 1
  
  # use reshape2 to cast the cumulative values
  cum2 <- dcast(cum, 
                formula = id + origin ~ dev, 
                value.var = dollar)
  
  # fill in missing years with zeros for NA where appropriate
  if (oldest_origin > min(ldf$origin)) {
    ## seperate origin years with complete data from years with incomplete data
    # partial claims are missing data for early development periods
    if (include_all) { # if you want to include increments from claims with incomplete devs
      cum2[is.na(cum2)] <- 0
    } else {
      partial_claims <- cum2[cum2$origin < oldest_origin, ]
      # full claims have data for all development periods
    
      full_claims <- cum2[cum2$origin >= oldest_origin, ]
    
      # fill NA with zeros where appropriate
      full_claims[is.na(full_claims)] <- 0
    
      cum2 <- rbind_list(partial_claims, full_claims)
    }
  }

  # calculate incremental losses
  incr <- t(apply(data.frame(rep(0, nrow(cum2)), cum2[, 3:length(cum2)]), 1, diff))
  
  # transform incremental data frame
  incr <- data.frame(cum2[, 1:2], incr)
  names(incr) <- c("id", "origin", 1:(length(incr) - 2))
  name_incr <- paste0(dollar, "_incr")
  
  # melt incremental data frame back into long form
  incr2 <- melt(incr, id.vars = c("id", "origin"), variable.name = "dev", 
                value.name = name_incr)
  
  incr2$dev <- as.numeric(as.character(incr2$dev))
  # remove all calendar additional periods that were created by
  # cast and melt
  out <- merge(ldf, incr2, by = c("id", "origin", "dev"))
  out[, c("id", "origin", "dev", name_incr)]
}