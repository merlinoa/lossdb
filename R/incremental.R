#' Transforms cumulative loss payments into incremental
#' 
#' @param ldf object of class loss_df
#' @param dollar column to transform from cumulative to incremental. Can be of any of
#' the allowable `dollar` types for class `loss_df`
#' 
#' @details `incremental` turns cumulative loss values into incemental amounts.  It currently
#' only includes incremental amounts if the previous data for the previous calendar period
#' is included in the data (i.e. if your trinagle is missing some of the early dev periods from
#' some of the old origin periods, the first time data is reported for those claims it will not
#' count as an incremental payment.)  Claims that are not reported until after development after the
#' first development period after their origin are given an incremental payment in the first at
#' the time they are first reported.
#' 
#' @export
#' 
#' @examples
#' incremental(ldf_data, dollar = "paid")
incremental <- function(ldf, dollar) {
  if (!is.loss_df(ldf)) stop("`ldf` must be of class `loss_df`")
  if (length(dollar) != 1) stop("value must be of length 1")
  
  # extract columns for transformation to incremental
  cum <- select_ldf(ldf, values = c("id", "origin", "dev", dollar))
  
  # identify oldest origin period to add zeros in to replace
  # missing values
  oldest_origin <- min(ldf$calendar) - 1
  
  # use reshape2 to cast the cumulative values
  cum2 <- dcast(cum, id + origin ~ dev)
  
  # fill in missing years with zeros for NA where appropriate
  if (oldest_origin > min(ldf$origin)) {
    # seperate origin years with complete data from years with incomplete data
    partial_origins <- min(cum2$origin):(oldest_origin - 1)
    partial_claims <- cum2[cum2$origin %in% partial_origins, ]
    
    full_origins <- oldest_origin:max(cum2$origin)
    full_claims <- cum2[cum2$origin %in% full_origins, ]
    
    # fill NA with zeros where appropriate
    full_claims[is.na(full_claims)] <- 0
    
    # remove zeros added to future claims
    full_claims <- melt(full_claims, id.vars = c("id", "origin"), variable.name = "dev")
    full_claims$dev <- as.numeric(as.character(full_claims$dev))
    full_claims <- full_claims[full_claims$dev + full_claims$origin <= max(ldf$calendar), ]
    
    full_claims <- dcast(cum, id + origin ~ dev)
    
    cum2 <- rbind_list(partial_claims, full_claims)
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