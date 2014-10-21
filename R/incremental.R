#' Transforms cumulative loss payments into incremental
#' 
#' @param ldf object of class loss_df
#' @param dollar column to transform from cumulative to incremental. Can be of any of
#' the allowable `dollar` types for class `loss_df`
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
  
  # use reshape2 to cast the cumulative values
  cum2 <- dcast(cum, id + origin ~ dev, sum)
  
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