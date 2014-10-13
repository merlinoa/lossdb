#' Transforms cumulative loss payments into incrmental
#' 
#' @param ldf object of class loss_df
#' @param detail column to transform from cumulative to incremental. Can be of any of
#' the allowable `detail` types for class `loss_df`
#' 
#' @export
#' 
#' @examples
#' incremental(recover_ldf, detail = "paid")
cum2incr <- function(ldf, detail) {
  if (is.loss_df(ldf)) stop("`ldf` must be of class `loss_df`")
  if (length(detail) != 1) stop("value must be of length 1")
  
  # extract columns for transformation to incremental
  cum <- select_ldf(ldf, values = c("id", "origin", "dev", detail))
  
  # use reshape2 to cast the cumulative values
  cum2 <- dcast(cum, id + origin ~ dev, sum)
  
  # calculate incremental losses
  incr <- t(apply(data.frame(rep(0, nrow(cum2)), cum2[, 3:length(cum2)]), 1, diff))
  
  # transform incremetnal data frame
  incr <- data.frame(cum2[, 1:2], incr)
  names(incr) <- c("id", "origin", 1:(length(incr) - 2))
  name_incr <- paste0(detail, "_incr")
  
  # cast incremental data frame into long form
  incr2 <- melt(incr, id.vars = c("id", "origin"), variable.name = "dev", 
                value.name = name_incr)
  
  # remove all times greater than the most recent calendar period  
  incr2$dev <- as.numeric(as.character(incr2$dev))
  incr2 <- incr2[(incr2$origin + incr2$dev) <= max(ldf$calendar), ]
  out <- merge(ldf, incr2, by = c("id", "origin", "dev"))
  out[, c("id", "origin", "dev", name_incr)]
}