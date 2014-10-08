#' Detect all changes in a \code{\link{loss_df}} at different calendar periods
#' 
#' @description \code{claim_changes} is designed to make create a dataframe for all values that have
#' changed from one calendar date to another.  A "changed claim" is a claim in a \code{\link{loss_df}} 
#' in which one or more of the \code{values} has changed from one selected calendar periods to another.
#' A changed claim must have a unique 'id' for it's 'origin'.  (i.e. if a claim differs origin year 
#' from 'calendar1' to calendar2' it will be returned as two seperate claims)
#' 
#' @param ldf S3 object of class \code{\link{loss_df}}
#' @param calendar1 the more recent calendar period to compare
#' @param calendar2 the older calendar period.  Data at calendar2 will be subtracted from data at calendar1.
#' @param values a vector of the names of the columns to be compared for changes.  If NULL all comparable
#' columns will be compared. If any column names are provided in \code{values} argument, the function will only
#' compare the columns with the corresponding names.
#' 
#' @details The motivation for \code{claim_changes} arose from the need to confirm that new claim data
#' added to the database is consistent with the claim data already in the database.  \code{claim_changes} produces a 
#' data frame consisting only of claims with changes in the selected \code{values} argument during the
#' period between the calendar periods of interest. By scanning over all the claims that have changes one can quickly 
#' detect errors in the data (i.e. missing claims, an unexplainable decrease in paid losses, etc.).  
#' Additionally it is often beneficial when reserving to have claim detail to support the changes 
#' that occured, summarized on an origin level basis, from one reserve report to the next.  A review
#' of the claims that caused the change in the overall reserve can provide valuable information about
#' how the losses may develop.
#' 
#' @export
#' @examples
#' # return all claims with changes in all comparable columns in the 'loss_df'
#' claim_changes(ldf_data, calendar1 = 2013, calendar2 = 2012)
#' 
#' # return only claims with changes in 'paid_loss_only'
#' claim_changes(ldf_data, calendar1 = 2013, calendar2 = 2012, values = "paid_loss_only")
#' 
#' # return claims with changes in multiple 'values'
#' claim_changes(ldf_data, calendar1 = 2013, calendar2 = 2012, 
#'         values = c("paid_loss_only", "incurred_loss_only", "claim_cts"))
claim_changes <- function(ldf, calendar1, calendar2, values = NULL) {
  if (length(get_colnum(ldf, type = "id")) == 0) {
    stop("A claim 'id' must be supplied when constructing your 'loss_df' to use the 'claim_changes' function")
  }
  
  # columns to exclude
  x_cols <- get_colnum(ldf, type = c("dev", "calendar"))
  # columns to merge by 
  by_cols <- get_colname(ldf, c("id", "origin"))
  
  # select values to be compared depending on 'values' argument
  # and create new data of merged data frames by selected calendar period
  if (is.null(values)) {
    comparison <- merge_loss_df(ldf, calendar1, calendar2, by = by_cols, exclude = x_cols)
    # may want to make this a utility function and add a check for factors so it can support non numeric comparisons
    values <- setdiff(names(ldf), c(get_colname(ldf, type = c("id", "origin", "dev", "calendar"))))
  } else {
    ldf2 <- ldf[, c(get_colname(ldf, type = c("id", "origin", "dev", "calendar")), values)]
    ldf2 <- carry_attr(ldf, ldf2)
    comparison <- merge_loss_df(ldf2, by = by_cols, exclude = x_cols)
  }
  
  # create change columns showing difference in value columns
  for (i in 1:length(values)) {
    comparison[, length(comparison) + 1] <- comparison[, paste0(values[i], "_", calendar1)] -
      comparison[, paste0(values[i], "_", calendar2)]
    names(comparison)[length(comparison)] <- paste0(values[i], "_change")
  }
  
  # find all rows containing differing values
  change <- apply(comparison[, (length(comparison) + 1 - length(values)):length(comparison), drop = FALSE], 1, 
                  function(x) ifelse(isTRUE(all.equal(x, rep(0, length(x)), check.attributes = FALSE, tolerance = .005)), FALSE, TRUE))
  
  # organize and return
  comparison <- comparison[change, ]
  comparison <- comparison[order(comparison[, 2], comparison[, 1]), ]
  comparison
}