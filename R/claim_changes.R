#' Detect all changes in a \code{\link{loss_df}} at different evaluation dates
#' 
#' @description \code{claim_changes} is designed to make viewing all "changed claims" more simple.  
#'  A "changed claim" is a claim in a \code{\link{loss_df}} in which one or more of the \code{values}
#'  has changed from one selected evaluation date to another. A changed claim must have 
#'  a unique 'id' for it's 'origin'.  (i.e. if a claim differs origin year from 'eval1' to 
#' 'eval2' it will be returned as two seperate claims)
#' 
#' @param ldf S3 object of class \code{\link{loss_df}}
#' @param eval1 one of the evaluation dates to compare
#' @param eval2 the other evaluation date.  Data at eval2 will be compared to data at eval1.
#' @param values a vector of the names or number of the columns to be compared for changes.  If NULL all comparable
#' columns will be compared. If any column names are provided in \code{values} argument, the function will only
#' compare the columns with the corresponding names.
#' 
#' @details The motivation for \code{claim_changes} arose from the need to confirm that new claim data
#' added to the database is consistent with the claim data already in the database.  \code{claim_changes} produces a 
#' data frame consisting only of claims with changes in the selected \code{values} argument during the
#' period between the evaluation dates of interest. By scanning over all the claims that have changes one can quickly 
#' detect errors in the data (i.e. missing claims, an unexplainable decrease in paid losses, etc.).  
#' Additionally it is often beneficial when reserving to have claim detail to support the changes 
#' that occured, summarized on an origin level basis, from one reserve report to the next.  A review
#' of the claims that caused the change in the overall reserve can provide valuable information about
#' how the losses may develop.
#' 
#' @export
#' @examples
#' # return all claims with changes in all comparable columns in the 'loss_df'
#' claim_changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30")
#' 
#' # return only claims with changes in 'paid_loss_only'
#' claim_changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30", values = "paid_loss_only")
#' 
#' # return claims with changes in multiple 'values'
#' claim_changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30", 
#'         values = c("paid_loss_only", "incurred_loss_only", "claim_cts"))
#'         
#' # return claims with changes in multiple 'values' using column number
#' claim_changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30", 
#'         values = c(6, 8, 9))
claim_changes <- function(ldf, eval1, eval2, values = NULL) {
  non_values <- get_colnum(df = ldf, type = c("dev", "evaluation_date"))
  # select values to be compared depending on 'values' argument
  if (is.null(values)) {
    comparison <- merge_loss_df(ldf, eval1 = eval1, eval2 = eval2,
                                by = c("id", "origin"), exclude = non_values)
    # may want to make this a utility function and add a check for factors so it can support non numeric comparisons
    values <- setdiff(names(ldf), c(get_colname(df = ldf, type = c("id", "origin", "dev", "evaluation_date"))))
  } else {
    values <- num_to_name(df = ldf, value = values)
    ldf <- loss_df(df = ldf,
                  id = get_colname(ldf, "id"),
                  origin = get_colname(ldf, "origin"),
                  dev = get_colname(ldf, "dev"),
                  evaluation_date = get_colname(ldf, "evaluation_date"),
                  paid = intersect(values, get_colname(ldf, "paid")),
                  incurred = intersect(values, get_colname(ldf, "incurred")),
                  paid_recovery = intersect(values, get_colname(ldf, "paid_recovery")),
                  incurred_recovery = intersect(values, get_colname(ldf, "incurred_recovery")),
                  desc = intersect(values, get_colname(ldf, "desc")))
    comparison <- merge_loss_df(ldf, eval1 = eval1, eval2 = eval2, 
                                by = c("id", "origin"), exclude = non_values)
  }
  for (i in 1:length(values)) {
    comparison[, length(comparison) + 1] <- comparison[, paste0(values[i], "_", eval1)] -
      comparison[, paste0(values[i], "_", eval2)]
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