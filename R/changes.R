#' Detect all changes in a \code{\link{loss_df}} at different evaluation dates
#' 
#' @description \code{changes} is designed to make viewing all "changed claims" more simple.  
#'  A "changed claim" is a claim in a \code{\link{loss_df}} in which one or more of the \code{values}
#'  has changed from one selected evaluation date to another. A changed claim must have 
#'  a unique 'id' for it's 'origin'.  (i.e. if a claim differs origin year from 'eval1' to 
#' 'eval2' it will be returned as two seperate claims)
#' 
#' @param df S3 object of class \code{\link{loss_df}}
#' @param eval1 one of the evaluation dates to compare
#' @param eval2 the other evaluation date.  Data at eval2 will be compared to data at eval1.
#' @param values a vector of the names or number of the columns to be compared for changes.  If NULL all comparable
#' columns will be compared. If any column names are provided in \code{values} argument, the function will only
#' compare the columns with the corresponding names.
#' 
#' @details The motivation for \code{changes} arose from the need to confirm that new claim data
#' added to the database is consistent with the claim data already in the database.  \code{changes} produces a 
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
#' changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30")
#' 
#' # return only claims with changes in 'paid_loss_only'
#' changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30", values = "paid_loss_only")
#' 
#' # return claims with changes in multiple 'values'
#' changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30", 
#'         values = c("paid_loss_only", "incurred_loss_only", "claim_cts"))
#'         
#' # return claims with changes in multiple 'values' using column number
#' changes(losses_ldf, eval1 = "2013-06-30", eval2 = "2012-06-30", 
#'         values = c(6, 8, 9))
changes <- function(df, eval1, eval2, values = NULL) {
    ## may want to change function name (i.e. to claim_changes) to avoid confusion with dplyr::changes
  non_values <- type_colnum(df_ = df, type = c("dev", "evaluation_date"))
  # function to merge loss_df by evaluation dates
    ## may want to make more generic and move to utils
  merge_loss_df <- function() {
    group1 <- df[df[, type_colnum(df_ = df, type = "evaluation_date")] == eval1, -non_values]
    group2 <- df[df[, type_colnum(df_ = df, type = "evaluation_date")] == eval2, -non_values]
    comparison <- merge(group1, group2, by = type_colname(df_ = df, type = c("id", "origin")),
                        all.x = TRUE, all.y = TRUE, suffixes = c(paste0("_", eval1), paste0("_", eval2)))
    comparison[is.na(comparison)] <- 0
    comparison
  }
  # select values to be compared depending on 'values' argument
  if (is.null(values)) {
    comparison <- merge_loss_df()
    values <- setdiff(names(df), c(type_colname(df_ = df, type = c("id", "origin", "dev", "evaluation_date"))))
  } else {
    values <- num_to_name(df = df, value = values)
    df <- loss_df(df = df,
                  id = type_colname(df_ = df, type = "id"),
                  origin = type_colname(df_ = df, type = "origin"),
                  dev = type_colname(df_ = df, type = "dev"),
                  evaluation_date = type_colname(df_ = df, type = "evaluation_date"),
                  paid = intersect(values, type_colname(df_ = df, type = "paid")),
                  incurred = intersect(values, type_colname(df_ = df, type = "incurred")),
                  paid_recovery = intersect(values, type_colname(df_ = df, type = "paid_recovery")),
                  incurred_recovery = intersect(values, type_colname(df_ = df, type = "incurred_recovery")),
                  desc = intersect(values, type_colname(df_ = df, type = "desc")))
    comparison <- merge_loss_df()
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