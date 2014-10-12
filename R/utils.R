# store types
meta <- c("origin", "dev", "id", "calendar")
detail <- list(dollar = list(
                  loss = c("paid", "incurred"), 
                  recovery = c("paid_recovery", "incurred_recovery")),
               "desc"
              )
        
# convert column numbers to column names
num_to_name <- function(ldf, col_num) {
  if (!missing(col_num) && is.numeric(col_num)) {
    col_name <- names(ldf)[col_num]
  } else {
    col_name <- col_num
  }
  col_name
}

# return columns for column names or by sum of dollar column
# can return multiple columns
# ldf_select(ldf_data, c("origin", "dev", "paid"))
# ldf_select(ldf_data, c("paid_excess250", "sal_sub", "paid"))
ldf_select <- function(ldf, values) {
  # return summed type for dollar detail categories
  dollar_detail <- intersect(values, unlist(detail$dollar))
  if (length(dollar_detail) > 0) {
    totals <- sapply(dollar_detail, function(x) sum_dollar(ldf, dollar = x))
  }
  # note: Probably need to find replacement for sapply as it can be inconsistent
  
  # return columns for values supplied by column name
  col_names <- setdiff(intersect(names(ldf), values), unlist(detail$dollar))
  out <- ldf[, col_names, drop = FALSE]
  
  # combine with totals for dollar columns supplied in values argument
  if (exists("totals")) {
    out <- data.frame(out, totals)
  }
  
  # order columns as supplied in values argument
  out[, match(values, names(out)), drop = FALSE]
}

#' merge wrapper for data frame using 'type' attribute
#'
#' @param df
#' @param by vector of column names to merge by
#' @param columns to be excluded from the merge
merge_loss_df <- function(df, calendar1, calendar2, by, exclude) {
  group1 <- df[df$calendar == calendar1, setdiff(names(df), exclude)]
  group2 <- df[df$calendar == calendar2, setdiff(names(df), exclude)]
  comparison <- merge(group1, group2, by = by,
                      all.x = TRUE, all.y = TRUE, suffixes = c(paste0("_", calendar1), paste0("_", calendar2)))
  comparison[is.na(comparison)] <- 0
  comparison
}