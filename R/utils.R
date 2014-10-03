# store types
types <- list(values = c("paid", "incurred"),
              recovery = c("paid_recovery", "incurred_recovery"),
              all = c("paid", "incurred", "paid_recovery", "incurred_recovery"))

# convert positions to names
num_to_name <- function(df, value) {
  if (!missing(value) && is.numeric(value)) {
    value <- names(df)[value]
  }
  value
}

# return columns with a certain 'type' attribute
# add functionality for negative type
get_col <- function(df, type, drop = TRUE) {
  df[, get_colnum(df = df, type = type), drop = drop]
}


# return column names that have a certain 'type' attribute
get_colname <- function(df, type) {
  col_name <- lapply(type, function(x) names(df[, attr(df, "type") == x, drop = FALSE]))
  unlist(col_name)
}

# return column numbers that have a certain 'type' attribute
get_colnum <- function(df, type) {
  col_index <- function(x) {
    colnum <- which(attr(df, "type") %in% x)
    colnum
  }
  col_num <- lapply(type, col_index)
  unlist(col_num)
}

# carry appropriate attributes over to new data frame
carry_attr <- function(df1, df2) {
  type_index <- match(names(df2), names(df1))
  attr(df2, "type") <- attr(df1, "type")[type_index]
  df2
}

# returns the sum of the selected 'type' attribute
sum_type <- function(df, type) {
  type_cols <- get_col(df = df, type = type, drop = FALSE)
  total <- apply(type_cols, 1, sum, na.rm = TRUE)
  total
}

# sum of paid `type` less sum of paid_recovery `type`
net_paid <- function(df) {
  sum_type(df = df, type = "paid") - sum_type(df = df, type = "paid_recovery")
}

# sum of incurred `type` less sum of incurred_recovery `type`
net_incurred <- function(df) {
  sum_type(df = df, type = "incurred") - sum_type(df = df, type = "incurred_recovery")
}

# return all claims at latest calendar date
get_latest <- function(df) {
  calendar <- get_calendar(df = df)
  max_cal <- max(calendar)
  latest <- df[calendar == max_cal, ]
  latest <- carry_attr(df1 = df, df2 = latest)
  return(latest)
}


#' return calandar year for each row
#' 
#' usefu
#'@param df a loss_df data frame
get_calendar <- function(df) {
  calendar <- get_col(df = df, type = "origin") + get_col(df = df, type = "dev")
}

#' merge wrapper for data frame using 'type' attribute
#'
#' @param df
#' @param calendar1
#' @param calendar2
#' @param by vector of column names to merge by
#' @param columns to be excluded from the merge
#' 
merge_loss_df <- function(df, calendar1, calendar2, by, exclude) {
  group1 <- df[df$calendar == calendar1, -exclude]
  group2 <- df[df$calendar == calendar2, -exclude]
  comparison <- merge(group1, group2, by = get_colname(df = df, type = by),
                      all.x = TRUE, all.y = TRUE, suffixes = c(paste0("_", calendar1), paste0("_", calendar2)))
  comparison[is.na(comparison)] <- 0
  comparison
}