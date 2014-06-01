# convert positions to names
num_to_name <- function(df, value) {
  if (!missing(value) && is.numeric(value)) {
    value <- names(df)[value]
  }
  value
}

# return columns with a certain 'type' attribute
# add functionality for negative type
get_col <- function(df, type) {
  df[, get_colnum(df = df, type = type), drop = TRUE]
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
  type_index <- type_index[!is.na(type_index)]
  attr(df2, "type") <- attr(df1, "type")[type_index]
  df2
}

# returns the sum of the selected 'type' attribute
sum_type <- function(df, type) {
  cols <- get_colnum(df = df, type = type)
  type_cols <- df[, cols, drop = FALSE]
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

# return all claims at latest evaluation date
get_latest <- function(df, type = NULL) {
  calendar <- get_col(df = df, type = "origin") +
                 get_col(df = df, type = "dev")
  attr(df, "type")[length(attr(df, "type"))] <- "calendar"
  latest <- df[calendar == max(calendar), ]
  latest <- carry_attr(df1 = df, df2 = latest)
  if (is.null(type)) {
    return(latest)
  } else {
    latest <- get_col(df = latest, type = c("id", "origin", "dev",
                                         "evaluation_date", type))
    latest <- carry_attr(df1 = df, df2 = latest)
    return(latest)
  }
}

#' merge wrapper for data frame using 'type' attribute
#'
#' @param df
#' @param eval1
#' @param eval2
#' @param by vector of column names to merge by
#' @param columns to be excluded from the merge
#' 
merge_loss_df <- function(df, eval1, eval2, by, exclude) {
  group1 <- df[get_col(df = df, type = "evaluation_date") == eval1, -exclude]
  group2 <- df[get_col(df = df, type = "evaluation_date") == eval2, -exclude]
  comparison <- merge(group1, group2, by = get_colname(df = df, type = by),
                      all.x = TRUE, all.y = TRUE, suffixes = c(paste0("_", eval1), paste0("_", eval2)))
  comparison[is.na(comparison)] <- 0
  comparison
}