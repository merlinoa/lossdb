# convert positions to names
num_to_name <- function(df, value) {
  if (!missing(value) && is.numeric(value)) {
    value <- names(df)[value]
  }
  value
}

# find column names that have a certain 'type' attribute
type_colname <- function(df_, type) {
  col_name <- sapply(type, function(x) names(df_[, attr(df_, "type") == x, drop = FALSE]))
  unlist(col_name)
  #as.vector(col_name)
}

# find column numbers that have a certain 'type' attribute
type_colnum <- function(df_, type) {
  col_index <- function(x) {
    colnum <- which(attr(df_, "type") %in% x)
    colnum
  }
  col_num <- sapply(type, col_index)
  unlist(col_num)
}

# carry appropriate attributes over to new data frame
carry_attr <- function(df1, df2) {
  type_index <- match(names(df2), names(df1))
  type_index <- type_index[!is.na(type_index)]
  attr(df2, "type") <- attr(df1, "type")[type_index]
  df2
}

sum_type <- function(df, type) {
  cols <- type_colnum(df_ = df, type = type)
  type_cols <- df[, cols]
  total <- apply(type_cols, 1, sum)
  total
}

# merge wrapper for data frame using 'type' attribute
merge_loss_df <- function(df, eval1, eval2, exclude) {
  group1 <- df[df[, type_colnum(df_ = df, type = "evaluation_date")] == eval1, -exclude]
  group2 <- df[df[, type_colnum(df_ = df, type = "evaluation_date")] == eval2, -exclude]
  comparison <- merge(group1, group2, by = type_colname(df_ = df, type = c("id", "origin")),
                      all.x = TRUE, all.y = TRUE, suffixes = c(paste0("_", eval1), paste0("_", eval2)))
  comparison[is.na(comparison)] <- 0
  comparison
}