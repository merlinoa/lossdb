# store types
types <- list(meta = c("origin", "dev", "id", "calendar"),
              detail = list(
                dollar = list(
                  loss = c("paid", "incurred"), 
                  recovery = c("paid_recovery", "incurred_recovery")),
                "desc")
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

# return columns for column names or by column type attribute
# can return multiple columns
# ldf_select(ldf_data, c("origin", "dev", "paid"))
# ldf_select(ldf_data, c("paid_excess250", "sal_sub", "paid"))
ldf_select <- function(ldf, values) {
  # return summed type for dollar types
  dollar_types <- intersect(values, unlist(types$detail$dollar))
  if (length(dollar_types) > 0) {
    totals <- sapply(dollar_types, function(x) sum_type(ldf, type = x))
  }
  # note: Probably need to find replacement for sapply as it can be inconsistent
  
  # return columns for meta and desc types supplied in values argument
  meta_types <- intersect(values, unlist(c(types$meta, types$detail[[2]])))
  if (length(meta_types) > 0) {
    metas <- get_col(ldf, type = meta_types, drop = FALSE)
  }
  
  col_names <- setdiff(intersect(names(ldf), values), unlist(types))
  out <- ldf[, col_names, drop = FALSE]
  
  if (exists("totals")) {
    out <- data.frame(out, totals)
  }
  if (exists("metas")) {
    out <- data.frame(out, metas) 
  }
  
  # order columns as supplied in values argument
  out <- out[, match(values, names(out))]
  
  out <- carry_attr(ldf, ldf2 = out)
  
  out
}

# return columns with a certain 'type' attribute
get_col <- function(ldf, type, drop = TRUE) {
  ldf[, get_colnum(ldf, type), drop]
}

# return column names that have a certain 'type' attribute
get_colname <- function(ldf, type) {
  col_name <- lapply(type, function(x) names(ldf[, attr(ldf, "type") == x, drop = FALSE]))
  unlist(col_name)
}

# return column numbers that have a certain 'type' attribute
get_colnum <- function(ldf, type) {
  col_index <- function(x) {
    colnum <- which(attr(ldf, "type") %in% x)
    colnum
  }
  col_num <- lapply(type, col_index)
  unlist(col_num)
}

# carry appropriate attributes over to new data frame
carry_attr <- function(ldf1, ldf2) {
  type_index <- match(names(ldf2), names(ldf1))
  attr(ldf2, "type") <- attr(ldf1, "type")[type_index]
  ldf2
}



# return all claims at latest calendar date
get_latest <- function(ldf) {
  calendar <- get_calendar(ldf)
  max_cal <- max(calendar)
  latest <- ldf[calendar == max_cal, ]
  latest <- carry_attr(ldf, latest)
  return(latest)
}


#' return calandar year for each row
#' 
#'@param df a loss_df data frame
get_calendar <- function(ldf) {
  calendar <- get_col(ldf, type = "origin") + get_col(ldf, type = "dev")
}

#' merge wrapper for data frame using 'type' attribute
#'
#' @param df
#' @param by vector of column names to merge by
#' @param columns to be excluded from the merge
merge_loss_df <- function(df, calendar1, calendar2, by, exclude) {
  group1 <- df[df$calendar == calendar1, -x_cols]
  group2 <- df[df$calendar == calendar2, -exclude]
  comparison <- merge(group1, group2, by = by,
                      all.x = TRUE, all.y = TRUE, suffixes = c(paste0("_", calendar1), paste0("_", calendar2)))
  comparison[is.na(comparison)] <- 0
  comparison
}