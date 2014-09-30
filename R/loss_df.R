#' loss_df S3 class constructor
#' 
#' \code{loss_df} is the primary S3 data class for the \code{lossdb}
#' package.  A \code{loss_df} is a data frame for insurance loss data.  The data frame 
#' is structured on a claim or occurence basis (i.e. each row represents 1 claim 
#' or 1 claim occurence) evaluated at a specific time, and each column contains
#' certain variables attributable to the claim at that evaluation time.
#' 
#' @param df a data frame containing the raw data of losses by claim
#' @param origin time period in which the claim originated
#' @param dev development stage of claim at relevant 'evaluation_date'
#' @param id optional identification key of claim
#' @param evaluation_date optional column naming the evaluation dates
#' @param paid vector listing names or numbers for all paid loss columns
#' @param incurred vector listing names or numbers for all incurred loss columns
#' @param paid_recovery vector listing names of numbers for all paid recovery columns
#' @param incurred_recovery vector listing names or numbers for all incurred recovery columns
#' @param desc vector of all additional columns relevant to the analysis. Common examples are 
#'   'open claims', and 'reported claims' (when claims are evaluated on an occurence basis)  
#' 
#' @details \code{loss_df} is designed to assign descriptive and consistent names 
#' to loss data to allow for easier data analysis. The relevant
#' columns can be assigned to the above arguments using a character vector of the column names
#' or a numeric vector of the column position.
#' 
#' @export
#' @examples
#' # create loss_df object from `occurences` dataset
#' mydf <- loss_df(occurences,  id = "claim_number",
#'                              origin = "origin", 
#'                              dev = "dev", 
#'                              paid = c("paid_loss_only", "paid_expense"),
#'                              incurred = c("incurred_loss_only", "incurred_expense"),
#'                              paid_recovery = c("sal_sub", "paid_excess250"),
#'                              incurred_recovery = c("incurred_excess250", "sal_sub_incurred"),
#'                              desc = "claim_cts"
#'                  )
loss_df <- function(ldf, origin, dev, id = NULL, evaluation_date = NULL, paid = NULL, 
                    incurred = NULL, paid_recovery = NULL, incurred_recovery = NULL,
                    desc = NULL) {
  
  # create list with bin for each attribute `type`
  values <- list(id = id,
                 origin = origin,
                 dev = dev,
                 evaluation_date = evaluation_date,
                 paid = paid,
                 incurred = incurred,
                 paid_recovery = paid_recovery,
                 incurred_recovery = incurred_recovery,
                 desc = desc
                 )
  
  # select relevant columns from supplied data frame and create new data frame
  df2 <- ldf[, unlist(values)]
  
  # creating vector for 'type' attribute
  n <- unlist(lapply(values, length))
  atr <- list()
  for (i in 1:length(values)) {
    atr[[i]] <- rep(names(values)[i], n[i])
  }
  
  # attaching type attribute to data frame
  attr(df2, "type") <- unlist(atr)
  
  # if no 'evaluation_date' specified set 'evaluation_date' equal
  # to calendar
  if (is.null(evaluation_date)) {
    df2$evaluation_date <- as.factor(get_calendar(df = df2))
    attr(df2, "type")[length(attr(df2, "type")) + 1] <- "evaluation_date"
  }
  
  # define data.frame class as "loss.df"
  class(df2) <- c("loss_df", "data.frame")
  
  # run automated check
  check_loss_df(df2)
  
  # return 'loss_df' object
  df2
}


#' Is an object a loss_df object
#' 
#' @export
#' @param x and object to test
#' @keywords internal
is.loss_df <- function(x) inherits(x, "loss_df")


#' returns the grouped sum of loss_df columns by origin period
#' 
#' @param ldf loss_df S3 object
#' @param values optional - select specific values to summarize
#' @param evaluation_date optional evaluation date
#' 
#' @method summary loss_df
#' 
#' @description summary.loss_df can provides snapshot for any columns
#' or `type` attributes in a `loss_df` object.
#' 
#' @export
#' @examples
#' 
#' # without specificied `evaluation_date`
#' summary(recovery_ldf)
#' 
#' # with specified `evaluation_date`
#' summary(recovery_ldf, evaluation_date = "2012-06-30")
#' 
#' # with specified `values`
#' summary(recovery_ldf, values = c("paid_excess250", "sal_sub", "paid"))
summary.loss_df <- function(ldf, values = NULL, evaluation_date = NULL) {
  if (is.null(evaluation_date)){
    
    # select columns to summarize at latest evaluation date
    latest <- get_latest(df = ldf) 
    exclude <- get_colnum(df = latest, type = c("id", "dev", "evaluation_date", "origin"))
    latest_values <- latest[, -exclude]
    latest_values <- carry_attr(df1 = latest, df2 = latest_values)
    
    # sum columns by origin
    smry <- apply(latest_values, 2,
                  function(x) tapply(x, get_col(df = latest, type = "origin"), sum, na.rm = TRUE))
  } else {
    
    # select columns to summarize at supplied evaluation date
    selected <- ldf[get_col(df = ldf, type = "evaluation_date") == evaluation_date, 
                    -get_colnum(df = ldf, type = c("id", "dev", "evaluation_date"))]
    selected <- carry_attr(df1 = ldf, df2 = selected)
    # sum columns by origin
    smry <- apply(selected[, -which(names(selected) %in% get_colname(df = selected, type = "origin"))], 2,
                  function(x) tapply(x, get_col(df = selected, type = "origin"), sum, na.rm= TRUE))
  }
  
  # move 'origin' column from rowname into actual data frame
  origin <- data.frame(rownames(smry))
  names(origin) <- get_colname(df = ldf, type = "origin")
  smry <- cbind(origin, smry)
  rownames(smry) <- NULL
  smry <- carry_attr(df1 = ldf, df2 = smry)
  
  
  if (is.null(values)) {
    return(smry)
  } else {   
    
    # select `origin` column
    smry2 <- get_col(df = smry, type = "origin", drop = FALSE)
    
    # summarize `type` by adding columns of same type together
    for (i in intersect(values, types$all)) {
      smry2[, i] <- sum_type(df = smry, type = i)
    }
    
    # return specific columns based on supplied column name
    for (j in intersect(values, names(smry))) {
      smry2[, j] <- smry[, j, drop = FALSE]
    }
    
    # reorder columns so they are in same order as `values` argument
    index <- match(values, names(smry2[, 2:length(smry2), drop = FALSE]))
    index <- index + 1
    smry2 <- data.frame(smry2[, 1, drop = FALSE], smry2[index])
    smry2 <- carry_attr(df1 = smry, df2 = smry2)
    return(smry2)
  } 
}


#' plot method for \code{loss_df}
#' 
#' barchart of loss amounts by origin year
#' 
#' @param df loss_df S3 object
#' 
#' @keywords internal
#' @method plot loss_df
#' 
#' @export
#' 
#' @examples
#' # plot paid and case at most recent `evaluation_date`
#' plot(losses_loss_df)
#' 
#' # plot paid and case at selected `evaluation_date`
#' plot(losses_loss_df, evaluation_date = "2012-06-30")
#' ## need to fix so other evaluation dates besides latest work
plot.loss_df <- function(ldf, evaluation_date = NULL) {
  # format data frame
  if (is.null(evaluation_date)) {
    smry <- as.data.frame(summary(ldf))
  } else {
    smry <- as.data.frame(summary(ldf, evaluation_date = evaluation_date))
  } 
  smry <- carry_attr(df1 = ldf, df2 = smry)
  pdata <- data.frame(get_col(df = smry, type = "origin"))
  smry$incurred <- sum_type(df = smry, type = "incurred")
  smry$incurred_recovery <- -sum_type(df = smry, type = "incurred_recovery")
  pdata$paid_recovery <- -sum_type(df = smry, type = "paid_recovery")
  pdata$case_recovery <- smry$incurred_recovery - pdata$paid_recovery
  pdata$paid <- sum_type(df = smry, type = "paid")
  pdata$case <- smry$incurred - pdata$paid
  
  totals <- melt(pdata, id.vars = 1)
  
  # separate data frames for positive or negative value
  # this is necessary to create stacked ggplot bar chart with 
  # positive and neg values in same stack
  pos <- subset(totals, value > 0)
  neg <- subset(totals, value < 0)
  
  attr(pos, "type")[1] <- "origin"
  attr(neg, "type")[1] <- "origin"
  
  # create plot
  p <- ggplot() +
    geom_bar(data = pos, aes_string(x = get_colname(df = pos, type = "origin"), y = "value", fill = "variable"),
             stat = "identity") +
    geom_bar(data = neg, aes_string(x = get_colname(df = neg, type = "origin"), y = "value", fill = "variable"),
             stat = "identity") +
    xlab("Origin Year") + ylab("Loss Amounts") + 
    ggtitle("Loss Amounts by Origin Year")
  suppressWarnings(print(p))
}


#' check that arguments and data columns to loss_df are appropriate
#' 
#' @param df a loss_df to check
check_loss_df <- function(ldf) {
  # check that required cols (id, origin and dev) each exist
  if (!identical(intersect(c("origin", "dev"), attr(ldf, "type")),
                c("origin", "dev"))) {
    stop("'origin' and 'dev' are required arguments")
  }
  # check that 'origin' and 'dev' are each only 1 column
  if (length(get_colname(df = ldf, c("origin", "dev"))) != 2) {
    stop("'origin' and 'dev' can only reference 1 column each")
  }
  # check to see that at least 1 column of loss data was supplied
  if (length(get_colname(df = ldf, c("paid", "incurred", "paid_recovery",
                                      "incurred_recovery", "desc"))) < 1) {
    stop("At least 1 column of loss data must be provided")
  }
  
  # chack that columns are of correct type
  factor_cols <- get_colname(df = ldf, c("id", "evaluation_date"))
  numeric_cols <- get_colname(df = ldf, c("origin", "dev", "paid", "incurred",
                                          "paid_recovery", "incurred_recovery", "desc"))
  if (!all(unlist(lapply(ldf[, factor_cols], is.factor)))) {
    stop("set 'id' and or 'evaluation_date' of type factor")
  }
  if (!all(unlist(lapply(ldf[, numeric_cols], is.numeric)))) {
    stop("All columns other than 'id' and 'evaluation_date' must be of type numeric")  
  }
}