#' loss_df S3 class constructor
#' 
#' \code{loss_df} is the primary S3 data class for the \code{reserve}
#' package.  A \code{loss_df} is a data frame for claims loss 
#' information of a non life insurance line of business.  The data frame 
#' is structured on a claim or occurence basis (i.e. each row represents 1 claim 
#' or 1 claim occurence) evaluated at a specific time.
#' 
#' @param df a data frame containing the raw data of losses by claim
#' @param id identification key of claim
#' @param origin time period in which the claim originated
#' @param dev development stage of claim at relevant 'evaluation_date'
#' @param paid vector of all paid loss columns
#' @param incurred vector of all incurred loss columns
#' @param paid_recovery vector of all paid recovery columns
#' @param incurred_recovery vector of all incurred recovery columns
#' @param desc vector of all additional columns relevant to the analysis. Common examples are 
#'   'open claims', and 'reported claims' (when claims are evaluated on an occurence basis)  
#' 
#' @details \code{loss_df} is designed to assign descriptive and consistent names 
#' to loss data so analysis with the \code{reserve} package can be possible. The relevant
#' columns can be assigned to the above arguments using a character vector of the column names
#' or a numeric vector of the column position.
#' 
#' @export
#' @examples
#' # use the 'dplyr' package to get raw data in proper format for 'loss_df'
#' library(dplyr)
#' losses <- mutate(losses, origin = as.numeric(substr(fiscal_year_desc, 1, 4)),
#'                  evaluation_year = as.numeric(format(as.Date(evaluation_date, "%Y-%m-%d"), "%Y")),
#'                  dev = evaluation_year - origin)
#' 
#' occurences <- losses %>%
#' group_by(claim_number, dev, evaluation_date) %>%
#'   summarise(claim_cts = n(),
#'             payment_amount = sum(payment_amount),
#'             reserve_amount = sum(reserve_amount),
#'             origin = mean(origin),
#'             paid_expense = sum(X4_exp_payment),
#'             incurred_expense = sum(X4_exp_reserve),
#'             sal_sub = sum(payment_no_reserve_a)
#'            )
#' occurences <- mutate(occurences,
#'                      paid_loss_only = payment_amount - paid_expense,
#'                      incurred_loss_only = reserve_amount - incurred_expense,
#'                      paid_excess250 = max(payment_amount - 250000, 0),
#'                      incurred_excess250 = max(reserve_amount - 250000, 0))
#' 
#' # create loss_df object
#' mydf <- loss_df(occurences, id = "claim_number",
#'                              origin = "origin", 
#'                              dev = "dev",
#'                              evaluation_date = "evaluation_date", 
#'                              paid = c("paid_loss_only", "paid_expense"),
#'                              incurred = c("incurred_loss_only", "incurred_expense"),
#'                              paid_recovery = c("sal_sub", "paid_excess250"),
#'                              incurred_recovery = "incurred_excess250",
#'                              desc = "claim_cts"
#'                  )
#'                  
#' # use numbers instead of character vector to identify columns
#' mydf2 <- loss_df(losses2, id = 1, origin = 7, dev = 2, evaluation_date = 3,
#'                  paid = c(8, 10), incurred = c(9, 11), desc = 4)
loss_df <- function(ldf, id, origin, dev, evaluation_date = NULL, paid = NULL, 
                    incurred = NULL, paid_recovery = NULL, incurred_recovery = NULL,
                    desc = NULL) {
  # create list with bin for each attribute 'type'
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
  values <- lapply(values, num_to_name, df = ldf)
  #change relevant columns to new names
  relevant <- match(unlist(values), colnames(ldf))
  relevant <- relevant[!is.na(relevant)]
  df2 <- ldf[, relevant]
  # applying attributes
  n <- unlist(lapply(values, length))
  atr <- list()
  for (i in 1:length(values)) {
    atr[[i]] <- rep(names(values)[i], n[i])
  }
  # set type attribute for data frame
  attr(df2, "type") <- unlist(atr)
  # define data.frame class as "loss.df"
  class(df2) <- c("loss_df", "data.frame")
  check_loss_df(df2)
  df2
}

#' Is an object a loss_df object
#' 
#' @export
#' @param x and object to test
#' @keywords internal
is.loss_df <- function(x) inherits(x, "loss_df")

#' get the sum of loss_df data by origin period
#' 
#' @param ldf loss_df S3 object
#' 
#' @keywords internal
#' @method summary loss_df
#' @method evaluation_date
#' 
#' @export
#' @examples
#' # without specificied `evaluation_date`
#' summary(losses_loss_df)
#' 
#' # with specified `evaluation_date`
#' summary(losses_loss_df, evaluation_date = "2012-06-30")
summary.loss_df <- function(ldf, evaluation_date = NULL) {
  ldf$calendar <- get_col(df = ldf, type = "origin") +
                 get_col(df = ldf, type = "dev")
  cols <- get_colnum(df = ldf, type = c("id", "dev"))
  df2 <- ldf[, -cols]
  df2 <- carry_attr(df1 = ldf, df2 = df2)
  attr(df2, "type")[length(attr(df2, "type"))] <- "calendar"
  if (is.null(evaluation_date)){
    latest <- df2[df2$calendar == max(df2$calendar), -get_colnum(df = df2, type = "evaluation_date")]
    latest <- carry_attr(df1 = df2, df2 = latest)
    latest_no_origin <- latest[, -get_colnum(df = latest, type = "origin")]
    smry <- apply(latest_no_origin, 2,
                  function(x) tapply(x, get_col(df = latest, type = "origin"), sum, na.rm = TRUE))
  } else {
    selected <- df2[get_col(df = df2, type = "evaluation_date") == evaluation_date, 
                    -get_colnum(df = df2, type = "evaluation_date")]
    selected <- carry_attr(df1 = df2, df2 = selected)
    selected_no_origin <- selected[, -get_colnum(df = selected, type = "origin")]
    smry <- apply(selected[, -which(names(selected) %in% get_colname(df = selected, type = "origin"))], 2,
                  function(x) tapply(x, get_col(df = selected, type = "origin"), sum, na.rm= TRUE))
  }
  smry <- as.data.frame(smry[, -which(colnames(smry) == "calendar")])
  
  attr(smry, "eval") <- evaluation_date
  origin <- data.frame(rownames(smry))
  names(origin) <- get_colname(df = ldf, type = "origin")
  smry <- cbind(origin, smry)
  rownames(smry) <- NULL
  smry
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
  ## need to add more checks
  # check that id, origin and dev are each 1 column
  if (length(get_colname(df = ldf, c("id", "origin", "dev"))) != 3) {
    stop("id, origin, and dev can only reference 1 column each")
  }
  # check to see that at least some loss data supplied
  if (length(get_colname(df = ldf, c("paid", "incurred", "paid_recovery",
                                      "incurred_recovery", "desc"))) == 0) {
    stop("Some loss data must be provided")
  }
  factor_cols <- get_colname(df = ldf, c("id", "evaluation_date"))
  numeric_cols <- get_colname(df = ldf, c("paid", "incurred", "paid_recovery",
                                           "incurred_recovery", "desc"))
  if (!all(unlist(lapply(ldf[, factor_cols], is.factor)))) stop("set 'id' and 'evaluation_date' to factor")
  if (!all(unlist(lapply(ldf[, numeric_cols], is.numeric)))) stop("All columns other than 'id' and 'evaluation_date' must be numeric")  
}