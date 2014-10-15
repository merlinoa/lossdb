#' loss_df S3 class constructor
#' 
#' \code{loss_df} is the primary S3 data class for the \code{lossdb}
#' package.  A \code{loss_df} is a data frame for insurance loss data.  Each
#' row/observation represents losses at a unique "origin", "dev", and optional
#' "id" (i.e \code{loss_df} takes loss data organized by claim/occurrence "id", 
#' "origin", and "dev" or just by "origin" and "dev").  Each column contains
#' certain variables of interest to the actuarial analysis.
#' 
#' @param ldf a data frame containing columns to be passed to \code{loss_df}
#' @param origin policy period in which the claim/loss is associated
#' @param dev development stage of claim/loss
#' @param id optional identification key for loss (e.g. a claim number)
#' @param paid vector listing names for all paid loss columns
#' @param incurred vector listing names for all incurred loss columns
#' @param paid_recovery vector listing names for all paid recovery columns
#' @param incurred_recovery vector listing names for all incurred recovery columns
#' @param desc vector of all additional columns relevant to the analysis. Common examples are 
#'   "open claims", and "reported claims" (when claims are evaluated on an occurrence basis)  
#' 
#' @details \code{loss_df} is designed to assign descriptive and consistent names 
#' to loss data to allow for quick execution of common actuarial analysis tasks.
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
loss_df <- function(ldf, origin, dev, id = NULL, paid = NULL, incurred = NULL, 
                    paid_recovery = NULL, incurred_recovery = NULL, desc = NULL) {
  
  # create list with bin for each attribute `type`
  detail <- list(dollar = list(loss = list(paid = paid, 
                                         incurred = incurred),
                             recovery = list(paid_recovery = paid_recovery,
                                             incurred_recovery = incurred_recovery)
                             ),  
               desc = desc
              )
  
  # select relevant columns from supplied data frame and create new data frame
  origin <- data.frame(origin = ldf[, origin])
  dev <- data.frame(dev = ldf[, dev])
  if (is.null(id)) {
    ldf2 <- data.frame(origin, dev, ldf[, unlist(detail)])
  } else {
    id <- data.frame(id = ldf[, id])
    ldf2 <- data.frame(id, origin, dev, ldf[, unlist(detail)])
  }
  
  # attaching type attribute to data frame
  attr(ldf2, "detail") <- detail
  
  # set `calendar`
  ldf2$calendar <- ldf2[, "origin"] + ldf2[, "dev"]
  
  # define data.frame class as "loss.df"
  class(ldf2) <- c("loss_df", "data.frame")

  # run automated check
  check_loss_df(ldf2)
  
  # return 'loss_df' object
  ldf2
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
#' @param by type attribute to summarize by.  Allowable values are `origin`, `dev`,
#' and `id`
#' @param values optional - select specific values to summarize
#' @param calendar optional calendar period (i.e. calendar = origin + dev)
#' 
#' @method summary loss_df
#' 
#' @description summary.loss_df can provides snapshot for any columns
#' or `type` attributes in a `loss_df` object.
#' 
#' @export
#' @examples
#' 
#' # without specificied `calendar`
#' summary(ldf_data)
#' 
#' # with specified `calendar`
#' summary(ldf_data, calendar = 2012)
#' 
#' # with specified `detail`
#' summary(ldf_data, detail = c("paid_excess250", "sal_sub", "paid"))
summary.loss_df <- function(ldf, detail = NULL, calendar = NULL) {
  ## make sure only no meta values being passed to `detail` arguement
  if (length(intersect(detail, meta)) > 0) stop("meta values not be comparable")
  
  # filter rows of calendar period to summarize
  if (is.null(calendar)) {
    selected_rows <- ldf[ldf$calendar == max(ldf$calendar), ]     
  } else {
    selected_rows <- ldf[ldf$calendar == calendar, ]
  }
  
  # select columns to summarize
  if (is.null(detail)) {
    selected <- selected_rows[, setdiff(names(selected_rows), meta)]
  } else {
    selected <- select_ldf(selected_rows, values = detail)
  }
  
  # sum columns based on origin
  smry <- apply(selected, 2,
                function(x) tapply(x, selected_rows$origin, sum, na.rm = TRUE))
  
  # move origin column rowname into column in actual data frame
  origin <- data.frame(origin = rownames(smry))
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
#' # plot paid and case at most recent `calendar`
#' plot(ldf_data)
#' 
#' # plot paid and case at selected `calendar`
#' plot(ldf_data, calendar = 2012)
plot.loss_df <- function(ldf, calendar = NULL) {
  # format data frame
  if (is.null(calendar)) {
    smry <- summary(ldf, detail = unlist(detail$dollar))
  } else {
    smry <- summary(ldf, detail = unlist(detail$dollar),
                    calendar = calendar)
  } 

  smry$case <- smry$incurred - smry$paid
  
  smry$paid_recovery <- -smry$paid_recovery
  smry$incurred_recovery <- -smry$incurred_recovery
  smry$case_recovery <- smry$incurred_recovery - smry$paid_recovery

  totals <- melt(smry[, c("origin", "paid", "case", 
                          "paid_recovery", "case_recovery")], id.vars = "origin")
  
  # separate data frames for positive or negative value
  # this is necessary to create stacked ggplot bar chart with 
  # positive and neg values in same stack
  pos <- subset(totals, value > 0)
  neg <- subset(totals, value < 0)
  
  # create plot
  p <- ggplot() +
    geom_bar(data = pos, aes(x = origin, y = value, fill = variable),
             stat = "identity") +
    geom_bar(data = neg, aes(x = origin, y = value, fill = variable),
             stat = "identity") +
    xlab("Origin Year") + ylab("Loss Amounts") + 
    ggtitle("Loss Amounts by Origin Year")
  suppressWarnings(print(p))
}


#' check that arguments and data columns to loss_df are appropriate
#' 
#' @param ldf a loss_df to check
check_loss_df <- function(ldf) {
  # check that reserved names are not used
  if (length(intersect(unlist(detail$dollar), names(ldf))) > 0) {
    stop("paid, incurred, paid_recovery, and incurred_recovery are reserved names.  Change column names.")
  }
  
  # check that required cols (origin and dev) each exist
  if (length(intersect(c("origin", "dev"), names(ldf))) != 2) {
    stop("'origin' and 'dev' are required arguments")
  }
  
  # check that 'id' is only 1 column
  if (!is.null(ldf$id)) {
    if (length(ldf[, "id", drop = FALSE]) != 1) {
      stop("'id' can only reference 1 column")
    }
    # check that columns are of correct type
    factor_cols <- "id"
  }
  
  # check that 'origin', 'dev', and 'id' are each only 1 column
  if (length(ldf[, c("origin", "dev")]) != 2) {
    stop("'origin' and 'dev' can only reference 1 column each")
  }
  
  # check to see that at least 1 column of loss data was supplied
  if (length(unlist(attr(ldf, "detail"))) < 1) {
    stop("At least 1 column of loss data must be provided")
  }
  
  numeric_cols <- setdiff(names(ldf), "id")
  if (!all(unlist(lapply(ldf[, factor_cols], is.factor)))) {
    stop("set 'id' to type factor")
  }
  if (!all(unlist(lapply(ldf[, numeric_cols], is.numeric)))) {
    stop("All columns other than 'id' must be of type numeric")  
  }
}