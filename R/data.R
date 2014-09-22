#' Data descriptive of the workers' compensation claims data in \code{\link{losses}}
#'
#' @docType data
#' @description a data frame detailing premium, payroll, excess reinsurance coverage
#' and applicable deductibles for the \code{\link{losses}} and \code{\link{losses_loss_df}}
#' data sets
#' 
#' @name general
NULL


#' example workers' compensation losses.
#'
#' @description This claims data is in a 'raw' state; it is constructed
#' of 47 columns, most of which are not relevant to a reserve analysis.  The
#' \code{losses} data set is meant to resemble the combination of 5 typical data sets 
#' an actuary would receive from a small workers' compensation insurance provider.  The \code{losses}
#' claims data is evaluated at 2009-06-30, 2010-06-30, 2011-06-30, 2012-06-30, and
#' 2013-06-30.
#' 
#' @usage losses
#'  
#' @format A data frame with 11,104 observations on 47 variables. 
#' 
#' @name losses
NULL


#' example workers' compensation \code{\link{loss_df}}
#'
#' @description This is a cleaned version of the \code{losses} data frame.  It was created
#' using the \code{\link{loss_df}} function.
#' 
#' @usage losses_ldf
#'  
#' @format A \code{\link{loss_df}} object  with 8,054 observations on 9 variables. 
#'  
#' @name losses_ldf
NULL