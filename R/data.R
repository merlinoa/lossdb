#' The \code{\link{losses}} data frame transformed for use in the \code{\link{loss_df}} function
#'
#' @description Columns have been transformed from dates to origin years and
#' development years.  Also losses have been split up as desired for the examples.
#' 
#' @docType data
#' 
#' @name occurrences
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


#' example workers' compensation loss_df
#'
#' @description This is an example \code{\link{loss_df}} S3 class object created from
#' the \code{losses} data frame.  It was created using the \code{\link{loss_df}} function.
#' 
#' @usage ldf_data
#'  
#' @format A \code{\link{loss_df}} object 
#'  
#' @name ldf_data
NULL


#' Minimal loss_df
#'
#' @description A small \code{\link{loss_df}} used to test and explain
#' functions.
#' 
#' @usage test_df
#'  
#' @format A \code{\link{loss_df}} object 
#'  
#' @name test_df
NULL

#' Minimal loss_df with missing historic data
#'
#' @description A small \code{\link{loss_df}} with missing historic data
#' used to test and explain functions.
#' 
#' @usage test_df_incomplete
#'  
#' @format A \code{\link{loss_df}} object 
#'  
#' @name test_df_incomplete
NULL