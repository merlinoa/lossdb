## ----, echo = FALSE, message = FALSE-------------------------------------
library(lossdb)
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  tidy = FALSE)

## ------------------------------------------------------------------------
str(occurrences)

## ------------------------------------------------------------------------
# create loss_df object
mydf <- loss_df(occurrences, 
          id = "claim_number",
          origin = "origin",
          dev = "dev", 
          paid = c("paid_loss_only", "paid_expense"),
          incurred = c("incurred_loss_only", "incurred_expense"),
          paid_recovery = c("paid_excess250", "sal_sub"),
          incurred_recovery = c("incurred_excess250", 
                                "sal_sub_incurred"),
          desc = "claim_cts"
        )
kable(head(mydf[, 1:6]))

## ----summary1------------------------------------------------------------
kable(summary(mydf)[, 1:9])

## ----summary2------------------------------------------------------------
kable(summary(mydf, calendar = "2012")[, 1:9])

## ------------------------------------------------------------------------
plot(mydf)

## ------------------------------------------------------------------------
plot(mydf, calendar = "2012")

## ------------------------------------------------------------------------
# specify the loss amount values you want to see the changed claims for 
mychanges <- claim_changes(mydf, 
               calendar1 = "2013", 
               calendar2 = "2012",
               values = c("paid_loss_only", "claim_cts")
             )
kable(head(mychanges))

## ------------------------------------------------------------------------
# check for missing claims
kable(mychanges[mychanges$claim_cts_change < 0, ])

## ------------------------------------------------------------------------
# check for claims in which paid_loss decreased
kable(mychanges[mychanges$paid_loss_only_change < 0, ])

## ----projection_values---------------------------------------------------
# project total paid losses gross of any recovery
value2project <- data.frame(origin = mydf$origin, 
                   dev = mydf$dev, 
                   paid_total = paid(mydf)
                 )
kable(head(value2project))

## ----triangle, message = FALSE-------------------------------------------
library(ChainLadder)
paid_tri <- as.triangle(value2project, 
              origin = "origin", 
              dev = "dev", 
              value = "paid_total"
            )

## ----mack, warning = FALSE-----------------------------------------------
MackChainLadder(paid_tri)

## ----boot, warning = FALSE-----------------------------------------------
BootChainLadder(paid_tri)

