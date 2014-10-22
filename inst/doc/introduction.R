## ----, echo = FALSE, message = FALSE-------------------------------------
library(lossdb)
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  tidy = FALSE)

## ----, eval = FALSE------------------------------------------------------
#  str(losses)

## ----, message = FALSE---------------------------------------------------
library(dplyr) # assumes dplyr has already been installed using install.packages("dplyr")
# create origin and dev columns
losses <- mutate(losses, origin = as.numeric(substr(fiscal_year_desc, 1, 4)), 
                 evaluation_year = as.numeric(format(as.Date(evaluation_date, "%Y-%m-%d"), "%Y")),
                 dev = evaluation_year - origin) 

# group claims by occurrence
# often necessary when excess reinsurance is applied on an occurrence basis
# rather than on a claims basis.
occurrences <- losses %>%
  group_by(claim_number, dev, origin) %>%
     summarise(claim_cts = n(),
               payment_amount = sum(payment_amount), # paid loss & ALAE
               incurred_amount = sum(reserve_amount), # incurred loss & ALAE
               paid_expense = sum(X4_exp_payment), # paid ALAE
               incurred_expense = sum(X4_exp_reserve), #incurred ALAE
               sal_sub_paid = sum(payment_no_reserve_a), # paid salvage and subrogation
               sal_sub_incurred = sum(payment_no_reserve_a) # incurred salvage and subrogation
              )

# create relevent "dollar" columns
occurrences <- mutate(occurrences,
                    paid_loss = payment_amount - paid_expense,
                    incurred_loss = incurred_amount - incurred_expense,
                    paid_excess250 = max(payment_amount - sal_sub_paid - 250000, 0),
                    incurred_excess250 = max(incurred_amount - sal_sub_incurred - 250000, 0))

# need to get rid of grouped df class. causing problems with subsetting.
occurrences <- as.data.frame(occurrences)

## ------------------------------------------------------------------------
# create loss_df object
mydf <- loss_df(occurrences, id = "claim_number",
                             origin = "origin",
                             dev = "dev", 
                             paid = c("paid_loss", "paid_expense"),
                             incurred = c("incurred_loss", "incurred_expense"),
                             paid_recovery = c("paid_excess250", "sal_sub_paid"),
                             incurred_recovery = c("incurred_excess250", 
                                                   "sal_sub_incurred"),
                             desc = "claim_cts"
                 )
head(mydf[, 1:6])

## ----summary1------------------------------------------------------------
summary(mydf)

## ----summary2------------------------------------------------------------
summary(mydf, calendar = "2012")

## ------------------------------------------------------------------------
plot(mydf)

## ------------------------------------------------------------------------
plot(mydf, calendar = "2012")

## ------------------------------------------------------------------------
# specify the loss amount values you want to see the changed claims for 
mychanges <- claim_changes(mydf, calendar1 = "2013", calendar2 = "2012",
                            values = c("paid_loss", "claim_cts"))
head(mychanges)

## ------------------------------------------------------------------------
# check for missing claims
mychanges[mychanges$claim_cts_change < 0, ]

## ------------------------------------------------------------------------
# check for claims in which paid_loss decreased
mychanges[mychanges$paid_loss_change < 0, ]

## ----projection_values---------------------------------------------------
# project total paid losses gross of any recovery
value2project <- data.frame(origin = mydf$origin, dev = mydf$dev, paid_total = paid(mydf))
head(value2project)

## ----triangle,  message = FALSE------------------------------------------
library(ChainLadder)
paid_tri <- as.triangle(value2project, origin = "origin", dev = "dev", value = "paid_total")

## ----mack, warning = FALSE-----------------------------------------------
MackChainLadder(paid_tri)

## ----boot, warning = FALSE-----------------------------------------------
BootChainLadder(paid_tri)

