## ----, echo = FALSE, message = FALSE-------------------------------------
library(lossdb)
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  tidy = FALSE)

## ----, eval = FALSE------------------------------------------------------
#  str(losses)

## ----, message = FALSE---------------------------------------------------
library(dplyr)
# create origin and dev column
losses <- mutate(losses, origin = as.numeric(substr(fiscal_year_desc, 1, 4)), 
                 evaluation_year = as.numeric(format(as.Date(evaluation_date, "%Y-%m-%d"), "%Y")),
                 dev = evaluation_year - origin) 

# group claims by occurence
# often necessary when excess reinsurance is applied on an occurence basis
# rather than on a claims basis.
occurences <- losses %>%
  group_by(claim_number, dev, origin) %>%
     summarise(claim_cts = n(),
               payment_amount = sum(payment_amount), # paid loss & ALAE
               incurred_amount = sum(reserve_amount), # incurred loss & ALAE
               paid_expense = sum(X4_exp_payment), # paid ALAE
               incurred_expense = sum(X4_exp_reserve), #incurred ALAE
               sal_sub_paid = sum(payment_no_reserve_a), # salvage and subrogation
               sal_sub_incurred = sum(payment_no_reserve_a)
              )

# create relevant loss values columns
occurences <- mutate(occurences,
                    paid_loss = payment_amount - paid_expense,
                    incurred_loss = incurred_amount - incurred_expense,
                    paid_excess250 = max(payment_amount - sal_sub_paid - 250000, 0),
                    incurred_excess250 = max(incurred_amount - sal_sub_incurred - 250000, 0))

# need to get rid of grouped df class. causing problems with subsetting.
occurences <- as.data.frame(occurences)

## ------------------------------------------------------------------------
# create loss_df object
mydf <- loss_df(occurences, id = "claim_number",
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

## ----, results = 'asis'--------------------------------------------------
kable(summary(mydf))

## ----, results = 'asis'--------------------------------------------------
kable(summary(mydf, calendar = "2012"))

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
mychanges[, mychanges$claim_cts_change < 0]

## ------------------------------------------------------------------------
# check for claims in which paid_loss decreased
mychanges[, mychanges$paid_loss_changes < 0]

## ------------------------------------------------------------------------
# project total paid losses gross of any recovery
value2project <- data.frame(origin = mydf$origin, dev = mydf$dev, paid_total = paid(mydf))
head(value2project)

## ------------------------------------------------------------------------
suppressMessages(library(ChainLadder))
paid_tri <- as.triangle(value2project, origin = "origin", dev = "dev", value = "paid_total")

## ------------------------------------------------------------------------
MackChainLadder(paid_tri)

