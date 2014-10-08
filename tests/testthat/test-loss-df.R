context("loss_df")

test_that("type attribute assigned to each column", {
  mydf <- loss_df(occurences,  id = "claim_number",
                               origin = "origin", 
                               dev = "dev", 
                               paid = c("paid_loss_only", "paid_expense"),
                               incurred = c("incurred_loss_only", "incurred_expense"),
                               paid_recovery = c("sal_sub", "paid_excess250"),
                               incurred_recovery = c("incurred_excess250", "sal_sub_incurred"),
                               desc = "claim_cts"
                  )
  
  expect_equal(length(attr(mydf, "type")), ncol(mydf))
})