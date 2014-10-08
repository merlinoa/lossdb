context("utils")

test_that("get_colname returns appropriate column names", {
  expect_equal(get_colname(ldf_data, type = "id"), "claim_number")
  expect_equal(get_colname(ldf_data, type = "origin"), "origin")
  expect_equal(get_colname(ldf_data, type = "dev"), "dev")
  expect_equal(get_colname(ldf_data, type = "calendar"), "calendar")
  expect_equal(get_colname(ldf_data, type = "paid"), c("paid_loss_only", "paid_expense"))
  expect_equal(get_colname(ldf_data, type = "incurred"), c("incurred_loss_only", "incurred_expense"))
  expect_equal(get_colname(ldf_data, type = "paid_recovery"), c("sal_sub", "paid_excess250"))
  expect_equal(get_colname(ldf_data, type = "incurred_recovery"), c("sal_sub_incr", "incurred_excess250"))
  expect_equal(get_colname(ldf_data, type = "desc"), "claim_cts")
})

