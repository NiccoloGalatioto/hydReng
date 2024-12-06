

#------------------------------------------------------------------------------
## test Bedload transport according to Smart and Jaeggi
#------------------------------------------------------------------------------

test_that("Gb_SJ calculates correctly", {
  B <- 27
  dm <- 0.11
  d90 <- 0.23
  d30 <- 3200000 * d90 / 4084101
  J <- 0.009
  um <- 2.6
  Rs <- 0.8

  # Example from "Abschaetzung der mittleren jährlichen Geschiebelieferung,in Vorfluter", S. 57
  result <- Gb_SJ(d30 = d30, dm = dm, d90 = d90, J = J, Rs = Rs, um = um, B = B,
                  tcrit = 0.023)
  expect_equal(result, 84.8576, tolerance = 0.01)
})

