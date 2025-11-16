#------------------------------------------------------------------------------
## Test scour
#------------------------------------------------------------------------------

test_that("scour_vert calculates correctly", {
  result <- scour_vert(Q = 4, B = 1, h = 3, h_u = 1.76, d90 = 150, d95 = 200,
                       ful_ov = TRUE)
  expect_equal(result$T0, 6.457918, tolerance = 0.01)
})

test_that("scour_horz calculates correctly", {
  result <- scour_horz(Q = 4, B = 1, h = 5, h_u = 1.76, d90 = 150, a = 1,
                       mu = 0.6)
  expect_equal(result$T0, 10.66, tolerance = 0.01)
})

test_that("scour_curve calculates correctly", {
  result <- scour_curve(h = 3.31, rm = 500, r = 530, method = "Bridge", psi = 20)
  expect_equal(result$T0, 4.179693, tolerance = 0.01)
})

test_that("scour_groyne calculates correctly", {
  v <- 2.7
  Fr <- 0.52
  h <- 3.31
  J <- 0.0022
  L <- 5
  Ks <- 0.82
  delta <- 60

  result<-scour_groyne(
    v = v, Fr = Fr, h = h, J = J, L = L,
    d16 = d16, dm = dm, d84 = d84,
    Ks = Ks, delta = delta, bedload = T
  )

  expect_equal(result$T0, 8.050801, tolerance = 0.01)
})

