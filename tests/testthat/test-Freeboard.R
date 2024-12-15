#------------------------------------------------------------------------------
## Test freeboard
#------------------------------------------------------------------------------


setUp <- function() {
  # Create profiles
  x <- c(-0.85, 3, 15, 18.85)
  z <- c(3.85, 0, 0, 3.85)

  # Initialize the CSarbitrary object
  csAr_KOHS_Schaechen <<- CSarbitrary(
    x = x, z = z, xb_l = 3, xb_r = 15,
    kSt_B = 45
  )
}

setUp()

#------------------------------------------------------------------------------
## Test Hydraulics
#------------------------------------------------------------------------------
test_that("uniform_flow_Qmax_freeboardcalculations are accurate", {
  result_uniform_flow_Qmax_freeboard <- uniform_flow_Qmax_freeboard(
    csAr_KOHS_Schaechen,
    J = 2.2e-2,
    type = "KOHS",
    sigma_wz = 0,
    fv = TRUE,
    ft = 0.5,
    fe = NULL,
    method = "Strickler"
  )
  expect_equal(result_uniform_flow_Qmax_freeboard$fe, 2.57, tolerance = 0.01,
               label = "uniform_flow_Qmax_freeboard 'fe' value is within tolerance")
  expect_equal(result_uniform_flow_Qmax_freeboard$v, 7.1, tolerance = 0.01,
               label = "uniform_flow_Qmax_freeboard 'v' value is within tolerance")
  expect_equal(result_uniform_flow_Qmax_freeboard$Qmax, 120, tolerance = 0.01,
               label = "uniform_flow_Qmax_freeboard 'Qmax' value is within tolerance")
  expect_equal(result_uniform_flow_Qmax_freeboard$hmax, 1.27, tolerance = 0.01,
               label = "uniform_flow_Qmax_freeboard 'hmax' value is within tolerance")


})

# Test for freeboard with ft=0
test_that("freeboard (ft=0) calculations are accurate", {
  expect_equal(
    freeboard(v = 7.1, h = 1.3, sigma_wz = 0, fv = TRUE, ft = 0),
    2.57, tolerance = 0.005,
    label = "freeboard (v=7.1, h=1.3, ft=0) is accurate"
  )
})

# Test for freeboard with ft=0.5
test_that("freeboard (ft=0.5) calculations are accurate", {
  expect_equal(
    freeboard(v = 7.1, h = 1.3, sigma_wz = 0, fv = TRUE, ft = 0.5),
    2.62, tolerance = 0.001,
    label = "freeboard (v=7.1, h=1.3, ft=0.5) is accurate"
  )
})

# Test for freeboard with sigma_wz=0.3
test_that("freeboard with sigma_wz=0.3 calculations are accurate", {
  expect_equal(
    freeboard(v = 4.56, h = 1.36, sigma_wz = 0.3, fv = TRUE, ft = 0),
    1.11, tolerance = 0.001,
    label = "freeboard (v=4.56, h=1.36, sigma_wz=0.3, ft=0) is accurate"
  )
})
