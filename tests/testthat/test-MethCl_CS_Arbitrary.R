#------------------------------------------------------------------------------
## Test CSarbitrary Methods
#------------------------------------------------------------------------------

# Setup function
setUp <- function() {
  # Create profiles
  x <- c(0, 4, 9, 13)
  z <- c(2, 0, 0, 2)
  csAr_Bollrich3_6_1_6 <<- CSarbitrary(x = x, z = z, xb_l = 4, xb_r = 9,
                                       kSt_B = 35, kSt_l = 45, kSt_r = 45)

  x <- seq(-sqrt(20), sqrt(20), length.out = 41)
  z <- 0.1 * x^2 - 2
  csAr_Bollrich3_6_1_7 <<- CSarbitrary(x = x, z = z, kSt_B = 40)
}

# Call setup function
setUp()

#------------------------------------------------------------------------------
## Test access and geometry
#------------------------------------------------------------------------------

## Test access
test_that("Test Arbitrary Access", {
  expect_equal(kSt_B(csAr_Bollrich3_6_1_7), 40)
})

## Test replacements that are not allowed
test_that("Test Arbitrary Replacement", {
  expect_error(x(csAr_Bollrich3_6_1_7) <- 1:3)
})

## Test unique x values are not allowed
test_that("Test unique x values", {
  x <- c(0, 0, 2, 4)
  z <- c(5, 0, 0, 5)
  expect_error(CSarbitrary(x = x, z = z, kSt_B = 40))
})

#------------------------------------------------------------------------------
## Hydraulics
#------------------------------------------------------------------------------

test_that("Test Arbitrary Bollrich3_6_1_7", {
  expect_equal(wetted_area(csAr_Bollrich3_6_1_7, h = 2), 11.92, tolerance = 0.001)
  expect_equal(wetted_perimeter(csAr_Bollrich3_6_1_7, h = 2), 10.02,
               tolerance = 0.001)
  expect_warning(wetted_area(csAr_Bollrich3_6_1_7, h = 4.5))
  expect_warning(wetted_perimeter(csAr_Bollrich3_6_1_7, h = 4.5))

})
