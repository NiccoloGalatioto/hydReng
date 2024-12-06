

library(roxygen2) # In-Line Documentation for R
library(devtools) # Tools to Make Developing R Packages Easier
library(testthat) # Unit Testing for R
library(rhub)
library(goodpractice)


goodpractice::gp()
devtools::document()
devtools::test()
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
devtools::check()


rhub::rhub_check(platform = "windows")
rhub::rhub_check(platform = "linux")
rhub::rhub_check(platform = "macos")

usethis::use_cran_comments()
