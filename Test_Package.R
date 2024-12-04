

library(roxygen2) # In-Line Documentation for R
library(devtools) # Tools to Make Developing R Packages Easier
library(testthat) # Unit Testing for R
library(rhub)

devtools::document()
devtools::test()
devtools::check()


rhub::rhub_check(platform = "windows")
rhub::rhub_check(platform = "linux")
rhub::rhub_check(platform = "macos")


