library(roxygen2)  # In-Line Documentation for R
library(devtools)  # Tools to Make Developing R Packages Easier
library(testthat)  # Unit Testing for R
library(rhub)      # R Hub for Checking Package Builds
library(goodpractice)  # Package to Assess Package Quality
library(spelling)


# Document and test the package

devtools::load_all()
devtools::document()    # Regenerate documentation
devtools::test()        # Run package tests
devtools::run_examples()



# Disable system clock check (for CRAN checks)
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)

# Check the package for CRAN submission
devtools::check()



# Run good practice checks
detach("package:hydReng", unload = TRUE)
goodpractice::gp()


#Run spell check
spell_check_package()




# Add comments for CRAN submission
usethis::use_cran_comments()

# Perform checks for different platforms
rhub::rhub_check(platform = "windows")
rhub::rhub_check(platform = "linux")
rhub::rhub_check(platform = "macos")
