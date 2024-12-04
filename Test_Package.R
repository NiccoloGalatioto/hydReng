# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

myPaths<-.libPaths()
newpath<-"T:/07_TG-Wasserbau/02_Allgemein/01_Informatik/GIT/gai/library"
.libPaths(c(newpath,myPaths))

library(roxygen2) # In-Line Documentation for R
library(devtools) # Tools to Make Developing R Packages Easier
library(testthat) # Unit Testing for R
library(usethis)  # Automate Package and Project Setup
library(rhub)

devtools::document()
devtools::test()
devtools::check()

Sys.setenv(GITLAB_PAT = "XsGdb_JXXoTdfbFVNLBZ")
rhub::rhub_check()
