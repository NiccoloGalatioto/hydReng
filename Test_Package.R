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


myPaths<-.libPaths()
newpath<-"T:/07_TG-Wasserbau/02_Allgemein/01_Informatik/GIT/gai/library"
.libPaths(c(newpath,myPaths))

library(roxygen2) # In-Line Documentation for R
library(devtools) # Tools to Make Developing R Packages Easier
library(testthat) # Unit Testing for R
library(usethis)  # Automate Package and Project Setup
library(rhub)
library(Rtools)

devtools::document()
devtools::test()
devtools::check()

devtools::check_rhub()

Sys.setenv(GITHUB_PAT_DEV_TKCONSULT_CH = "b-KPXhp8G3SN-_Q7zeYS")
Sys.getenv("GITLAB_PAT")
rhub::rhub_platforms()
rhub::rhub_check(platform = "windows")
