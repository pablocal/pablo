# Metadata ----------------------------------------------------------------
# Title: test_elections.R
# Purpose: test functions related to elections
# Author(s): @pablocal
# Date: 2020-06-06 11:41:48
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------
context("election inputs")
library(pablo)

test_that("check_input_output errors", {
  expect_error(check_input_output("Summary"), "output must be 'summary' or 'all'")
})

test_that("check_input_by errors", {
  expect_error(check_input_by("overall"), "by must be one of 'national', 'regional', 'province', 'municipality', 'district', 'section', 'ballotbox'")
  expect_error(check_input_by(c("national", "regional")), "by must be of length 1")
})

