library(testthat)
library(httr2)
library(purrr)
library(dplyr)

# (Make sure to load your get_timeseries_core and get_regional_core functions here)

test_that("get_timeseries_core returns standard daily data", {
  # Tucson Airport (USW00023160) for Jan 2024
  df <- get_timeseries_core("USW00023160", "2024-01-01", "2024-01-31", "maxt")
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c("date", "value") %in% names(df)))
  expect_type(df$value, "double")
  expect_true(nrow(df) >= 28) # Account for potential missing days
})

test_that("get_timeseries_core handles departure from normal", {
  df <- get_timeseries_core("USW00023160", "2024-01-01", "2024-01-31", "maxt", normal_departure = TRUE)
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c("date", "value") %in% names(df)))
  # Departures should have negative and positive values, not just absolute temps
  expect_true(any(df$value < 0, na.rm = TRUE)) 
})

test_that("get_regional_core handles basic summaries and thresholds", {
  # Stations in AZ with most days >= 100F in July 2024
  df <- get_regional_core("AZ", "2024-07-01", "2024-07-31", "maxt", "cnt_ge_100")
  
  expect_s3_class(df, "data.frame")
  expect_true(all(c("station", "state", "value") %in% names(df)))
  expect_true(nrow(df) > 0)
  expect_true(max(df$value, na.rm = TRUE) <= 31) # Can't have more than 31 days in July
})

test_that("get_regional_core successfully appends the date of extremes", {
  # Max temp in NM for Jan 2024, including the exact date
  df <- get_regional_core("NM", "2024-01-01", "2024-01-31", "maxt", "max", include_date = TRUE)
  
  expect_s3_class(df, "data.frame")
  expect_true("date_of_extreme" %in% names(df))
  # Check if the date format looks like YYYY-MM-DD
  expect_match(df$date_of_extreme[1], "^\\d{4}-\\d{2}-\\d{2}$")
})

#####
# run test
#testthat::test_file("./utils/test_acis_api.R")
