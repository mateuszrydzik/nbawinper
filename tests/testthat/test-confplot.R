test_that("confplot errors", {
  expect_error(confplot("northern", 2017),
  "Please select a conference: 'eastern' or 'western'")
  expect_error(confplot("eastern", 1969),
    "Please select a year after 1970")
})
