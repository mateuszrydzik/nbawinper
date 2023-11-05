test_that("confdr errors", {
  expect_error(confdf("northern", 2017),
  "Please select a conference: 'eastern' or 'western'")
  expect_error(confdf("eastern", 1969),
    "Please select a year after 1970")
})
