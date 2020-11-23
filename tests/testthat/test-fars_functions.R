test_that("make_filename works", {
  expect_equal(make_filename("2013"), "accident_2013.csv.bz2")
})

test_that("make_filename works", {
  expect_equal(make_filename("2014"), "accident_2014.csv.bz2")
})

test_that("make_filename works", {
  expect_equal(make_filename("2015"), "accident_2015.csv.bz2")
})
