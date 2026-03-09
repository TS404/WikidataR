# test search functions

test_that("English-language search works",{
  expect_no_error(find_item("Wonder Girls", "en"))
})

test_that("Non-English-language search works",{
  expect_no_error(find_item("Wonder Girls", "es"))
})

test_that("Property search works",{
  expect_no_error(find_property("Music", "en"))
})
