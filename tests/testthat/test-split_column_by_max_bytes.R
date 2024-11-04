## Test 1: split_text_by_max_bytes: Split a text by specifid bytes ----
test_that("split_text_by_max_bytes Test 1: Split a text by specifid bytes", {

  text <- "This is a test of the emergency broadcast system. HOGE:This is only a test. If this were a real-emergency, you would be instructed to do something. But this is not a real-emergency; so you can relax. data:sdkfjskdfjsifjskfjskdjfskdljfskldfjskldjfskdljfksldjfskdljfskdlfj"

  result <- c("This is a test of the emergency broadcast system. HOGE:This is only a test. If this were a real-emergency, you would be instructed to do something. But this is not a real-emergency; so you can relax. ", "data:sdkfjskdfjsifjskfjskdjfskdljfskldfjskldjfskdljfksldjfskdljfskdlfj")

  expect_equal(split_text_by_max_bytes(text, max_bytes= 200), result)
})
