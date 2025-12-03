test_that("ntickets returns named list", {
  res <- ntickets(200, 0.02, 0.95)
  expect_type(res, "list")
  expect_true(all(c("nd","nc","N","p","gamma") %in% names(res)))
})
