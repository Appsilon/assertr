context("assertions about utils functions in utils.R")

data <- data.frame(stringsAsFactors = FALSE,
  id = LETTERS[1:5],
  age = 10:14,
  height = rnorm(5, 150, 10)
)

############### v_class ###############
test_that("v_class returns classes for variables in data.frame", {
  expect_equal(v_class(data), c("character", "integer", "numeric"))
})

test_that("v_class returns classes for variables in ellipsis", {
  expect_equal(v_class(data$id, data$age, data$height), c("character", "integer", "numeric"))
})

test_that("v_class works with pipe with verify", {
  expect_equal(data %>% verify(v_class(id, age, height) == c("character", "integer", "numeric")), data)
  expect_error(data %>% verify(v_class(id, age, height) == c("character", "numeric", "numeric")))
})
