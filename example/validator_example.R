library(assertr)
library(dplyr)
# Create new 'validator' object of class 'Validator'
validator <- Validator$new()

# Let's add some basic validations:
sample_data <- tibble(
  x = letters[1:3],
  y = letters[1:3],
  z = 1:3)

validation_rules <- function(data) {
  data %>%
    verify(title = "x should have character class", class(x) == "character") %>%
    verify(title = "y should have numeric class", class(y) == "numeric") %>%
    verify(title = "y should have date class", class(y) == "Date") %>%
    verify(title = "z should have Date class", ignore_chain_funs = TRUE,
           error_fun = append_as_warning, v_class(z) == "Date")
}

sample_data %>%
  validation_rules() %>%
  validator$add_validations(., "sample_data")

sample_data %>%
  validation_rules() %>%
  validator$add_validations(.)

validator$get_validations(type = "data.frame")
validator$get_validations(type = "json")
