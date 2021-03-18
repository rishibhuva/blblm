test_that("blblm works", {

  library(blblm)
  library(purrr)
  library(furrr)
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  coef(fit)



  expect_equal(length(coef(fit)), 4)
})


test_that("blblm works", {

  library(blblm)
  library(purrr)
  library(furrr)

  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  coef(fit)




  confint(fit, c("wt", "hp"))


  expect_equal(length(confint(fit, c("wt", "hp"))), 4)
})


test_that("blblm works", {

  library(blblm)
  library(purrr)
  library(furrr)

  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  coef(fit)





  confint(fit, c("wt", "hp"))


  sigma(fit)

  expect_equal(length(sigma(fit)), 1)
})


test_that("blblm works", {

  library(blblm)
  library(purrr)
  library(furrr)

  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  coef(fit)





  confint(fit, c("wt", "hp"))


  sigma(fit)

  sigma(fit, confidence = TRUE)

  expect_equal(length(sigma(fit, confidence = TRUE)), 3)
})


test_that("blblm works", {

  library(blblm)
  library(purrr)
  library(furrr)

  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  coef(fit)





  confint(fit, c("wt", "hp"))


  sigma(fit)

  sigma(fit, confidence = TRUE)

  predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))

  expect_equal(length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))), 2)
})


test_that("blblm works", {

  library(blblm)
  library(purrr)
  library(furrr)

  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  coef(fit)





  confint(fit, c("wt", "hp"))


  sigma(fit)

  sigma(fit, confidence = TRUE)

  predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))

  predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)

  expect_equal(length(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)), 6)
})


