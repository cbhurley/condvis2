library(condvis2)

context("Test tours")

mtcars1 <- mtcars
mtcars1$am <- factor(mtcars$am)
f <- lm(mpg ~ ., data=mtcars1)
f1 <- lm(mpg ~ wt, data=mtcars1)

test_that("randomPath", {
  pd <- randomPath(mtcars1, length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})


test_that("randomPath", {
  pd <- randomPath(mtcars1, length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})

test_that("seqPath", {
  pd <- seqPath(mtcars1, length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})


test_that("alongPath", {
  pd <- alongPath(mtcars1, "wt", length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,1)))
  pd <- alongPath(mtcars1, "am", length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(2,1)))
})


test_that("kmeansPath", {
  pd <- kmeansPath(mtcars1, length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})

test_that("pamPath", {
  pd <- pamPath(mtcars1, length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})


test_that("claraPath", {
  pd <- claraPath(mtcars, length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars))))
})

test_that("medoidPath", {
  pd <- medoidPath(mtcars1, c(rep(1,16), rep(2,16)))
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(2,ncol(mtcars1))))
})

test_that("centroidPath", {
  pd <- centroidPath(mtcars, c(rep(1,16), rep(2,16)))
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(2,ncol(mtcars))))
})

test_that("medoid", {
  pd <- medoid(mtcars1)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(1,ncol(mtcars1))))
})

test_that("lofPath", {
  pd <- lofPath(mtcars1,f,length=5, response="mpg")
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})

test_that("diffitsPath", {
  pd <- diffitsPath(mtcars1,list(f,f1),length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})


test_that("hiresponsePath", {
  pd <- hiresponsePath(mtcars1,response="mpg",length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})

test_that("loresponsePath", {
  pd <- loresponsePath(mtcars1,response="mpg",length=5)
  expect_that(pd, is_a("data.frame"))
  expect_that(dim(pd), equals(c(5,ncol(mtcars1))))
})

