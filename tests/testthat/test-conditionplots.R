library(condvis2)

context("Test conditionPlot()")



test_that("Condition plot n", {
  ptest<- conditionPlot(iris, c("Sepal.Length"), 
                        c("Sepal.Length"=iris$Sepal.Length[1]), 
                        pointColor="Species")
  expect_equal(ptest,NULL)
})

test_that("Condition plot f", {
  ptest<- conditionPlot(iris, "Species", 
                        c("Species"=iris$Species[1]))
  expect_equal(ptest,NULL)
})


test_that("Condition plot nn", {
  ptest<- conditionPlot(iris, c("Sepal.Length","Sepal.Width"), 
                        c("Sepal.Length"=iris$Sepal.Length[1], "Sepal.Width"=iris$Sepal.Width[1]), 
                        pointColor="Species")
  expect_equal(ptest,NULL)
})

test_that("Condition plot nf", {
  ptest<- conditionPlot(iris, c("Sepal.Length","Species"), 
                        c("Sepal.Length"=iris$Sepal.Length[1], "Species"=iris$Species[1]))
  expect_equal(ptest,NULL)
})


test_that("Condition plot ff", {
  ptest<- conditionPlot(CO2, c("Treatment","Type"), 
                        c("Treatment"=CO2$Treatment[1], "Type"=CO2$Type[1]))
  expect_equal(ptest,NULL)
})

