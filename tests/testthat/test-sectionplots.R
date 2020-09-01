library(condvis2)
library(MASS)
library(randomForest)
context("Test sectionPlot()")




Boston1 <- Boston
Boston1$crim <- cut(Boston1$crim, breaks=3)
levels(Boston1$crim) <- c("c1", "c2", "c3")
Boston1$chas <- as.factor(Boston1$chas)
Boston1$zn <- cut(Boston1$zn,breaks=2)
levels(Boston1$zn) <- c("zn1", "zn2")


pointColor <- "steelblue"


f <- randomForest(medv ~ ., data=Boston1)
preds <- names(Boston1)[1:6]

test_that("Section plot nn", {
  ptest<- sectionPlot(Boston1,f, response="medv",preds=c(preds,"lstat"),
                      sectionvar="lstat", conditionvals= Boston1[32,],pointColor=pointColor )
  expect_equal(ptest,NULL)
})



test_that("Section plot nf", {
  ptest<- sectionPlot(Boston1,f, response="medv",preds=preds,
                      sectionvar="zn", conditionvals= Boston1[2,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot nnn", {
  ptest<- sectionPlot(Boston1,f, response="medv",preds=preds,
                      sectionvar=c("lstat","rm"), conditionvals= Boston1[2,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot nfn", {
  ptest<- sectionPlot(Boston1,f, response="medv",preds=preds,
                      sectionvar=c("lstat","chas"), conditionvals= Boston1[2,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot nff", {
  ptest<- sectionPlot(Boston1,f, response="medv",preds=preds,
                      sectionvar=c("zn","chas"), conditionvals= Boston1[2,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot pairs", {
  ptest<- sectionPlot(Boston1,NULL, response="medv",preds=names(Boston1)[1:6],
                      sectionvar=names(Boston1)[1:3], conditionvals= Boston1[2,],
                      pointColor=pointColor, dataplot="pairs")

  expect_equal(ptest,NULL)
})


test_that("Section plot pcp", {
  ptest<- sectionPlot(Boston1,NULL, response="medv",preds=names(Boston1)[1:6],
                      sectionvar=names(Boston1)[1:3], conditionvals= Boston1[2,],
                      pointColor=pointColor, dataplot="pcp")
  expect_equal(ptest,NULL)
})




Boston2 <- Boston1
Boston2$medv <- cut(Boston2$medv, breaks=3)
levels(Boston2$medv) <- c("lo","mid", "hi")
f1 <- lda(medv ~ ., data=Boston2)



test_that("Section plot fn", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar="lstat", conditionvals= Boston2[2,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot pn", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar="lstat", conditionvals= Boston2[2,],probs=T,pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot fnn", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("lstat","rm"), conditionvals= Boston2[2,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot pnn", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("lstat","rm"), conditionvals= Boston2[2,],probs=T,pointColor=pointColor)
  
  expect_equal(ptest,NULL)
})


test_that("Section plot fnf", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("lstat","chas"), conditionvals= Boston2[2,],pointColor=pointColor)
  
  expect_equal(ptest,NULL)
})



test_that("Section plot pnf", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("lstat","chas"), conditionvals= Boston2[2,],probs=T,pointColor=pointColor)
  expect_equal(ptest,NULL)
})

test_that("Section plot ff", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("chas"), conditionvals= Boston2[2,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot pf", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("chas"), conditionvals= Boston2[2,],probs=T,pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot fff", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("chas","zn"), conditionvals= Boston2[22,],pointColor=pointColor)
  expect_equal(ptest,NULL)
})


test_that("Section plot pff", {
  ptest<- sectionPlot(Boston2,f1, response="medv",preds=preds,
                      sectionvar=c("chas","zn"), conditionvals= Boston2[22,],probs=T,pointColor=pointColor)
  expect_equal(ptest,NULL)
})



