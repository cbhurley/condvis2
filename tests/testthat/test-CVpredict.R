library(condvis2)

context("Test CVpredict()")





test_that("CVpredict lm", {
  f <- lm(Sepal.Length ~ ., data=iris)
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
  expect_vector(CVpredict(f, iris[1:4,],ptrans=log) , ptype = double(), size = 4) 
  m <- CVpredict(f, iris[1:4,],pinterval="confidence")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
  
})


test_that("CVpredict glm", {
  iris1 <- droplevels(iris[1:100,])
  f <- glm(Species ~ Sepal.Length, data=iris1, family="binomial")
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris1[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris1[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))
  
})




test_that("CVpredict lda", {
  f <- MASS::lda(Species ~ ., data=iris)
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})



test_that("CVpredict nnet factor", {
    skip_if_not_installed("nnet")
    f <- nnet::nnet(Species ~ ., data=iris,size=1, trace=FALSE)
    r <- CVpredict(f, iris[1:4,]) 
    expect_vector(r ,  size = 4) 
    expect_that(r, is_a("factor"))
    expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
    
    m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
    expect_that(m, is_a("matrix"))
    expect_type(m, "double")
    expect_that(dim(m), equals(c(4,3)))
  })
  
test_that("CVpredict nnet numeric", {
    skip_if_not_installed("nnet")
    f <- nnet::nnet(Sepal.Length ~ ., data=iris, size=1,trace=FALSE)
    expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
    expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
   })


test_that("CVpredict random forest factor", {
  skip_if_not_installed("randomForest")
  f <- randomForest::randomForest(Species ~ ., data=iris)
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})



test_that("CVpredict random forest numeric", {
  skip_if_not_installed("randomForest")
  f <- randomForest::randomForest(Sepal.Length ~ ., data=iris)
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})



test_that("CVpredict rpart factor", {
  skip_if_not_installed("rpart")
  f <- rpart::rpart(Species ~ ., data=iris)
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})

test_that("CVpredict rpart numeric", {
  skip_if_not_installed("rpart")
  f <- rpart::rpart(Sepal.Length ~ ., data=iris)
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})


test_that("CVpredict tree factor", {
  skip_if_not_installed("tree")
  f <- tree::tree(Species ~ ., data=iris)
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})

test_that("CVpredict tree numeric", {
  skip_if_not_installed("tree")
  f <- tree::tree(Sepal.Length ~ ., data=iris)
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})

test_that("CVpredict C5.0 factor", {
  skip_if_not_installed("C50")
  f <- C50::C5.0(Species ~ ., data=iris)
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})






test_that("CVpredict svm factor", {
  skip_if_not_installed("e1071")
  f <- e1071::svm(Species ~ ., data=iris,probability=TRUE)
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})

test_that("CVpredict svm numeric", {
  skip_if_not_installed("e1071")
  f <- e1071::svm(Sepal.Length ~ ., data=iris)
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})



test_that("CVpredict gbm factor", {
  skip_if_not_installed("gbm")
  iris1 <- droplevels(iris[1:100,])
  ylevels <- levels(iris1$Species)
  iris1$Species <- as.numeric(iris1$Species)-1
  f <- gbm::gbm(Species ~ ., data=iris1, distribution="bernoulli")
  r <- CVpredict(f, iris1[1:4,], ylevels=ylevels) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix",ylevels=ylevels)
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))
})

test_that("CVpredict gbm numeric", {
  skip_if_not_installed("gbm")
  f <- gbm::gbm(Sepal.Length ~ ., data=iris,distribution = "gaussian")
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})

test_that("CVpredict loess", {
  f <- loess(Sepal.Length ~ ., data=iris[,-5])
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})



test_that("CVpredict ksvm factor", {
  skip_if_not_installed("kernlab")
  f <- kernlab::ksvm(Species ~ ., prob.model=TRUE,data=iris)
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})


test_that("CVpredict ksvm numeric", {
  skip_if_not_installed("kernlab")
   f <- kernlab::ksvm(Sepal.Length ~ ., data=iris) 
   # expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) # ok if data provided
})






test_that("CVpredict glmnet factor", {
  skip_if_not_installed("glmnet")
  preds <- names(iris)[c(2,3)]
  mm <- function(d, p=preds) model.matrix(~ .-1,data=d[,p])
  iris1 <- droplevels(iris[1:100,])
  f <- glmnet::glmnet(x=mm(iris1),y=iris1[,5], family="binomial")
  irist <- iris1[c(1,2,50,51),]
  r <- CVpredict(f, irist, makex=mm,s=.25)
  expect_vector(r ,  size = 4)
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, irist, ptype="prob",makex=mm,s=.25)  , ptype = double(), size = 4)
  
  m <- CVpredict(f, irist,ptype="probmatrix",makex=mm,s=.25)
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))

  skip_if_not_installed("glmnetUtils")
  
  f1 <- glmnetUtils::glmnet(Species ~ ., data=iris,family="multinomial" )
  r1 <- CVpredict(f1, irist)
  expect_vector(r1 ,  size = 4)
  expect_that(r1, is_a("factor"))
  expect_vector(CVpredict(f1, irist, ptype="prob",s=.25)  , ptype = double(), size = 4)

  m1 <- CVpredict(f1, irist,ptype="probmatrix", s=.25)
  expect_that(m1, is_a("matrix"))
  expect_type(m1, "double")
  expect_that(dim(m1), equals(c(4,3)))

})



test_that("CVpredict glmnet numeric", {
  skip_if_not_installed("glmnet")
  preds <- names(iris)[2:5]
  mm <- function(d, p=preds) model.matrix(~ .-1,data=d[,p])
  f <- glmnet::glmnet(x=mm(iris),y=iris[,1])
  # expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,],makex=mm, s=0.0075) , ptype = double(), size = 4) 
  
  skip_if_not_installed("glmnetUtils")
  f1 <- glmnetUtils::glmnet(Sepal.Length ~ ., data=iris )
  expect_vector(CVpredict(f1, iris[1:4,], s=0.0075) , ptype = double(), size = 4) 
})


test_that("CVpredict cv.glmnet factor", {
  skip_if_not_installed("glmnet")
  preds <- names(iris)[1:4]
  mm <- function(d, p=preds) model.matrix(~ .-1,data=d[,p])
  iris1 <- droplevels(iris[1:100,])
  f <- glmnet::cv.glmnet(x=mm(iris1),y=iris1[,5], family="binomial")
  irist <- iris1[c(1,2,50,51),]
  r <- CVpredict(f, irist, makex=mm)
  expect_vector(r ,  size = 4)
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, irist, ptype="prob",makex=mm)  , ptype = double(), size = 4)

  m <- CVpredict(f, irist,ptype="probmatrix",makex=mm)
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))

  skip_if_not_installed("glmnetUtils")
  iris1 <- droplevels(iris[1:100,])
  f1 <- glmnetUtils::cv.glmnet(Species ~ ., data=iris,family="multinomial" )
  r1 <- CVpredict(f1, irist)
  expect_vector(r1 ,  size = 4)
  expect_that(r1, is_a("factor"))
  expect_vector(CVpredict(f1, irist, ptype="prob")  , ptype = double(), size = 4)

  m1 <- CVpredict(f1, irist,ptype="probmatrix")
  expect_that(m1, is_a("matrix"))
  expect_type(m1, "double")
  expect_that(dim(m1), equals(c(4,3)))

})


test_that("CVpredict cv.glmnet numeric", {
  skip_if_not_installed("glmnet")
  preds <- names(iris)[2:5]
  mm <- function(d, p=preds) model.matrix(~ .-1,data=d[,p])
  f <- glmnet::cv.glmnet(x=mm(iris),y=iris[,1])
  # expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,],makex=mm) , ptype = double(), size = 4) 
  
  skip_if_not_installed("glmnetUtils")
  f1 <- glmnetUtils::cv.glmnet(Sepal.Length ~ ., data=iris )
  expect_vector(CVpredict(f1, iris[1:4,]) , ptype = double(), size = 4) 
})







# test_that("CVpredict keras.engine.training.Model factor", {
#   skip_if_not_installed("keras")
#   suppressMessages(library(keras))
#   f <- keras::keras_model_sequential() 
#   keras::layer_dense(f,units = 8, activation = 'relu', 
#               input_shape = c(4)) %>% layer_dense(units = 3, activation = 'softmax')
#   
#   keras::compile(f,
#           loss = 'categorical_crossentropy',
#           optimizer = 'adam',
#           metrics = 'accuracy')
#   irisX <- as.matrix(iris[,1:4])
#   irisY <- keras::to_categorical(as.numeric(iris$Species))[, 2:4]
#   
#   keras::fit(f,irisX, irisY,
#       epochs = 10,
#       batch_size = 5,
#       validation_split = 0.2,view_metrics=F,verbose=0)
#   
#   r <- CVpredict(f, iris[1:4,], response=5, predictors=1:4) 
#   expect_vector(r ,  size = 4) 
#   expect_that(r, is_a("factor"))
#   expect_vector(CVpredict(f, iris[1:4,], ptype="prob",response=5, predictors=1:4)  , ptype = double(), size = 4) 
#   
#   m <- CVpredict(f, iris[1:4,],ptype="probmatrix",response=5, predictors=1:4)
#   expect_that(m, is_a("matrix"))
#   expect_type(m, "double")
#   expect_that(dim(m), equals(c(4,3)))
# })
# 
# 
# test_that("CVpredict keras.engine.training.Model numeric", {
#   skip_if_not_installed("keras")
#   suppressMessages(library(keras))
#   f <- keras::keras_model_sequential() 
#   keras::layer_dense(f,units = 2, activation = 'relu', 
#                      input_shape =3) %>% layer_dense(units = 1, activation = 'relu')
#   
#   keras::compile(f,
#                  loss = 'mse', optimizer = optimizer_rmsprop(),
#                  metrics = list("mean_absolute_error"))
#   irisX <- as.matrix(iris[,2:4])
#   irisY <- iris$Sepal.Length
#   
#   keras::fit(f,irisX, irisY,
#              epochs = 10,
#              batch_size = 5,
#              validation_split = 0.2,view_metrics=F,verbose=0)
#   
#   # expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
#   expect_vector(CVpredict(f, iris[1:4,],response=1, predictors=2:4) , ptype = double(), size = 4) 
# })



test_that("CVpredict kde", {
  skip_if_not_installed("ks")
  f <- ks::kde(iris[,1:2]) 
  expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})


test_that("CVpredict densityMclust", {
  skip_if_not_installed("mclust")
  f <- mclust::densityMclust(iris[,1:2],verbose=F) 
   # expect_vector(CVpredict(f) , ptype = double(), size = nrow(iris)) 
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})


test_that("CVpredict MclustDA", {
  skip_if_not_installed("mclust")
  suppressMessages(library(mclust))
  f <- mclust::MclustDA(iris[,1:4],iris[,5],verbose=F,modelType = "EDDA") 
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})

test_that("CVpredict MclustDR", {
  skip_if_not_installed("mclust")
  suppressMessages(library(mclust))
  f <- mclust::MclustDA(iris[,1:4],iris[,5],verbose=F,modelType = "EDDA")
  f <- mclust::MclustDR(f) 
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})



test_that("CVpredict Mclust", {
  skip_if_not_installed("mclust")
  suppressMessages(library(mclust))
  f <- mclust::Mclust(iris[,-5],verbose=F,modelType = "EDDA")
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = integer(), size = 4)
})


test_that("CVpredict caret factor", {
  skip_if_not_installed("caret")
  f <- caret::train(iris[,-5], iris[,5], 
                        method = "multinom", tuneGrid = expand.grid(decay = 0),trace=FALSE)
  
  r <- CVpredict(f, iris[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,3)))
})


test_that("CVpredict caret numeric", {
  skip_if_not_installed("caret")
  f <- caret::train(iris[,-1], iris[,1], method="lm")
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})


test_that("CVpredict bartMachine factor", {
  skip_if_not_installed("bartMachine")
  iris1 <- droplevels(iris[1:100,])
  f <- bartMachine::bartMachine(iris1[,-5], iris1[,5],verbose = FALSE)
  
  r <- CVpredict(f, iris1[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris1[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))
})


test_that("CVpredict bartMachine numeric", {
  skip_if_not_installed("bartMachine")
  iris1 <- droplevels(iris[1:100,])
  f <- bartMachine::bartMachine(iris1[,-1], iris1[,1],verbose = FALSE)
  expect_vector(CVpredict(f, iris1[1:4,]) , ptype = double(), size = 4) 
})


test_that("CVpredict parsnip factor", {
  skip_if_not_installed("parsnip")
  iris1 <- droplevels(iris[51:150,])
  
  f <- parsnip::logistic_reg()
  f <- parsnip::set_engine(f,"glm")
  f <- suppressWarnings(parsnip::fit(f, Species ~ ., data = iris1))
 
  r <- CVpredict(f, iris1[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris1[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris1[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))
})




test_that("CVpredict parsnip numeric", {
  skip_if_not_installed("parsnip")
  iris1 <- droplevels(iris[51:150,])
  
  f <- parsnip::linear_reg()
  f <- parsnip::set_engine(f,"lm")
  f <- suppressWarnings(parsnip::fit(f, Sepal.Length ~ ., data = iris1))
  expect_vector(CVpredict(f, iris1[1:4,]) , ptype = double(), size = 4) 
})


test_that("CVpredict mlr factor", {
  skip_if_not_installed("mlr")
  iris1 <- droplevels(iris[51:150,])
  task <- mlr::makeClassifTask(data=iris1, target="Species")
  lrn <- mlr::makeLearner("classif.binomial",predict.type = "prob")
  f  <- mlr::train(lrn,task)
  r <- CVpredict(f, iris1[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris1[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))
})


test_that("CVpredict mlr numeric", {
  skip_if_not_installed("mlr")
  iris1 <- droplevels(iris[51:150,])
  task <- mlr::makeRegrTask(data=iris1, target="Sepal.Length")
  lrn <- mlr::makeLearner("regr.lm")
  f  <- mlr::train(lrn,task)
  expect_vector(CVpredict(f, iris1[1:4,]) , ptype = double(), size = 4) 
})


test_that("CVpredict mlr3 factor", {
  skip_if_not_installed("mlr3")
  iris1 <- droplevels(iris[51:150,])
  task <- mlr3::TaskClassif$new(id = "iris1", backend = iris1, target = "Species")
  f<- mlr3::lrn("classif.rpart", predict_type="prob")
  f$train(task)
  
  r <- CVpredict(f, iris1[1:4,]) 
  expect_vector(r ,  size = 4) 
  expect_that(r, is_a("factor"))
  expect_vector(CVpredict(f, iris1[1:4,], ptype="prob")  , ptype = double(), size = 4) 
  
  m <- CVpredict(f, iris1[1:4,],ptype="probmatrix")
  expect_that(m, is_a("matrix"))
  expect_type(m, "double")
  expect_that(dim(m), equals(c(4,2)))
})


test_that("CVpredict mlr3 numeric", {
  skip_if_not_installed("mlr3")
  
  task <- mlr3::TaskRegr$new(id = "iris2", backend = iris, target = "Sepal.Length")
  f<- mlr3::lrn("regr.rpart")
  f$train(task)
  expect_vector(CVpredict(f, iris[1:4,]) , ptype = double(), size = 4) 
})
