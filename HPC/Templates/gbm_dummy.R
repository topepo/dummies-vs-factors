library(caret)
library(gbm)
library(pROC)
library(AppliedPredictiveModeling)

###################################################################

seed <- SEED

data(schedulingData)


###################################################################



ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     search = "random",                     
                     summaryFunction = multiClassSummary)

###################################################################

set.seed(seed)
in_train <- createDataPartition(schedulingData$Class, p = 3/4, list = FALSE)
training <- schedulingData[ in_train, ]
testing  <- schedulingData[-in_train, ]

mod <- train(Class ~ ., data = training, 
             method = "gbm",
             tuneLength = 5, #50
             metric = "logLoss",
             verbose = FALSE,
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$Class

test_res <- multiClassSummary(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "High Performance Computing"
test_res$Model <- "Stochastic Gradient Boosting"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Dummy Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "High Performance Computing"
rs_res$Model <- "Stochastic Gradient Boostingt"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Dummy Variables"

###################################################################

save(test_res, rs_res,
     file = file.path("..", "Results",
                      paste0("gbm_dummy_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
