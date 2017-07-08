library(caret)
library(party)
library(pROC)
library(randomUniformForest)

###################################################################

seed <- SEED

data(carEvaluation)


###################################################################


ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary)

###################################################################

set.seed(seed)
in_train <- createDataPartition(carEvaluation$class, p = 3/4, list = FALSE)
training <- carEvaluation[ in_train, ]
testing  <- carEvaluation[-in_train, ]

mod <- train(class ~ ., data = training, 
             method = "ctree",
             tuneLength = 10,
             metric = "logLoss",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$class

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Car Evaluation"
test_res$Model <- "Conditional Inference Tree"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Dummy Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Car Evaluation"
rs_res$Model <- "Conditional Inference Tree"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Dummy Variables"

###################################################################

save(test_res, rs_res,
     file = file.path("..", "Results",
                      paste0("ctree_dummy_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
