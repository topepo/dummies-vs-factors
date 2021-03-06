library(caret)
library(rpart)
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

mod <- train(x = training[, names(training) != "class"], 
             y = training$class,
             method = "rpart1SE",
             metric = "logLoss",
             trControl = ctrl)

print(mod)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$class

test_res <- multiClassSummary(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Car Evaluation"
test_res$Model <- "CART"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Factor Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Car Evaluation"
rs_res$Model <- "CART"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Factor Variables"

###################################################################

imp <- varImp(mod, scale = FALSE)$importance
imp$Data <- "Car Evaluation"
imp$Model <- "CART"
imp$Seed <- seed
imp$Variable  <- rownames(imp)

###################################################################

save(test_res, rs_res, imp,
     file = file.path("..", "Results",
                      paste0("rpart_factor_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
