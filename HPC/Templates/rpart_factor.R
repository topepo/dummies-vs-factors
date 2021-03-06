library(caret)
library(rpart)
library(pROC)
library(AppliedPredictiveModeling)

###################################################################

seed <- SEED

data(schedulingData)


###################################################################



ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary)

###################################################################

set.seed(seed)
in_train <- createDataPartition(schedulingData$Class, p = 3/4, list = FALSE)
training <- schedulingData[ in_train, ]
testing  <- schedulingData[-in_train, ]

mod <- train(x = training[, names(training) != "Class"], 
             y = training$Class,
             method = "rpart1SE",
             metric = "logLoss",
             trControl = ctrl)

print(mod)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$Class

test_res <- multiClassSummary(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "High Performance Computing"
test_res$Model <- "CART"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Factor Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "High Performance Computing"
rs_res$Model <- "CART"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Factor Variables"

###################################################################

imp <- varImp(mod, scale = FALSE)$importance
imp$Data <- "High Performance Computing"
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
