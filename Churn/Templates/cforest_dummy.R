library(caret)
library(party)
library(pROC)
library(C50)

###################################################################

seed <- SEED

data("churn")
churn <- rbind(churnTrain, churnTest)

###################################################################


stats <- function(...) {
  c(defaultSummary(...),
    twoClassSummary(...),
    prSummary(...),
    mnLogLoss(...))
}

ctrl <- trainControl(method = "cv", 
                     sampling = "down",
                     classProbs = TRUE,
                     summaryFunction = stats)

###################################################################

set.seed(seed)
in_train <- createDataPartition(churn$churn, p = 3/4, list = FALSE)
training <- churn[ in_train, ]
testing  <- churn[-in_train, ]

mod <- train(churn ~ ., data = training, 
             method = "cforest",
             tuneLength = 10,
             controls = cforest_unbiased(ntree = 100),
             metric = "ROC",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$churn

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Churn"
test_res$Model <- "Conditional Inference Forest"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Dummy Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Churn"
rs_res$Model <- "Conditional Inference Forest"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Dummy Variables"

###################################################################

save(test_res, rs_res,
     file = file.path("..", "Results",
                      paste0("cforest_dummy_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
