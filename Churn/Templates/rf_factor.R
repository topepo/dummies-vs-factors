library(caret)
library(ranger)
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

mod <- train(x = training[, names(training) != "churn"], 
             y = training$churn,
             method = "ranger",
             tuneLength = 10,
             verbose = FALSE, 
             seed = seed + 1,
             num.threads = 1,
             num.trees = 1500,
             importance = "impurity",
             metric = "ROC",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$churn

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Churn"
test_res$Model <- "Random Forest"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Factor Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Churn"
rs_res$Model <- "Random Forest"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Factor Variables"

###################################################################

imp <- varImp(mod, scale = FALSE)$importance
imp$Data <- "Churn"
imp$Model <- "Random Forest"
imp$Seed <- seed
imp$Variable  <- rownames(imp)

###################################################################

save(test_res, rs_res, imp,
     file = file.path("..", "Results",
                      paste0("rf_factor_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
