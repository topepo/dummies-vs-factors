library(caret)
library(gbm)
library(pROC)
library(rsample)

###################################################################

seed <- SEED

data("attrition")
attrition$Education <- 
  factor(as.character(attrition$Education))
attrition$EnvironmentSatisfaction <- 
  factor(as.character(attrition$EnvironmentSatisfaction))
attrition$JobInvolvement <- 
  factor(as.character(attrition$JobInvolvement))
attrition$JobSatisfaction <- 
  factor(as.character(attrition$JobSatisfaction))
attrition$PerformanceRating <- 
  factor(as.character(attrition$PerformanceRating))
attrition$RelationshipSatisfaction <- 
  factor(as.character(attrition$RelationshipSatisfaction))
attrition$WorkLifeBalance <- 
  factor(as.character(attrition$WorkLifeBalance))

###################################################################


stats <- function(...) {
  c(defaultSummary(...),
    twoClassSummary(...),
    prSummary(...),
    mnLogLoss(...))
}

ctrl <- trainControl(method = "cv", 
                     sampling = "down",
                     search = "random",
                     classProbs = TRUE,
                     summaryFunction = stats)

###################################################################

set.seed(seed)
in_train <- createDataPartition(attrition$Attrition, p = 3/4, list = FALSE)
training <- attrition[ in_train, ]
testing  <- attrition[-in_train, ]

mod <- train(Attrition ~ ., data = training, 
             method = "gbm",
             tuneLength = 25,
             verbose = FALSE,
             metric = "ROC",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$Attrition

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Attrition"
test_res$Model <- "Stochastic Gradient Boosting"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Dummy Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Attrition"
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
