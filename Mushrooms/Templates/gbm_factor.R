library(caret)
library(gbm)
library(pROC)
library(ClusterR)

###################################################################

seed <- SEED

data("mushroom")
mushroom$veil_type <- NULL


###################################################################


stats <- function(...) {
  c(defaultSummary(...),
    twoClassSummary(...),
    prSummary(...),
    mnLogLoss(...))
}

ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     search = "random",                    
                     summaryFunction = stats)

###################################################################

set.seed(seed)
in_train <- createDataPartition(mushroom$class, p = 3/4, list = FALSE)
training <- mushroom[ in_train, ]
testing  <- mushroom[-in_train, ]

mod <- train(x = training[, names(training) != "class"], 
             y = training$class,
             method = "gbm",
             tuneLength = 25,
             verbose = FALSE,
             metric = "ROC",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$class

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Mushrooms"
test_res$Model <- "Stochastic Gradient Boosting"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Factor Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Mushrooms"
rs_res$Model <- "Stochastic Gradient Boosting"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Factor Variables"

###################################################################

imp <- varImp(mod, scale = FALSE)$importance
imp$Data <- "Mushrooms"
imp$Model <- "Stochastic Gradient Boosting"
imp$Seed <- seed
imp$Variable  <- rownames(imp)

###################################################################

save(test_res, rs_res, imp,
     file = file.path("..", "Results",
                      paste0("gbm_factor_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
