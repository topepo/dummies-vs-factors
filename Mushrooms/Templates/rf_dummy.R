library(caret)
library(ranger)
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
                     summaryFunction = stats)

###################################################################

set.seed(seed)
in_train <- createDataPartition(mushroom$class, p = 3/4, list = FALSE)
training <- mushroom[ in_train, ]
testing  <- mushroom[-in_train, ]

mod <- train(class ~ ., data = training, 
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
test_pred$obs <- testing$class

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Mushrooms"
test_res$Model <- "Random Forest"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Dummy Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Mushrooms"
rs_res$Model <- "Random Forest"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Dummy Variables"

###################################################################

save(test_res, rs_res,
     file = file.path("..", "Results",
                      paste0("rf_dummy_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
