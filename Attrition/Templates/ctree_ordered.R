library(caret)
library(C50)
library(pROC)
library(rsample)

###################################################################

seed <- SEED

data("attrition")


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
in_train <- createDataPartition(attrition$Attrition, p = 3/4, list = FALSE)
training <- attrition[ in_train, ]
testing  <- attrition[-in_train, ]

mod <- train(x = training[, names(training) != "Attrition"], 
             y = training$Attrition,
             method = "ctree",
             tuneLength = 10,
             metric = "ROC",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$Attrition

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Attrition"
test_res$Model <- "Conditional Inference Tree"
test_res$Ordered <- "Yes"
test_res$Seed <- seed
test_res$Encoding <- "Ordered Factors"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Attrition"
rs_res$Model <- "Conditional Inference Tree"
rs_res$Ordered <- "Yes"
rs_res$Seed <- seed
rs_res$Encoding <- "Ordered Factors"

###################################################################

save(test_res, rs_res,
     file = file.path("..", "Results",
                      paste0("ctree_ordered_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
