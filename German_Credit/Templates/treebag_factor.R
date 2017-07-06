library(caret)
library(ipred)
library(pROC)
library(evtree)

###################################################################

seed <- SEED

data("GermanCredit")


###################################################################


stats <- function(...) {
  c(defaultSummary(...),
    twoClassSummary(...),
    prSummary(...),
    mnLogLoss(...))
}

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = stats)

###################################################################

set.seed(seed)
in_train <- createDataPartition(GermanCredit$credit_risk, p = 3/4, list = FALSE)
training <- GermanCredit[ in_train, ]
testing  <- GermanCredit[-in_train, ]

mod <- train(x = training[, names(training) != "credit_risk"], 
             y = training$credit_risk,
             method = "treebag",
             nbagg = 50,
             metric = "ROC",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$credit_risk

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "German Credit"
test_res$Model <- "Bagged CART"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Factor Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "German Credit"
rs_res$Model <- "Bagged CART"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Factor Variables"

###################################################################

imp <- varImp(mod, scale = FALSE)$importance
imp$Data <- "German Credit"
imp$Model <- "Bagged CART"
imp$Seed <- seed
imp$Variable  <- rownames(imp)

###################################################################

save(test_res, rs_res, imp,
     file = file.path("..", "Results",
                      paste0("treebag_factor_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
