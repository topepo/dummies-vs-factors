library(caret)
library(C50)
library(pROC)
library(evtree)

###################################################################

seed <- SEED

data("GermanCredit")
GermanCredit$employment_duration <-
   factor(as.character(GermanCredit$employment_duration))


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

mod <- train(credit_risk ~ ., data = training, 
             method = "C5.0",
             tuneGrid = data.frame(trials = c(1:20, 10*(3:10)),
                                   model = "tree",
                                   winnow = FALSE),
             metric = "ROC",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$credit_risk

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "German Credit"
test_res$Model <- "Boosted C5.0 Tree"
test_res$Ordered <- "None"
test_res$Seed <- seed
test_res$Encoding <- "Dummy Variables"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "German Credit"
rs_res$Model <- "Boosted C5.0 Tree"
rs_res$Ordered <- "None"
rs_res$Seed <- seed
rs_res$Encoding <- "Dummy Variables"

###################################################################

save(test_res, rs_res,
     file = file.path("..", "Results",
                      paste0("C5boosttree_dummy_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
