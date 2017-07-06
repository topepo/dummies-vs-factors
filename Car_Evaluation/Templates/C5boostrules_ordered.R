library(caret)
library(C50)
library(pROC)
library(randomUniformForest)

###################################################################

seed <- SEED

data(carEvaluation)
carEvaluation$buying <- ordered(as.character(carEvaluation$buying),
                                levels = c("low", "med", "high", "vhigh"))
carEvaluation$priceOfMaintenance <- ordered(as.character(carEvaluation$priceOfMaintenance),
                                            levels = c("low", "med", "high", "vhigh"))
carEvaluation$safety <- ordered(as.character(carEvaluation$safety),
                                levels = c("low", "med", "high"))
carEvaluation$nbDoors <- ordered(as.character(carEvaluation$nbDoors),
                                 levels = c("2", "3", "4", "5more"))
carEvaluation$nbPersons <- ordered(as.character(carEvaluation$nbPersons),
                                   levels = c("2","4","more"))
carEvaluation$luggageBoot <- ordered(as.character(carEvaluation$luggageBoot),
                                     levels = c("small","med","big"))


###################################################################


stats <- function(...) {
  c(multiClassSummary(...),
    mnLogLoss(...))
}

ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     summaryFunction = stats)

###################################################################

set.seed(seed)
in_train <- createDataPartition(carEvaluation$class, p = 3/4, list = FALSE)
training <- carEvaluation[ in_train, ]
testing  <- carEvaluation[-in_train, ]

mod <- train(x = training[, names(training) != "class"], 
             y = training$class,
             method = "C5.0",
             tuneGrid = data.frame(trials = c(1:20, 10*(3:10)),
                                   model = "rules",
                                   winnow = FALSE),
             metric = "logLoss",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$class

test_res <- stats(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Car Evaluation"
test_res$Model <- "Boosted C5.0 Rules"
test_res$Ordered <- "Yes"
test_res$Seed <- seed
test_res$Encoding <- "Ordered Factors"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Car Evaluation"
rs_res$Model <- "Boosted C5.0 Rules"
rs_res$Ordered <- "Yes"
rs_res$Seed <- seed
rs_res$Encoding <- "Ordered Factors"

###################################################################

save(test_res, rs_res,
     file = file.path("..", "Results",
                      paste0("C5boostrules_ordered_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
