library(caret)
library(rpart)
library(pROC)
library(randomUniformForest)

###################################################################

seed <- SEED

data(carEvaluation)
carEvaluation$buying <- ordered(as.character(carEvaluation$buying),
                                levels = c("low", "med", "high", "vhigh"))
carEvaluation$buying <- as.numeric(carEvaluation$buying)
carEvaluation$priceOfMaintenance <- ordered(as.character(carEvaluation$priceOfMaintenance),
                                            levels = c("low", "med", "high", "vhigh"))
carEvaluation$priceOfMaintenance <- as.numeric(carEvaluation$priceOfMaintenance)
carEvaluation$safety <- ordered(as.character(carEvaluation$safety),
                                levels = c("low", "med", "high"))
carEvaluation$safety <- as.numeric(carEvaluation$safety)
carEvaluation$nbDoors <- ordered(as.character(carEvaluation$nbDoors),
                                 levels = c("2", "3", "4", "5more"))
carEvaluation$nbDoors <- as.numeric(carEvaluation$nbDoors)
carEvaluation$nbPersons <- ordered(as.character(carEvaluation$nbPersons),
                                   levels = c("2","4","more"))
carEvaluation$nbPersons <- as.numeric(carEvaluation$nbPersons)
carEvaluation$luggageBoot <- ordered(as.character(carEvaluation$luggageBoot),
                                     levels = c("small","med","big"))
carEvaluation$luggageBoot <- as.numeric(carEvaluation$luggageBoot)

###################################################################


ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary)

###################################################################

set.seed(seed)
in_train <- createDataPartition(carEvaluation$class, p = 3/4, list = FALSE)
training <- carEvaluation[ in_train, ]
testing  <- carEvaluation[-in_train, ]

mod <- train(x = training[, names(training) != "class"], 
             y = training$class,
             method = "rpart1SE",
             metric = "logLoss",
             trControl = ctrl)

###################################################################

test_pred <- predict(mod, testing, type = "prob")
test_pred$pred <- predict(mod, testing)
test_pred$obs <- testing$class

test_res <- multiClassSummary(test_pred, lev = levels(test_pred$obs))
test_res <- data.frame(t(test_res))
test_res$Data <- "Car Evaluation"
test_res$Model <- "CART"
test_res$Ordered <- "Yes"
test_res$Seed <- seed
test_res$Encoding <- "Linear Scores"
test_res$Time <- mod$times$everything[3]

###################################################################

rs_res <- mod$resample
rs_res$Data <- "Car Evaluation"
rs_res$Model <- "CART"
rs_res$Ordered <- "Yes"
rs_res$Seed <- seed
rs_res$Encoding <- "Linear Scores"

###################################################################

imp <- varImp(mod, scale = FALSE)$importance
imp$Data <- "Car Evaluation"
imp$Model <- "CART"
imp$Seed <- seed
imp$Variable  <- rownames(imp)

###################################################################

save(test_res, rs_res, imp,
     file = file.path("..", "Results",
                      paste0("rpart_scores_", seed, ".RData")))

###################################################################

sessionInfo()

q("no")
