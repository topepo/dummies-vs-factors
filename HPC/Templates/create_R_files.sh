#!/bin/bash

R CMD BATCH --vanilla C5boostrules_make.R
R CMD BATCH --vanilla C5boosttree_make.R
R CMD BATCH --vanilla C5rules_make.R
R CMD BATCH --vanilla C5tree_make.R
R CMD BATCH --vanilla cforest_make.R
R CMD BATCH --vanilla ctree_make.R
R CMD BATCH --vanilla gbm_make.R
R CMD BATCH --vanilla rf_make.R
R CMD BATCH --vanilla rpart_make.R
R CMD BATCH --vanilla treebag_make.R
