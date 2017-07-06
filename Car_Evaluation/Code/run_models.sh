#!/bin/bash

make -i -j 11 -f C5boosttree_make
make -i -j 11 -f cforest_make
make -i -j 11 -f ctree_make
make -i -j 11 -f rpart_make
make -i -j 11 -f C5boostrules_make
make -i -j 11 -f C5rules_make
make -i -j 11 -f treebag_make
#make -i -j 11 -f gbm_make
make -i -j 11 -f rf_make
make -i -j 11 -f C5tree_make