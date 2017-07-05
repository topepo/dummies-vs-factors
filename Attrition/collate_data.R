library(dplyr)
library(tidyr)
library(ggplot2)

###################################################################

rdata_files <- list.files("Results", 
                          pattern = "RData$",
                          full.names = TRUE)

test_results <- vector(mode = "list", length = length(rdata_files))
rs_results <- test_results

###################################################################

for (i in seq_along(rdata_files)) {
  load(rdata_files[i])
  test_results[[i]] <- test_res
  rs_results[[i]] <- rs_res
  rm(test_res, rs_res)
}

test_results <- bind_rows(test_results)
rs_results <-  bind_rows(rs_results)

###################################################################

factor_results <- test_results %>%
  filter(Encoding == "Factor Variables") %>%
  select(-Encoding) %>%
  gather(Metric, Factors, Accuracy:logLoss, Time)


dummy_results <- test_results %>%
  filter(Encoding == "Dummy Variables") %>%
  select(-Encoding) %>%
  gather(Metric, Dummies, Accuracy:logLoss, Time)

merged_results <- full_join(factor_results, dummy_results)

###################################################################

if(interactive()) {
  merged_results %>%
    filter(Metric == "ROC") %>%
    mutate(Difference = Dummies-Factors) %>%
    mutate(Model = gsub(" ", "\n", as.character(Model))) %>%
    mutate(Model = reorder(Model, Difference, median, na.rm = TRUE)) %>%
    ggplot(aes(x = Model, y = Difference)) + 
    geom_boxplot()
}

###################################################################

save(merged_results, file = "Attrition.RData")

if(!interactive()) 
  q("no")
