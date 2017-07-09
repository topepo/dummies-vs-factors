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

test_results <- bind_rows(test_results) %>%
  arrange(Model, Seed, Encoding)
rs_results <-  bind_rows(rs_results)

table(test_results$Model, test_results$Encoding)

###################################################################

factor_results <- test_results %>%
  filter(Encoding == "Factor Variables") %>%
  select(-Encoding, -Ordered) %>%
  gather(Metric, Factors, Accuracy:logLoss, Time)

dummy_results <- test_results %>%
  filter(Encoding == "Dummy Variables") %>%
  select(-Encoding, -Ordered) %>%
  gather(Metric, Dummies, Accuracy:logLoss, Time)

ordered_results <- test_results %>%
  filter(Encoding == "Ordered Factors") %>%
  select(-Encoding, -Ordered) %>%
  gather(Metric, Ordered, Accuracy:logLoss, Time)

ordereddummy_results <- test_results %>%
  filter(Encoding == "Ordered Dummy Variables") %>%
  select(-Encoding, -Ordered) %>%
  gather(Metric, Ordered_Dummy, Accuracy:logLoss, Time)

merged_results <- full_join(factor_results, dummy_results) %>%
  full_join(ordered_results) %>%
  full_join(ordereddummy_results) 

###################################################################

if(interactive()) {
  merged_results %>%
    filter(Metric == "Time") %>%
    mutate(Difference = Dummies/Factors) %>%
    mutate(Model = gsub(" ", "\n", as.character(Model))) %>%
    mutate(Model = reorder(Model, Difference, median, na.rm = TRUE)) %>%
    ggplot(aes(x = Model, y = Difference)) + 
    geom_point()
}

###################################################################

save(merged_results, file = "Attrition.RData")
