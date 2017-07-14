load("Churn/Churn.RData")
churn <- merged_results
load("Mushrooms/Mushrooms.RData")
mushrooms <- merged_results
load("HPC/HPC.RData")
hpc <- merged_results
load("Car_Evaluation/Car_Evaluation.RData")
cars <- merged_results

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

sim_res <- bind_rows(churn, mushrooms, hpc)
sim_res$Ordered <- NA
sim_res$Ordered_Dummy <- NA

sim_res <- bind_rows(sim_res, cars)

sim_res %>%
  filter(Metric == "Time") %>%
  mutate(Difference = Dummies/Factors) %>%
  mutate(Model = gsub(" ", "\n", as.character(Model))) %>%
  mutate(Model = reorder(Model, Difference, median, na.rm = TRUE)) %>%
  group_by(Model, Data) %>%
  summarize(Speedup = median(Difference)) %>%
  ggplot(aes(x = Model, y = Speedup, group = Data, col = Data, shape = Data)) + 
  geom_point() + 
  theme(legend.position = "top")


sim_res %>%
  filter(Metric %in% c("ROC", "Accuracy") & Data == "Churn") %>%
  mutate(Difference = Dummies-Factors) %>%
  mutate(Model = gsub(" ", "\n", as.character(Model))) %>%
  mutate(Model = reorder(Model, Difference, mean, na.rm = TRUE)) %>%
  ggplot(aes(x = Model, y = Difference)) + 
  geom_boxplot() + 
  facet_wrap(~Metric, ncol = 1, scales = "free_y") + 
  geom_hline(yintercept = 0, col = "red", alpha = .5)



churn_roc <- sim_res %>%
  filter(Metric %in% "ROC" & Data == "Churn") %>%
  mutate(Difference = Dummies-Factors,
         Seed = factor(paste0("Sim", Seed)),
         Model = factor(Model))

library(lme4)

mod <- lmer(Difference ~ Model + (1 | Seed), churn_roc)





