library(tidyverse)
library(boot)
set.seed(111)

aggregated_data_with_IRR <- read_csv("aggregated_data_with_IRR.csv")
aggregated_data_with_IRR$`Log IRR` <- log(aggregated_data_with_IRR$`IRR`)
aggregated_data_with_IRR$`Log IRR SE` <- 
  log(aggregated_data_with_IRR$`IRR Upper`/aggregated_data_with_IRR$`IRR Lower`)/3.92

aggregated_data_with_IRR %>% group_by(`Suicide Method`) %>% summarise(hotspot_count = n())
aggregated_data_with_IRR %>% group_by(`Suicide Method`, `Location Type`) %>% summarise(hotspot_count = n())

aggregated_data_with_IRR %>% group_by(`Intervention Type`) %>% summarise(hotspot_count = n())
aggregated_data_with_IRR %>% group_by(`Suicide Method`,`Intervention Type`) %>% summarise(hotspot_count = n())
aggregated_data_with_IRR %>% group_by(`Location Type`,`Intervention Type`) %>% summarise(hotspot_count = n())

aggregated_data_with_IRR %>% group_by(`Intervention Type (general)`) %>% summarise(hotspot_count = n())


summary(aggregated_data_with_IRR)


dotchart(aggregated_data_with_IRR$`IRR`)
hist(aggregated_data_with_IRR$`IRR`, breaks = 10)

dotchart(aggregated_data_with_IRR$`Log IRR`)
hist(aggregated_data_with_IRR$`Log IRR`, breaks = 10)

# Jumping from height * Physical Barriers
JumpFromHeight_PhysicalBarriers <- aggregated_data_with_IRR %>% filter(`Suicide Method` == "Jumping from height", `Intervention Type` == "Physical Barriers")
dotchart(JumpFromHeight_PhysicalBarriers$IRR)
hist(JumpFromHeight_PhysicalBarriers$IRR)
dotchart(JumpFromHeight_PhysicalBarriers$`Log IRR`)
hist(JumpFromHeight_PhysicalBarriers$`Log IRR`)


# Bootstrap weighted mean

weightedmean <- function(data, i, j) {
  d <- data[i, ]
  w <- j[i, ]
  return(weighted.mean(d, 1/w))   
}
result_jump_physical <- boot(data= JumpFromHeight_PhysicalBarriers[, "Log IRR", drop = FALSE], 
                             statistic = weightedmean, 
                             R = 10000, 
                             j = JumpFromHeight_PhysicalBarriers[, "Log IRR SE", drop = FALSE])
plot(result_jump_physical)
hist(exp(result_jump_physical$t), breaks = 15)
mean(exp(result_jump_physical$t))
exp(boot.ci(result_jump_physical)$bca[4:5])

result_jump_physical2 <- boot(data= JumpFromHeight_PhysicalBarriers[, "IRR", drop = FALSE], 
                             statistic = weightedmean, 
                             R = 10000, 
                             j = JumpFromHeight_PhysicalBarriers[, "Log IRR SE", drop = FALSE])
plot(result_jump_physical2)
hist(result_jump_physical2$t, breaks = 15)
mean(result_jump_physical2$t)
boot.ci(result_jump_physical2)$bca[4:5]

boot_samples_jump_physical <- as.data.frame(result_jump_physical2$t)
colnames(boot_samples_jump_physical) <- c("IRR_means")

ggplot(boot_samples_jump_physical, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Distribution of estimated population average IRR: jumping from height x physical barriers")
ggplot(boot_samples_jump_physical, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Jumping from height x All location types x Physical barriers")


write_csv(as.data.frame(result_jump_physical2$t, col.names = c("IRR_mean")), "jumping_from_height_physical_barriers_bootstrap_IRRmeans.csv")
head(as.data.frame(result_jump_physical2$t, col.names = c("IRR_mean")))


## Model evaluation

test_jump_physical <- sample(nrow(JumpFromHeight_PhysicalBarriers),3)
eval_result_jump_physical <- boot(data= JumpFromHeight_PhysicalBarriers[-test_jump_physical, "IRR", drop = FALSE], 
                           statistic = weightedmean, 
                           R = 10000, 
                           j = JumpFromHeight_PhysicalBarriers[-test_jump_physical, "Log IRR SE", drop = FALSE])
plot(eval_result_jump_physical)
mean(eval_result_jump_physical$t)
eval_ci_jump_physical <- boot.ci(eval_result_jump_physical)$bca[4:5]
# training set coverage probability
mean(JumpFromHeight_PhysicalBarriers[-test_jump_physical, "IRR"] >= eval_ci_jump_physical[1] & JumpFromHeight_PhysicalBarriers[-test_jump_physical, "IRR"] <= eval_ci_jump_physical[2])

# test set coverage probability
mean(JumpFromHeight_PhysicalBarriers[test_jump_physical, "IRR"] >= eval_ci_jump_physical[1] & JumpFromHeight_PhysicalBarriers[test_jump_physical, "IRR"] <= eval_ci_jump_physical[2])

eval_means_jump_physical <- c()
train_coverage_prob_jump_physical <- c()
test_coverage_prob_jump_physical <- c()
train_coverage_prob2_jump_physical <- c()
test_coverage_prob2_jump_physical <- c()


for (i in 1:500) {
  test_jump_physical <- sample(nrow(JumpFromHeight_PhysicalBarriers),3)
  eval_result_jump_physical <- boot(data= JumpFromHeight_PhysicalBarriers[-test_jump_physical, "IRR", drop = FALSE], 
                                    statistic = weightedmean, 
                                    R = 10000, 
                                    j = JumpFromHeight_PhysicalBarriers[-test_jump_physical, "Log IRR SE", drop = FALSE])
  eval_means_jump_physical[i] <- mean(eval_result_jump_physical$t)
  eval_ci_jump_physical <- boot.ci(eval_result_jump_physical)$bca[4:5]
  # training set coverage probability
  train_coverage_prob_jump_physical[i] <- 
    mean(JumpFromHeight_PhysicalBarriers$IRR[-test_jump_physical]) >= eval_ci_jump_physical[1] & mean(JumpFromHeight_PhysicalBarriers$IRR[-test_jump_physical]) <= eval_ci_jump_physical[2]
  # training prob. of individual IRR contained in bootstrapped C.I.
  train_coverage_prob2_jump_physical[i] <- 
    mean(JumpFromHeight_PhysicalBarriers[-test_jump_physical, "IRR"] >= eval_ci_jump_physical[1] & JumpFromHeight_PhysicalBarriers[-test_jump_physical, "IRR"] <= eval_ci_jump_physical[2])
  # test set coverage probability
  test_coverage_prob_jump_physical[i] <- 
    mean(JumpFromHeight_PhysicalBarriers$IRR[test_jump_physical]) >= eval_ci_jump_physical[1] & mean(JumpFromHeight_PhysicalBarriers$IRR[test_jump_physical]) <= eval_ci_jump_physical[2]
  # testing prob. of individual IRR contained in bootstrapped C.I.
  test_coverage_prob2_jump_physical[i] <- 
    mean(JumpFromHeight_PhysicalBarriers[test_jump_physical, "IRR"] >= eval_ci_jump_physical[1] & JumpFromHeight_PhysicalBarriers[test_jump_physical, "IRR"] <= eval_ci_jump_physical[2])
  
}


# Jumping from height * Bridge * Physical Barriers
JumpFromHeight_Bridge_PhysicalBarriers <- aggregated_data_with_IRR %>% filter(`Suicide Method` == "Jumping from height", `Location Type` == "Bridge", `Intervention Type` == "Physical Barriers")

result_jump_bridge_physical2 <- boot(data= JumpFromHeight_Bridge_PhysicalBarriers[, "IRR", drop = FALSE], 
                              statistic = weightedmean, 
                              R = 10000, 
                              j = JumpFromHeight_Bridge_PhysicalBarriers[, "Log IRR SE", drop = FALSE])
plot(result_jump_bridge_physical2)
hist(result_jump_bridge_physical2$t, breaks = 15)
mean(result_jump_bridge_physical2$t)
boot.ci(result_jump_bridge_physical2)$bca[4:5]

boot_samples_jump_bridge_physical <- as.data.frame(result_jump_bridge_physical2$t)
colnames(boot_samples_jump_bridge_physical) <- c("IRR_means")

ggplot(boot_samples_jump_bridge_physical, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Distribution of estimated population average IRR: jumping from height x bridge x physical barriers")
ggplot(boot_samples_jump_bridge_physical, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Jumping from height x Bridges x Physical barriers")


write_csv(data.frame(IRR_mean = result_jump_bridge_physical2$t), "jumping_from_height_bridge_physical_barriers_bootstrap_IRRmeans.csv")
head(data.frame(IRR_mean = result_jump_bridge_physical2$t))


## Model evaluation
eval_means_jump_bridge_physical <- c()
train_coverage_prob_jump_bridge_physical <- c()
test_coverage_prob_jump_bridge_physical <- c()
train_coverage_prob2_jump_bridge_physical <- c()
test_coverage_prob2_jump_bridge_physical <- c()

for (i in 1:500) {
  test_jump_bridge_physical <- sample(nrow(JumpFromHeight_Bridge_PhysicalBarriers),3)
  eval_result_jump_bridge_physical <- boot(data= JumpFromHeight_Bridge_PhysicalBarriers[-test_jump_bridge_physical, "IRR", drop = FALSE], 
                                    statistic = weightedmean, 
                                    R = 10000, 
                                    j = JumpFromHeight_Bridge_PhysicalBarriers[-test_jump_bridge_physical, "Log IRR SE", drop = FALSE])
  eval_means_jump_bridge_physical[i] <- mean(eval_result_jump_bridge_physical$t)
  eval_ci_jump_bridge_physical <- boot.ci(eval_result_jump_bridge_physical)$bca[4:5]
  # training set coverage probability
  train_coverage_prob_jump_bridge_physical[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriers$IRR[-test_jump_bridge_physical]) >= eval_ci_jump_bridge_physical[1] & mean(JumpFromHeight_Bridge_PhysicalBarriers$IRR[-test_jump_bridge_physical]) <= eval_ci_jump_bridge_physical[2]
  # training prob. of individual IRR contained in bootstrapped C.I.
  train_coverage_prob2_jump_bridge_physical[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriers[-test_jump_bridge_physical, "IRR"] >= eval_ci_jump_bridge_physical[1] 
         & JumpFromHeight_Bridge_PhysicalBarriers[-test_jump_bridge_physical, "IRR"] <= eval_ci_jump_bridge_physical[2])
  # test set coverage probability
  test_coverage_prob_jump_bridge_physical[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriers$IRR[test_jump_bridge_physical]) >= eval_ci_jump_bridge_physical[1] & mean(JumpFromHeight_Bridge_PhysicalBarriers$IRR[test_jump_bridge_physical]) <= eval_ci_jump_bridge_physical[2]
  # testing prob. of individual IRR contained in bootstrapped C.I.
  test_coverage_prob2_jump_bridge_physical[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriers[test_jump_bridge_physical, "IRR"] >= eval_ci_jump_bridge_physical[1] 
         & JumpFromHeight_Bridge_PhysicalBarriers[test_jump_bridge_physical, "IRR"] <= eval_ci_jump_bridge_physical[2])
  
}



# Jumping from height * (Physical Barriers + Encouraging help seeking)
JumpFromHeight_PhysicalBarriersHelpSeeking <- aggregated_data_with_IRR %>% filter(`Suicide Method` == "Jumping from height", 
                                                                                  `Intervention Type` == "Physical Barriers + Encouraging Help Seeking")
result_jump_physical_helpseeking <- boot(data= JumpFromHeight_PhysicalBarriersHelpSeeking[, "IRR", drop = FALSE], 
                                     statistic = weightedmean, 
                                     R = 10000, 
                                     j = JumpFromHeight_PhysicalBarriersHelpSeeking[, "Log IRR SE", drop = FALSE])
plot(result_jump_physical_helpseeking)
hist(result_jump_physical_helpseeking$t, breaks = 15)
mean(result_jump_physical_helpseeking$t)
boot.ci(result_jump_physical_helpseeking)$bca[4:5]

boot_samples_jump_physical_help <- as.data.frame(result_jump_physical_helpseeking$t)
colnames(boot_samples_jump_physical_help) <- c("IRR_means")

ggplot(boot_samples_jump_physical_help, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Estimated population average IRR: jumping from height x (physical barriers + encourage help seeking)")
ggplot(boot_samples_jump_physical_help, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Jumping from height x All location types x (Physical barriers + Encourage help seeking)")


write_csv(data.frame(IRR_mean = result_jump_physical_helpseeking$t), "jumping_from_height_(physical_barriers+help_seeking)_bootstrap_IRRmeans.csv")


## Model evaluation
eval_means_jump_physical_help <- c()
train_coverage_prob_jump_physical_help <- c()
test_coverage_prob_jump_physical_help <- c()
train_coverage_prob2_jump_physical_help <- c()
test_coverage_prob2_jump_physical_help <- c()

for (i in 1:500) {
  test_jump_physical_help <- sample(nrow(JumpFromHeight_PhysicalBarriersHelpSeeking),3)
  eval_result_jump_physical_help <- boot(data= JumpFromHeight_PhysicalBarriersHelpSeeking[-test_jump_physical_help, "IRR", drop = FALSE], 
                                           statistic = weightedmean, 
                                           R = 10000, 
                                           j = JumpFromHeight_PhysicalBarriersHelpSeeking[-test_jump_physical_help, "Log IRR SE", drop = FALSE])
  eval_means_jump_physical_help[i] <- mean(eval_result_jump_physical_help$t)
  eval_ci_jump_physical_help <- boot.ci(eval_result_jump_physical_help)$bca[4:5]
  # training set coverage probability
  train_coverage_prob_jump_physical_help[i] <- 
    mean(JumpFromHeight_PhysicalBarriersHelpSeeking$IRR[-test_jump_physical_help]) >= eval_ci_jump_physical_help[1] & mean(JumpFromHeight_PhysicalBarriersHelpSeeking$IRR[-test_jump_physical_help]) <= eval_ci_jump_physical_help[2]
  # training prob. of individual IRR contained in bootstrapped C.I.
  train_coverage_prob2_jump_physical_help[i] <- 
    mean(JumpFromHeight_PhysicalBarriersHelpSeeking[-test_jump_physical_help, "IRR"] >= eval_ci_jump_physical_help[1] 
         & JumpFromHeight_PhysicalBarriersHelpSeeking[-test_jump_physical_help, "IRR"] <= eval_ci_jump_physical_help[2])
  # test set coverage probability
  test_coverage_prob_jump_physical_help[i] <- 
    mean(JumpFromHeight_PhysicalBarriersHelpSeeking$IRR[test_jump_physical_help]) >= eval_ci_jump_physical_help[1] & mean(JumpFromHeight_PhysicalBarriersHelpSeeking$IRR[test_jump_physical_help]) <= eval_ci_jump_physical_help[2]
  # testing prob. of individual IRR contained in bootstrapped C.I.
  test_coverage_prob2_jump_physical_help[i] <- 
    mean(JumpFromHeight_PhysicalBarriersHelpSeeking[test_jump_physical_help, "IRR"] >= eval_ci_jump_physical_help[1] 
         & JumpFromHeight_PhysicalBarriersHelpSeeking[test_jump_physical_help, "IRR"] <= eval_ci_jump_physical_help[2])
  
}



# Jumping from height * Bridge * (Physical Barriers + Encouraging help seeking)
JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking <- aggregated_data_with_IRR %>% filter(`Suicide Method` == "Jumping from height", `Location Type` == "Bridge",
                                                                                  `Intervention Type` == "Physical Barriers + Encouraging Help Seeking")
result_jump_bridge_physical_helpseeking <- boot(data= JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[, "IRR", drop = FALSE], 
                                         statistic = weightedmean, 
                                         R = 10000, 
                                         j = JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[, "Log IRR SE", drop = FALSE])
plot(result_jump_bridge_physical_helpseeking)
hist(result_jump_bridge_physical_helpseeking$t, breaks = 15)
mean(result_jump_bridge_physical_helpseeking$t)
boot.ci(result_jump_bridge_physical_helpseeking)$bca[4:5]

boot_samples_jump_bridge_physical_help <- as.data.frame(result_jump_bridge_physical_helpseeking$t)
colnames(boot_samples_jump_bridge_physical_help) <- c("IRR_means")

ggplot(boot_samples_jump_bridge_physical_help, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Estimated population average IRR: jumping from height x bridge x (physical barriers + encourage help seeking)")
ggplot(boot_samples_jump_bridge_physical_help, aes(x = IRR_means)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="grey") +
  ggtitle("Jumping from height x Bridges x (Physical barriers + Encourage help seeking)")


write_csv(data.frame(IRR_mean = result_jump_bridge_physical_helpseeking$t), "jumping_from_height_bridge_(physical_barriers+help_seeking)_bootstrap_IRRmeans.csv")


## Model evaluation
eval_means_jump_bridge_physical_help <- c()
train_coverage_prob_jump_bridge_physical_help <- c()
test_coverage_prob_jump_bridge_physical_help <- c()
train_coverage_prob2_jump_bridge_physical_help <- c()
test_coverage_prob2_jump_bridge_physical_help <- c()

for (i in 1:1000) {
  test_jump_bridge_physical_help <- sample(nrow(JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking),3)
  eval_result_jump_bridge_physical_help <- boot(data= JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[-test_jump_bridge_physical_help, "IRR", drop = FALSE], 
                                         statistic = weightedmean, 
                                         R = 10000, 
                                         j = JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[-test_jump_bridge_physical_help, "Log IRR SE", drop = FALSE])
  eval_means_jump_bridge_physical_help[i] <- mean(eval_result_jump_bridge_physical_help$t)
  eval_ci_jump_bridge_physical_help <- boot.ci(eval_result_jump_bridge_physical_help)$bca[4:5]
  # training set coverage probability
  train_coverage_prob_jump_bridge_physical_help[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking$IRR[-test_jump_bridge_physical_help]) >= eval_ci_jump_bridge_physical_help[1] & mean(JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking$IRR[-test_jump_bridge_physical_help]) <= eval_ci_jump_bridge_physical_help[2]
  # training prob. of individual IRR contained in bootstrapped C.I.
  train_coverage_prob2_jump_bridge_physical_help[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[-test_jump_bridge_physical_help, "IRR"] >= eval_ci_jump_bridge_physical_help[1] 
         & JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[-test_jump_bridge_physical_help, "IRR"] <= eval_ci_jump_bridge_physical_help[2])
  # test set coverage probability
  test_coverage_prob_jump_bridge_physical_help[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking$IRR[test_jump_bridge_physical_help]) >= eval_ci_jump_bridge_physical_help[1] & mean(JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking$IRR[test_jump_bridge_physical_help]) <= eval_ci_jump_bridge_physical_help[2]
  # testing prob. of individual IRR contained in bootstrapped C.I.
  test_coverage_prob2_jump_bridge_physical_help[i] <- 
    mean(JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[test_jump_bridge_physical_help, "IRR"] >= eval_ci_jump_bridge_physical_help[1] 
         & JumpFromHeight_Bridge_PhysicalBarriersHelpSeeking[test_jump_bridge_physical_help, "IRR"] <= eval_ci_jump_bridge_physical_help[2])
  
}


# bridge x help seeking
JumpFromHeight_Bridge_HelpSeeking <- aggregated_data_with_IRR %>% filter(`Suicide Method` == "Jumping from height", `Location Type` == "Bridge",
                                                                         `Intervention Type` == "Encouraging Help Seeking")

# cliff x physical barriers
JumpFromHeight_Cliff_PhysicalBarrier <- aggregated_data_with_IRR %>% filter(`Suicide Method` == "Jumping from height", `Location Type` == "Cliff",
                                                                         `Intervention Type` == "Physical Barriers")
weighted.mean(JumpFromHeight_Cliff_PhysicalBarrier$IRR, 1/JumpFromHeight_Cliff_PhysicalBarrier$`Log IRR SE`)

# railways x physical barriers
Railways_PhysicalBarrier <- aggregated_data_with_IRR %>% filter(`Location Type` == "Railway", `Intervention Type` == "Physical Barriers")
weighted.mean(Railways_PhysicalBarrier$IRR, 1/Railways_PhysicalBarrier$`Log IRR SE`)


# railways x blue lights
Railways_BlueLights <- aggregated_data_with_IRR %>% filter(`Location Type` == "Railway", `Intervention Type` == "Blue Lights")
mean(Railways_BlueLights$IRR)
weighted.mean(Railways_BlueLights$IRR, 1/Railways_BlueLights$`Log IRR SE`)
