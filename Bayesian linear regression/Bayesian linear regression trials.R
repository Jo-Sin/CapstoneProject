library(tidyverse)
set.seed(111)

aggregated_data_with_IRR <- read_csv("aggregated_data_with_IRR.csv")
aggregated_data_with_IRR$`Log IRR` <- log(aggregated_data_with_IRR$`IRR`)
aggregated_data_with_IRR$`Log IRR SE` <- 
  log(aggregated_data_with_IRR$`IRR Upper`/aggregated_data_with_IRR$`IRR Lower`)/3.92


# Linear regression explorations
linear_reg_IRR <- aggregated_data_with_IRR %>% select(`Suicide Method`, `Location Type`, `Intervention Type`,`Physical Restriction`, `Restriction Type`, `Restriction Height (m)`, `Signs with Emergency Contact`, `Crisis Telephones`, 
                                                      `CCTV Cameras`, `Safety Staff`, `Blue Lights`, `Reduction in Yearly Suicide Rate`, IRR, `Log IRR`)
linear_reg_IRR <- linear_reg_IRR %>% mutate(`Encouraging Help Seeking` = ifelse(`Signs with Emergency Contact` == 1 | `Crisis Telephones` == 1, 1, 0),
                                            `Intervention from Third Party` = ifelse(`CCTV Cameras` == 1 | `Safety Staff` == 1, 1, 0))

linear_reg_IRR.lm1 <- lm(`Log IRR` ~ 0+`Location Type`+`Physical Restriction`+`Encouraging Help Seeking`+`Intervention from Third Party`+`Blue Lights`, data = linear_reg_IRR)
summary(linear_reg_IRR.lm1)
plot(linear_reg_IRR.lm1)
linear_reg_IRR.lm2 <- lm(`Log IRR` ~ 0+`Location Type`+`Intervention Type`, data = linear_reg_IRR)
summary(linear_reg_IRR.lm2)
plot(linear_reg_IRR.lm2)


# Trying out Bayesian linear regression
install.packages("rstanarm")
install.packages("bayestestR")
install.packages("bayesplot")
library(rstanarm)
library(bayestestR)
library(bayesplot)
bayes.lm1 <- stan_glm(`Log IRR` ~ 0+`Location Type`+`Physical Restriction`+`Encouraging Help Seeking`+`Intervention from Third Party`+`Blue Lights`, data = linear_reg_IRR, seed=111)
summary(bayes.lm1)
describe_posterior(bayes.lm1)
mcmc_dens(bayes.lm1, pars=c("`Location Type`Bridge", "`Physical Restriction`"))
plot(bayes.lm1)
hdi(bayes.lm1)
posterior_sample <- as.data.frame(bayes.lm1)
colnames(posterior_sample) <- c("Bridge", "Building", "Cliff", "Forest", "Hospital", "Island", "Railway", "Terrace", "Physical_Restriction", "Encouraging_Help_Seeking", "Intervention_from_Third_Party", "Blue_Lights", "sigma")
hist(exp(posterior_sample$Bridge + posterior_sample$Physical_Restriction))
hist(exp(posterior_sample$Bridge + posterior_sample$Physical_Restriction + posterior_sample$Encouraging_Help_Seeking))
hist(exp(posterior_sample$Building + posterior_sample$Physical_Restriction))
hist(exp(posterior_sample$Cliff + posterior_sample$Physical_Restriction + posterior_sample$Encouraging_Help_Seeking + posterior_sample$Intervention_from_Third_Party))
hist(exp(posterior_sample$Bridge + posterior_sample$Encouraging_Help_Seeking + posterior_sample$Intervention_from_Third_Party))
hist(exp(posterior_sample$Island + posterior_sample$Encouraging_Help_Seeking + posterior_sample$Intervention_from_Third_Party))
hist(exp(posterior_sample$Bridge + posterior_sample$Physical_Restriction + posterior_sample$Intervention_from_Third_Party))
hist(exp(posterior_sample$Bridge + posterior_sample$Encouraging_Help_Seeking))
hist(exp(posterior_sample$Forest + posterior_sample$Encouraging_Help_Seeking))

bayes.lm2 <- stan_glm(`IRR` ~ 0+`Location Type`+`Intervention Type`, data = linear_reg_IRR, seed=111)
summary(bayes.lm2)
describe_posterior(bayes.lm2)
hdi(bayes.lm2)
posterior_sample2 <- as.data.frame(bayes.lm2)
colnames(posterior_sample2) <- c("Bridge", "Building", "Cliff", "Forest", "Hospital", "Island", "Railway", "Terrace", 
                                 "Encouraging_Help_Seeking", "Encouraging_Help_Seeking + Intervention_from_Third_Party", "Physical_Restriction", "Physical_Restriction + Encouraging_Help_Seeking", 
                                 "Physical_Restriction + Encouraging_Help_Seeking + Intervention_from_Third_Party", "Physical_Restriction + Intervention_from_Third_Party", "sigma")
hist(posterior_sample2$Bridge + posterior_sample2$Physical_Restriction)
hist(posterior_sample2$Bridge + posterior_sample2$`Physical_Restriction + Encouraging_Help_Seeking`)
hist(posterior_sample2$Building + posterior_sample2$Physical_Restriction)
hist(posterior_sample2$Cliff + posterior_sample2$`Physical_Restriction + Encouraging_Help_Seeking + Intervention_from_Third_Party`)
hist(posterior_sample2$Bridge + posterior_sample2$`Encouraging_Help_Seeking + Intervention_from_Third_Party`)
hist(posterior_sample2$Island + posterior_sample2$`Encouraging_Help_Seeking + Intervention_from_Third_Party`)
hist(posterior_sample2$Bridge + posterior_sample2$`Physical_Restriction + Intervention_from_Third_Party`)
hist(posterior_sample2$Bridge + posterior_sample2$Encouraging_Help_Seeking)
hist(posterior_sample2$Forest + posterior_sample2$Encouraging_Help_Seeking)


# include interactions between interventions
bayes.lm2 <- stan_glm(`Log IRR` ~ 0+`Location Type`+`Physical Restriction`*`Encouraging Help Seeking`*`Intervention from Third Party`+`Blue Lights`, data = linear_reg_IRR, seed=111)
summary(bayes.lm2)
describe_posterior(bayes.lm2)
mcmc_dens(bayes.lm1, pars=c("`Location Type`Bridge", "`Physical Restriction`"))
plot(bayes.lm1)



