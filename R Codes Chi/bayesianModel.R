library(tidyverse)
library(readxl)
library(janitor)
library(psych)
library(rstanarm)
library(rstan)
library(brms)
library(bayestestR)
library(glmnet)

setwd("~/Google Drive/Unimelb/Masters/Data Science Project/Hierarchical Bayesian Model/Bayesian Models")

raw_data = read_excel("Final Aggregated Data.xlsx")
dat = raw_data
dat = dat %>% janitor::clean_names()
dat = dat%>% select(-distribution)
dat = dat%>% select(-link, -region)
dat = dat%>% select(-pre_intervention_outsiders_percent, -post_intervention_u_2028_outsiders_percent)
dat = dat%>% select(-pre_intervention_male_percent, -post_intervention_male_percent)
# dat = dat%>% select(-other_sites_u_2028_pre_intervention_suicide, -other_sites_u_2028_post_intervention_suicide, -other_sites_u_2028_yearly_pre_intervention_rate, -other_sites_yearly_post_intervention_rate)
# convert strings to factors
dat = as.data.frame(unclass(dat), stringsAsFactors = TRUE)
# replacing numeric na values with 0
dat_na_zero = dat
dat_na_zero[is.na(dat_na_zero)] = 0
View(dat_na_zero)
is.factor(dat_na_zero$suicide_method)
# getting rid of first 3 columns 
dat_final = dat_na_zero
dat_final = dat_final%>% select(-hotspot_name, -country, -city)
View(dat_final)


# Linear regression model 
model_linear = lm(post_intervention_suicide~., data = dat_final)
summary(model_linear)

# Poisson regression model 
model1 = glm(post_intervention_suicide ~ location_type
             + driving_distance_from_cbd_km
             + closest_psychiatric_hospital_km
             + suicide_method
             + hotspot_height_u_2028_m
             + restriction_type
             + restriction_height_m 
             + signs_with_emergency_contact
             + phones + cctv + safety_staff + blue_lights
             + pre_intervention_years 
             + pre_intervention_suicide
             + year_of_intervention
             + years_of_installation 
             + post_intervention_years
             + displacement ,data = dat_na_zero, family = 'poisson')
anova(model1, test="Chi")
model2 = glm(post_intervention_suicide ~ location_type
             + driving_distance_from_cbd_km
             + closest_psychiatric_hospital_km
             + suicide_method
             + hotspot_height_u_2028_m
             + restriction_type
             + restriction_height_m 
             + signs_with_emergency_contact
             + phones + cctv + safety_staff + blue_lights
             + pre_intervention_years 
             + pre_intervention_suicide
             + year_of_intervention
             + years_of_installation 
             + displacement, data = dat_na_zero, family = 'poisson')
anova(model2, test = "Chi")
summary(model2)
# Goodness of fit test
with(model2, cbind(res.deviance = deviance, df = df.residual, 
                   p = pchisq(deviance, df.residual, lower.tail = FALSE)))
# conclusion: p-val < 0.05 so goodness of fit chi-square test is significant
# and the model doesn't fit the data well 



# Poisson regression model with shrinkage (LASSO)
y = dat_final$post_intervention_suicide
x = data.matrix(dat_final[,c('location_type', 
                             'driving_distance_from_cbd_km',
                             'closest_psychiatric_hospital_km',
                             'suicide_method', 
                             'hotspot_height_u_2028_m',
                             'restriction_type',
                             'restriction_height_m',
                             'signs_with_emergency_contact',
                             'phones',
                             'cctv',
                             'safety_staff',
                             'blue_lights',
                             'pre_intervention_years',
                             'pre_intervention_suicide',
                             'year_of_intervention',
                             'years_of_installation',
                             'post_intervention_years',
                             'displacement')])
# cross-validation to select best lambda (the least MSE)
cv_model = cv.glmnet(x,y, family = 'gaussian', alpha=1, seed = 100)
best_lambda = cv_model$lambda.min
best_lambda
plot(cv_model)
model_poisson = glmnet(x,y,family = 'poisson', alpha = 1, lambda = best_lambda)
coef(model_poisson)




# Bayesian linear regression model (stan_glm function from rstanarm package)
# uniform prior, normal likelihood
# the code below takes almost 15 minutes to run and returns a bunch of warnings 
model_bayes = stan_glm(post_intervention_suicide ~., data = dat_final, prior = NULL, seed = 100)
describe_posterior(model_bayes)
# normal prior, normal likelihood
# model_bayes_normal = stan_glm(post_intervention_suicide ~., data = dat_final, seed = 1)


# Bayesian linear regression (brm function)
# normal prior, normal likelihood
model_bayes_2 = brm(post_intervention_suicide ~., data = dat_final, seed = 100)
summary(model_bayes_2)
posterior_summary(model_bayes_2)




# gibbs sampler example (from lecture 10 r codes)
n = 80
rho = 0.5
R = matrix(c(1,rho,rho,1),2,2)
y = mvrnorm(n, mu=c(3,6), Sigma=2*R)
bary = colMeans(y)
Rinv = solve(R)
ydiff = y - rep(1,n)%*%t(bary)
s2 = sum(diag(ydiff%*%Rinv%*%t(ydiff)))/(2*n-2)

mu = rnorm(2,2/n)
tau = rgamma(1,n,2*(n-1))
sigma2 = 1/tau
iter = 5000
para.est = matrix(0, iter, 3)
for (i in 1:iter) {
  mu[1] = rnorm(1,bary[1] + rho*(bary[2]-mu[2]), sd = sqrt(sigma2*(1-rho^2)/n))
  mu[2] = rnorm(1,bary[2] + rho*(bary[1]-mu[1]), sd = sqrt(sigma2*(1-rho^2)/n))
  SSmu = t(bary-mu)%*%Rinv%*%(bary-mu)
  
}









