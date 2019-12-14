library(tidyverse)
library(lfe)
library(lme4)
library(lmerTest)
library(gridExtra)

# Source data generation process functions
source("dgp_script.R") 

# Set iteration number for simulations
iter <- 100

# Set seed for reproducibility
set.seed(2020)

# BASE SIMULATION #####################################################################################################

## call base dgp and store data frame
classroom_dat <- class_dat_function(40, 25)

## store SATE and generate data frame for models
SATE <- mean(classroom_dat$Y1) - mean(classroom_dat$Y0)
model_stu <- classroom_dat %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest,
                                      minority, parent.edu, fam.income, freelunch, dist.school.hour, 
                                      gender, pretest, classid)

#### Initial Run ######################################################################################################
# Base Linear Regression
lr.base <- lm(Y_stud ~., data = model_stu[,-13])

# Base - Linear Regression Model Bias
(lr.base.bias <- lr.base$coefficients[2] - SATE)

# Base w/ Fixed Effects
fixed.base <- lfe::felm(model_stu$Y_stud ~ model_stu$Z_stud + model_stu$minority + model_stu$parent.edu +
                          model_stu$fam.income + model_stu$freelunch + model_stu$dist.school.hour + model_stu$gender +
                          model_stu$pretest | model_stu$classid) 

# note: felm function results are same as linear regression with factor(classid)

# Base - Fixed Effects Model Bias
(fe.base.bias <- fixed.base$coefficients[1] - SATE)

# Base w/ Random Effects
random.base <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                                gender + pretest + yearstea + avgtest + teach.edu + (1 | classid), data = model_stu)

# Base - Random Effects Model Bias
(re.base.bias <- summary(random.base)$coefficients[2,1] - SATE)

#### Base Randomization Distribution ##################################################################################

# Generate empty data frame for results
base_rd_sim <- data.frame(type = rep(NA, iter*3), coef = rep(NA, iter*3), 
                          conf_int_low = rep(NA, iter*3), conf_int_high = rep(NA, iter*3))

# Initialize counts which is used for which row to store outputs in
count <- 1

# For loop for randomization distribution - only alters treatment assigment

for(i in 1:iter){
  
  # Randomize treatment
  base_data <- classroom_dat
  N <- nrow(base_data)
  
  X_stud <- rnorm(N, 0, 1)
  prob_stud <- inv.logit((X_stud/max(abs(X_stud))) * log(19))
  Z_stud <- rbinom(N, 1, prob = prob_stud)
  
  base_data$Z_stud <- Z_stud
  base_data$Y_stud <- ifelse(Z_stud == 1, base_data$Y1, base_data$Y0)
  
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, 
                                          fam.income, freelunch, dist.school.hour, gender, pretest, classid)
  
  # Linear Regression Sim
  j <- count
  base_rd_sim[j,1] <- "lr"
  lr_base_sim <- lm(Y_stud ~., data = base_data_model[,-13])
  base_rd_sim[j,2] <- lr_base_sim$coefficients[2]
  base_rd_sim[j,3] <- confint(lr_base_sim, 'Z_stud', level = .95)[1,1]
  base_rd_sim[j,4] <- confint(lr_base_sim, 'Z_stud', level = .95)[1,2]
  
  # Fixed Effect Sim
  j <- count+1
  base_rd_sim[j,1] <- "fixed"
  fixed_base_sim <- lfe::felm(base_data_model$Y_stud ~ base_data_model$Z_stud + base_data_model$minority +
                                base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch +
                                base_data_model$dist.school.hour + base_data_model$gender + base_data_model$pretest |
                                base_data_model$classid) 
  base_rd_sim[j,2] <- fixed_base_sim$coefficients[1]
  base_rd_sim[j,3] <- confint(fixed_base_sim, 'base_data_model$Z_stud', level = .95)[1,1]
  base_rd_sim[j,4] <- confint(fixed_base_sim, 'base_data_model$Z_stud', level = .95)[1,2]
  
  # Random Effect Sim
  j <- count+2
  base_rd_sim[j,1] <- "random"
  random_base_sim <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                                      gender + pretest + yearstea + avgtest + teach.edu + (1 | classid), 
                                    data = base_data_model) 
  base_rd_sim[j,2] <- summary(random_base_sim)$coefficients[2,1]
  base_rd_sim[j,3] <- confint(random_base_sim, 'Z_stud', level = .95)[1,1]
  base_rd_sim[j,4] <- confint(random_base_sim, 'Z_stud', level = .95)[1,2]  
  
  # Increase count for next set of models
  count <- j+1
  # Status iteration print
  print(i)
}

# Add SATE to results data frame
base_rd_sim$SATE <- rep(SATE, nrow(base_rd_sim))
base_rd_sim$bias <- base_rd_sim$coef - base_rd_sim$SATE

# Histograms for base case randomization distribution
hist(base_rd_sim$coef[base_rd_sim$type == "lr"], main = "Randomization Distribution - Base (Linear Regression)", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

hist(base_rd_sim$coef[base_rd_sim$type == "fixed"], main = "Randomization Distribution - Base (Fixed Effects)", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

hist(base_rd_sim$coef[base_rd_sim$type == "random"], main = "Randomization Distribution - Base (Random Effects)", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

#### Base Sampling Distribution #######################################################################################

# Generate empty data frame for results
base_sd_sim <- data.frame(type = rep(NA, iter*3), 
                          coef = rep(NA, iter*3), 
                          conf_int_low = rep(NA, iter*3), 
                          conf_int_high = rep(NA, iter*3), 
                          SATE = rep(NA, iter*3))

# Initialize counts which is used for which row to store outputs in
count <- 1

for(i in 1:iter){
  
  #Sampling Distribution - resample data
  base_data <- class_dat_function(40, 25)
  SATE_sim <- mean(base_data$Y1) - mean(base_data$Y0)
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, 
                                          fam.income, freelunch, dist.school.hour, gender, pretest, classid)
  
  #Linear Regression Sim
  j <- count
  base_sd_sim[j,1] <- "lr"
  lr_base_sim <- lm(Y_stud ~., data = base_data_model[,-13])
  base_sd_sim[j,2] <- lr_base_sim$coefficients[2]
  base_sd_sim[j,3] <- confint(lr_base_sim, 'Z_stud', level = .95)[1,1]
  base_sd_sim[j,4] <- confint(lr_base_sim, 'Z_stud', level = .95)[1,2]
  base_sd_sim[j,5] <- SATE_sim
  
  #Fixed Effect Sim
  j <- count+1
  base_sd_sim[j,1] <- "fixed"
  fixed_base_sim <- lfe::felm(base_data_model$Y_stud ~ base_data_model$Z_stud + base_data_model$minority +
                                base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch +
                                base_data_model$dist.school.hour + base_data_model$gender + base_data_model$pretest |
                                base_data_model$classid) 
  base_sd_sim[j,2] <- fixed_base_sim$coefficients[1]
  base_sd_sim[j,3] <- confint(fixed_base_sim, 'base_data_model$Z_stud', level = .95)[1,1]
  base_sd_sim[j,4] <- confint(fixed_base_sim, 'base_data_model$Z_stud', level = .95)[1,2]
  base_sd_sim[j,5] <- SATE_sim
  
  #Random Effect Sim
  j <- count+2
  base_sd_sim[j,1] <- "random"
  random_base_sim <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                                      gender + pretest + yearstea + avgtest + teach.edu + (1 | classid), data = base_data_model) 
  base_sd_sim[j,2] <- summary(random_base_sim)$coefficients[2,1]
  base_sd_sim[j,3] <- confint(random_base_sim, 'Z_stud', level = .95)[1,1]
  base_sd_sim[j,4] <- confint(random_base_sim, 'Z_stud', level = .95)[1,2]  
  base_sd_sim[j,5] <- SATE_sim
  
  count <- j+1
  print(i)
}

base_sd_sim$bias <- base_sd_sim$coef - base_sd_sim$SATE

# Histograms for base case sampling distribution
hist(base_sd_sim$coef[base_sd_sim$type == "lr"], main = "Sampling Distribution - Base (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(base_sd_sim$coef)-.5, max = max(base_sd_sim$coef)+.5))
abline(v = mean(base_sd_sim$SATE), col = "red")

hist(base_sd_sim$coef[base_sd_sim$type == "fixed"], main = "Sampling Distribution - Base (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(base_sd_sim$coef)-.5, max = max(base_sd_sim$coef)+.5))
abline(v = mean(base_sd_sim$SATE), col = "red")

hist(base_sd_sim$coef[base_sd_sim$type == "random"], main = "Sampling Distribution - Base (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(base_sd_sim$coef)-.5, max = max(base_sd_sim$coef)+.5))
abline(v = mean(base_sd_sim$SATE), col = "red")

ggplot() +
  geom_histogram(data = base_sd_sim[base_sd_sim$type == "lr",], aes(x = coef), 
                 color = "blue", fill = "white", alpha = .25) +
  geom_histogram(data = base_sd_sim[base_sd_sim$type == "fixed",], aes(x = coef), 
                 color = "green", fill = "white", alpha = .25) +
  geom_histogram(data = base_sd_sim[base_sd_sim$type == "random",], aes(x = coef), 
                 color = "red", fill = "white", alpha = .25) +
  labs(x = "Treatment Effect", y = "Frequency") +
  geom_vline(xintercept=SATE, color = "black", linetype = "dashed", size = 1.25)

# RANDOM EFFECTS VIOLATION SIMULATION #################################################################################

## call random effects dgp and store data frame
re_dat <- re_dat_function(40, 25)

## store SATE and generate data frame for models
SATE_re <- mean(re_dat$Y1) - mean(re_dat$Y0)
model_stu_re <- re_dat %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, 
                                  fam.income, freelunch, dist.school.hour, gender, pretest, classid)

#### Initial Run ######################################################################################################

# Random Effects Violation Linear Regression
lr.re <- lm(Y_stud ~., data = model_stu_re[,-13])

# Random Effects Violation - Linear Regression Model Bias
(lr.re.bias <- lr.re$coefficients[2] - SATE_re)

# Random Effects Violation w/ Fixed Effects
fixed.re <- lfe::felm(model_stu_re$Y_stud ~ model_stu_re$Z_stud + model_stu_re$minority + model_stu_re$parent.edu +
                        model_stu_re$fam.income + model_stu_re$freelunch + model_stu_re$dist.school.hour +
                        model_stu_re$gender + model_stu_re$pretest | model_stu_re$classid) 

#Random Effects Violation - Fixed Effects Model Bias
(fe.re.bias <- fixed.re$coefficients[1] - SATE_re)

#Random Effects Violation w/ Random Effects
random.re <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                              gender + pretest + yearstea + avgtest + teach.edu + (1 | classid), data = model_stu_re)

#Random Effects Violation - Random Effects Model Bias
(re.re.bias <- summary(random.re)$coefficients[2,1] - SATE_re)

#### REV Randomization Distribution ###################################################################################

# initialize results data frame
vre_rd_sim <- data.frame(type = rep(NA, iter*3), 
                         coef = rep(NA, iter*3), 
                         conf_int_low = rep(NA, iter*3), 
                         conf_int_high = rep(NA, iter*3))

# initialize count number for results data frame rows
count <- 1

for(i in 1:iter){
  
  # Randomization Distribution - randomize treatment
  base_data <- re_dat
  N <- nrow(base_data)
  
  X_stud <- rnorm(N, 0, 1)
  prob_stud <- inv.logit((X_stud/max(abs(X_stud))) * log(19))
  Z_stud <- rbinom(N, 1, prob = prob_stud)
  
  base_data$Z_stud <- Z_stud
  base_data$Y_stud <- ifelse(Z_stud == 1, base_data$Y1, base_data$Y0)
  
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, 
                                          fam.income, freelunch, dist.school.hour, gender, pretest, classid)
  
  # Linear Regression Sim
  j <- count
  vre_rd_sim[j,1] <- "lr"
  lr_vre_sim <- lm(Y_stud ~., data = base_data_model[,-13])
  vre_rd_sim[j,2] <- lr_vre_sim$coefficients[2]
  vre_rd_sim[j,3] <- confint(lr_vre_sim, 'Z_stud', level = .95)[1,1]
  vre_rd_sim[j,4] <- confint(lr_vre_sim, 'Z_stud', level = .95)[1,2]
  
  # Fixed Effect Sim
  j <- count+1
  vre_rd_sim[j,1] <- "fixed"
  fixed_vre_sim <- lfe::felm(base_data_model$Y_stud ~ base_data_model$Z_stud + base_data_model$minority +
                               base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch +
                               base_data_model$dist.school.hour + base_data_model$gender + base_data_model$pretest |
                               base_data_model$classid) 
  vre_rd_sim[j,2] <- fixed_vre_sim$coefficients[1]
  vre_rd_sim[j,3] <- confint(fixed_vre_sim, 'base_data_model$Z_stud', level = .95)[1,1]
  vre_rd_sim[j,4] <- confint(fixed_vre_sim, 'base_data_model$Z_stud', level = .95)[1,2]
  
  # Random Effect Sim
  j <- count+2
  vre_rd_sim[j,1] <- "random"
  random_vre_sim <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                                     gender + pretest + (1 | classid), data = base_data_model) 
  vre_rd_sim[j,2] <- summary(random_vre_sim)$coefficients[2,1]
  vre_rd_sim[j,3] <- confint(random_vre_sim, 'Z_stud', level = .95)[1,1]
  vre_rd_sim[j,4] <- confint(random_vre_sim, 'Z_stud', level = .95)[1,2]  
  
  count <- j+1
  print(i)
}

# add random effects violation SATE to results data frame
vre_rd_sim$SATE <- rep(SATE_re, nrow(vre_rd_sim))
vre_rd_sim$bias <- vre_rd_sim$coef - vre_rd_sim$SATE

# histograms for violation of random effects assumption
## linear regression
hist(vre_rd_sim$coef[vre_rd_sim$type == "lr"], main = "Randomization Distribution - VRE (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5))
abline(v = SATE_re, col = "red")

## fixed effects
hist(vre_rd_sim$coef[vre_rd_sim$type == "fixed"], main = "Randomization Distribution - VRE (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5))
abline(v = SATE_re, col = "red")

## random effects
hist(vre_rd_sim$coef[vre_rd_sim$type == "random"], main = "Randomization Distribution - VRE (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5))
abline(v = SATE_re, col = "red")

#### REV Sampling Distribution ########################################################################################

# initialize results data frame
vre_sd_sim <- data.frame(type = rep(NA, iter*3), 
                         coef = rep(NA, iter*3), 
                         conf_int_low = rep(NA, iter*3), 
                         conf_int_high = rep(NA, iter*3), 
                         SATE = rep(NA, iter*3))

# initialize count number for results data frame rows
count <- 1

for(i in 1:iter){
  
  #Sampling Distribution - resample data
  base_data <- re_dat_function(40, 25)
  SATE_sim <- mean(base_data$Y1) - mean(base_data$Y0)
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, 
                                          fam.income, freelunch, dist.school.hour, gender, pretest, classid)
  
  #Linear Regression Sim
  j <- count
  vre_sd_sim[j,1] <- "lr"
  lr_vre_sim <- lm(Y_stud ~., data = base_data_model[,-13])
  vre_sd_sim[j,2] <- lr_vre_sim$coefficients[2]
  vre_sd_sim[j,3] <- confint(lr_vre_sim, 'Z_stud', level = .95)[1,1]
  vre_sd_sim[j,4] <- confint(lr_vre_sim, 'Z_stud', level = .95)[1,2]
  vre_sd_sim[j,5] <- SATE_sim
  
  #Fixed Effect Sim
  j <- count+1
  vre_sd_sim[j,1] <- "fixed"
  fixed_vre_sim <- lfe::felm(base_data_model$Y_stud ~ base_data_model$Z_stud + base_data_model$minority +
                               base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch +
                               base_data_model$dist.school.hour + base_data_model$gender + base_data_model$pretest |
                               base_data_model$classid) 
  vre_sd_sim[j,2] <- fixed_vre_sim$coefficients[1]
  vre_sd_sim[j,3] <- confint(fixed_vre_sim, 'base_data_model$Z_stud', level = .95)[1,1]
  vre_sd_sim[j,4] <- confint(fixed_vre_sim, 'base_data_model$Z_stud', level = .95)[1,2]
  vre_sd_sim[j,5] <- SATE_sim
  
  #Random Effect Sim
  j <- count+2
  vre_sd_sim[j,1] <- "random"
  random_vre_sim <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                                     gender + pretest + (1 | classid), data = base_data_model) 
  vre_sd_sim[j,2] <- summary(random_vre_sim)$coefficients[2,1]
  vre_sd_sim[j,3] <- confint(random_vre_sim, 'Z_stud', level = .95)[1,1]
  vre_sd_sim[j,4] <- confint(random_vre_sim, 'Z_stud', level = .95)[1,2]  
  vre_sd_sim[j,5] <- SATE_sim
  
  count <- j+1
  print(i)
}

vre_sd_sim$bias <- vre_sd_sim$coef - vre_sd_sim$SATE

# histograms for sampling distribution - random effects violation
## linear regression
hist(vre_sd_sim$coef[vre_sd_sim$type == "lr"], main = "Sampling Distribution - VRE (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(vre_sd_sim$coef)-.5, max = max(vre_sd_sim$coef)+.5))
abline(v = mean(vre_sd_sim$SATE), col = "red")

## fixed effects
hist(vre_sd_sim$coef[vre_sd_sim$type == "fixed"], main = "Sampling Distribution - VRE (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_sd_sim$coef)-.5, max = max(vre_sd_sim$coef)+.5))
abline(v = mean(vre_sd_sim$SATE), col = "red")

## random effects
hist(vre_sd_sim$coef[vre_sd_sim$type == "random"], main = "Sampling Distribution - VRE (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_sd_sim$coef)-.5, max = max(vre_sd_sim$coef)+.5))
abline(v = mean(vre_sd_sim$SATE), col = "red")

ggplot() +
  geom_histogram(data = vre_sd_sim[vre_sd_sim$type == "lr",], aes(x = coef), color = "blue", fill = "white", alpha = .25) +
  geom_histogram(data = vre_sd_sim[vre_sd_sim$type == "fixed",], aes(x = coef), color = "green", fill = "white", alpha = .25) +
  geom_histogram(data = vre_sd_sim[vre_sd_sim$type == "random",], aes(x = coef), color = "red", fill = "white", alpha = .25) +
  labs(x = "Treatment Effect", y = "Frequency") +
  geom_vline(xintercept=mean(vre_sd_sim$SATE), color = "black", linetype = "dashed", size = 1.25)

# VIOLATE IGNORABILITY SIMULATION ####################################################################################

## call ignorability dgp and store data frame
ig_dat <- ig_dat_function(40, 25)

## store SATE and generate data frame for models
SATE_ig <- mean(ig_dat$Y1) - mean(ig_dat$Y0)
model_ig <- ig_dat %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest,
                              minority, parent.edu, fam.income, 
                              freelunch, gender, pretest, classid)

#### Initial Run ######################################################################################################

# Ignorability Violation - Linear Regression
lr.ig <- lm(Y_stud ~., data = model_ig[,-12])

# Ignorability Violation - Linear Regression Model Bias
(lr.ig.bias <- lr.ig$coefficients[2] - SATE_ig)

# Ignorability Violation w/ Fixed Effects
fixed.ig <- lfe::felm(model_ig$Y_stud ~ model_ig$Z_stud + model_ig$minority + model_ig$parent.edu + 
                        model_ig$fam.income + model_ig$freelunch + model_ig$gender + model_ig$pretest | model_ig$classid)

# Ignorability Violation - Fixed Effects Model Bias
(fe.ig.bias <- fixed.base$coefficients[1] - SATE_ig)

# Ignorability Violation w/ Random Effects
random.ig <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + gender + pretest + 
                              yearstea + avgtest + teach.edu + (1 | classid), data = model_ig)

# Ignorability Violation - Random Effects Model Bias
(re.ig.bias <- summary(random.ig)$coefficients[2,1] - SATE_ig)

#### Ignorability Violation Randomization Distribution #################################################################

# initialize results data frame
ig_rd_sim <- data.frame(type = rep(NA, iter*3), 
                        coef = rep(NA, iter*3), 
                        conf_int_low = rep(NA, iter*3), 
                        conf_int_high = rep(NA, iter*3))

# initialize count number for results data frame rows
count <- 1

for(i in 1:iter){
  
  # Randomization Distribution - randomize treatment
  base_data <- ig_dat
  N <- nrow(base_data)
  
  # probability of assignment is based on pretest scores 
  low_count <- sum(base_data$pretest <= 65)
  high_count <- length(base_data$pretest) - low_count
  low_score <- sample(c(rep(0,round(.3*low_count,0)),rep(1,low_count - round(.3*low_count,0))), low_count) 
  # higher probability (70%) of treatment if test score is <= 65 
  high_score <- sample(c(rep(0,round(.5*high_count,0)), rep(1, high_count -round(.5*high_count,0))), high_count) 
  # equal probability of treatment if test score is > 65 
  base_data$Z_stud <- rep(NA,N)
  base_data$Z_stud[base_data$pretest > 65] <- high_score
  base_data$Z_stud[base_data$pretest <= 65] <- low_score
  
  base_data$Y_stud <- ifelse(base_data$Z_stud==1, base_data$Y1, base_data$Y0)
  
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, 
                                          avgtest, minority, parent.edu, fam.income, 
                                          freelunch, gender, pretest, classid)
  
  # Linear Regression Sim
  j <- count
  ig_rd_sim[j,1] <- "lr"
  lr_ig_sim <- lm(Y_stud ~., data = base_data_model[,-12])
  ig_rd_sim[j,2] <- lr_ig_sim$coefficients[2]
  ig_rd_sim[j,3] <- confint(lr_ig_sim, 'Z_stud', level = .95)[1,1]
  ig_rd_sim[j,4] <- confint(lr_ig_sim, 'Z_stud', level = .95)[1,2]
  
  # Fixed Effect Sim
  j <- count+1
  ig_rd_sim[j,1] <- "fixed"
  fixed_ig_sim <- lfe::felm(base_data_model$Y_stud ~ base_data_model$Z_stud + base_data_model$minority + 
                              base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch + 
                              base_data_model$gender + base_data_model$pretest | base_data_model$classid)
  ig_rd_sim[j,2] <- fixed_ig_sim$coefficients[1]
  ig_rd_sim[j,3] <- confint(fixed_ig_sim, 'base_data_model$Z_stud', level = .95)[1,1]
  ig_rd_sim[j,4] <- confint(fixed_ig_sim, 'base_data_model$Z_stud', level = .95)[1,2]
  
  # Random Effect Sim
  j <- count+2
  ig_rd_sim[j,1] <- "random"
  random_ig_sim <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + gender + pretest + 
                                    yearstea + avgtest + teach.edu + (1 | classid), data = base_data_model)
  ig_rd_sim[j,2] <- summary(random_ig_sim)$coefficients[2,1]
  ig_rd_sim[j,3] <- confint(random_ig_sim, 'Z_stud', level = .95)[1,1]
  ig_rd_sim[j,4] <- confint(random_ig_sim, 'Z_stud', level = .95)[1,2]  
  
  count <- j+1
  print(i)
}

# add random effects violation SATE to results data frame
ig_rd_sim$SATE <- rep(SATE_ig, nrow(ig_rd_sim))
ig_rd_sim$bias <- ig_rd_sim$coef - ig_rd_sim$SATE

# histograms for violation of ignorability assumption
## linear regression
hist(ig_rd_sim$coef[ig_rd_sim$type == "lr"], main = "Randomization Distribution - IV (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(ig_rd_sim$coef)-.5, max = max(ig_rd_sim$coef)+.5))
abline(v = SATE_ig, col = "red")

## fixed effects
hist(ig_rd_sim$coef[ig_rd_sim$type == "fixed"], main = "Randomization Distribution - IV (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(ig_rd_sim$coef)-.5, max = max(ig_rd_sim$coef)+.5))
abline(v = SATE_ig, col = "red")

## random effects
hist(ig_rd_sim$coef[ig_rd_sim$type == "random"], main = "Randomization Distribution - IV (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(ig_rd_sim$coef)-.5, max = max(ig_rd_sim$coef)+.5))
abline(v = SATE_ig, col = "red")

#### Ignorability Violation Sampling Distribution ######################################################################

# Generate empty data frame for results
ig_sd_sim <- data.frame(type = rep(NA, iter*3), coef = rep(NA, iter*3), 
                        conf_int_low = rep(NA, iter*3), conf_int_high = rep(NA, iter*3), SATE = rep(NA, iter*3))

# Initialize counts which is used for which row to store outputs in
count <- 1

for(i in 1:iter){
  
  #Sampling Distribution - resample data
  base_data <- ig_dat_function(40, 25)
  SATE_sim <- mean(base_data$Y1) - mean(base_data$Y0)
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, 
                                          minority, parent.edu, fam.income, 
                                          freelunch, gender, pretest, classid)
  
  #Linear Regression Sim
  j <- count
  ig_sd_sim[j,1] <- "lr"
  lr_ig_sim <- lm(Y_stud ~., data = base_data_model[,-12])
  ig_sd_sim[j,2] <- lr_ig_sim$coefficients[2]
  ig_sd_sim[j,3] <- confint(lr_ig_sim, 'Z_stud', level = .95)[1,1]
  ig_sd_sim[j,4] <- confint(lr_ig_sim, 'Z_stud', level = .95)[1,2]
  ig_sd_sim[j,5] <- SATE_sim
  
  #Fixed Effect Sim
  j <- count+1
  ig_sd_sim[j,1] <- "fixed"
  fixed_ig_sim <- lfe::felm(base_data_model$Y_stud ~ base_data_model$Z_stud + base_data_model$minority + 
                              base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch + 
                              base_data_model$gender + base_data_model$pretest | base_data_model$classid)
  ig_sd_sim[j,2] <- fixed_ig_sim$coefficients[1]
  ig_sd_sim[j,3] <- confint(fixed_ig_sim, 'base_data_model$Z_stud', level = .95)[1,1]
  ig_sd_sim[j,4] <- confint(fixed_ig_sim, 'base_data_model$Z_stud', level = .95)[1,2]
  ig_sd_sim[j,5] <- SATE_sim
  
  #Random Effect Sim
  j <- count+2
  ig_sd_sim[j,1] <- "random"
  random_ig_sim <- lmerTest::lmer(Y_stud ~ Z_stud + minority + parent.edu + fam.income + freelunch + gender + pretest + 
                                    yearstea + avgtest + teach.edu + (1 | classid), data = base_data_model)
  ig_sd_sim[j,2] <- summary(random_ig_sim)$coefficients[2,1]
  ig_sd_sim[j,3] <- confint(random_ig_sim, 'Z_stud', level = .95)[1,1]
  ig_sd_sim[j,4] <- confint(random_ig_sim, 'Z_stud', level = .95)[1,2]  
  ig_sd_sim[j,5] <- SATE_sim
  
  count <- j+1
  print(i)
}

ig_sd_sim$bias <- ig_sd_sim$coef - ig_sd_sim$SATE

# histograms for violation of ignorability assumption
## linear regression
hist(ig_sd_sim$coef[ig_sd_sim$type == "lr"], main = "Randomization Distribution - IV (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(ig_sd_sim$coef)-.5, max = max(ig_sd_sim$coef)+.5))
abline(v = mean(ig_sd_sim$SATE), col = "red")

## fixed effects
hist(ig_sd_sim$coef[ig_sd_sim$type == "fixed"], main = "Randomization Distribution - IV (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(ig_sd_sim$coef)-.5, max = max(ig_sd_sim$coef)+.5))
abline(v = mean(ig_sd_sim$SATE), col = "red")

## random effects
hist(ig_sd_sim$coef[ig_sd_sim$type == "random"], main = "Randomization Distribution - IV (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(ig_sd_sim$coef)-.5, max = max(ig_sd_sim$coef)+.5))
abline(v = mean(ig_sd_sim$SATE), col = "red")

# TREATMENT AT GROUP LEVEL SIMULATION ###############################################################################
#### Initial Run ######################################################################################################

# Treatment Effect of the Treated @ Group Level
model_class <- classroom_dat %>% select(Y_class, Z_class, yearstea, teach.edu, avgtest, minority, parent.edu,
                                        fam.income, freelunch, dist.school.hour, gender, pretest, classid)

# Group Level Treatment Effect Linear Regression # ignore group structure
lr.gl <- lm(Y_class ~., data = model_class[,-13])

# Group Level Treatment Effect - Linear Regression Model Bias
(lr.gl.bias <- lr.gl$coefficients[2] - SATE)

# Group Level Treatment Effect w/ Fixed Effects
fixed.gl <- lfe::felm(model_class$Y_class ~ model_class$Z_class + model_class$minority + model_class$parent.edu +
                        model_class$fam.income + model_class$freelunch + model_class$dist.school.hour +
                        model_class$gender + model_class$pretest | model_class$classid)

# Group Level Treatment Effect - Fixed Effects Model Bias
## Z_class does not return coeffient - I think this makes sense since we're running the regression at the class level?
# (fe.gl.bias <- fixed.gl$coefficients[1] - SATE)

# Group Level Treatment Effect w/ Random Effects
random.gl <- lmerTest::lmer(Y_class ~ Z_class + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                              gender + pretest + yearstea + avgtest + teach.edu + (1 | classid), data = model_class)

# Group Level Treatment Effect - Random Effects Model Bias
(re.gl.bias <- summary(random.gl)$coefficients[2,1] - SATE)

#### Group Level Randomization Distribution ######################################################################

# initialize results data frame
gl_rd_sim <- data.frame(type = rep(NA, iter*3), 
                        coef = rep(NA, iter*3), 
                        conf_int_low = rep(NA, iter*3), 
                        conf_int_high = rep(NA, iter*3))

# initialize count number for results data frame rows
count <- 1

for(i in 1:iter){
  
  base_data <- classroom_dat
  
  # Randomization Distribution - randomize treatment
  avg_tea <- data.frame(classid = base_data$classid, avgtest = base_data$avgtest)
  avg_tea <- unique(avg_tea)
  X_class <- rnorm(length(unique(base_data$classid)), 1, 1) - (.03*avg_tea$avgtest)
  
  # correlate classroom treatment with classroom level predictor
  prob_class <- inv.logit((X_class/max(abs(X_class)))*log(19))
  Z_class <- rbinom(unique(base_data$classid), 1, prob = prob_class)
  Z_class <- rep(Z_class, each = (length(unique(base_data$studentid))/length(unique(base_data$classid))))
  
  base_data$Z_class <- Z_class
  base_data$Y_class <- ifelse(Z_class == 1, base_data$Y1, base_data$Y0)
  
  base_data_model <- base_data %>% select(Y_class, Z_class, yearstea, teach.edu, avgtest, minority, parent.edu,
                                          fam.income, freelunch, dist.school.hour, gender, pretest, classid)
  
  # Linear Regression Sim
  j <- count
  gl_rd_sim[j,1] <- "lr"
  lr_gl_sim <- lm(Y_class ~., data = base_data_model[,-13])
  gl_rd_sim[j,2] <- lr_gl_sim$coefficients[2]
  gl_rd_sim[j,3] <- confint(lr_gl_sim, 'Z_class', level = .95)[1,1]
  gl_rd_sim[j,4] <- confint(lr_gl_sim, 'Z_class', level = .95)[1,2]
  
  # Fixed Effect Sim
  j <- count+1
  gl_rd_sim[j,1] <- "fixed"
  fixed_gl_sim <- lfe::felm(base_data_model$Y_class ~ base_data_model$Z_class + base_data_model$minority + 
                              base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch + 
                              base_data_model$dist.school.hour + base_data_model$gender + 
                              base_data_model$pretest | base_data_model$classid)
  gl_rd_sim[j,2] <- fixed_gl_sim$coefficients[1]
  gl_rd_sim[j,3] <- confint(fixed_gl_sim, 'base_data_model$Z_class', level = .95)[1,1]
  gl_rd_sim[j,4] <- confint(fixed_gl_sim, 'base_data_model$Z_class', level = .95)[1,2]
  
  # Random Effect Sim
  j <- count+2
  gl_rd_sim[j,1] <- "random"
  random_gl_sim <- lmerTest::lmer(Y_class ~ Z_class + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                                    gender + pretest + yearstea + avgtest + teach.edu + (1 | classid), data = base_data_model)
  gl_rd_sim[j,2] <- summary(random_gl_sim)$coefficients[2,1]
  gl_rd_sim[j,3] <- confint(random_gl_sim, 'Z_class', level = .95)[1,1]
  gl_rd_sim[j,4] <- confint(random_gl_sim, 'Z_class', level = .95)[1,2]  
  
  count <- j+1
  print(i)
}

# add random effects violation SATE to results data frame
gl_rd_sim$SATE <- rep(SATE, nrow(gl_rd_sim))
gl_rd_sim$bias <- gl_rd_sim$coef - gl_rd_sim$SATE

# histograms for group level treatment effect
## linear regression
hist(gl_rd_sim$coef[gl_rd_sim$type == "lr"], main = "Randomization Distribution - IV (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef)-.5, max = max(gl_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

## fixed effects
hist(gl_rd_sim$coef[gl_rd_sim$type == "fixed"], main = "Randomization Distribution - IV (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef)-.5, max = max(gl_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

## random effects
hist(gl_rd_sim$coef[gl_rd_sim$type == "random"], main = "Randomization Distribution - IV (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef)-.5, max = max(gl_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

#### Group Level Sampling Distribution ######################################################################

# Generate empty data frame for results
gl_sd_sim <- data.frame(type = rep(NA, iter*3), coef = rep(NA, iter*3), 
                        conf_int_low = rep(NA, iter*3), conf_int_high = rep(NA, iter*3), SATE = rep(NA, iter*3))

# Initialize counts which is used for which row to store outputs in
count <- 1

for(i in 1:iter){
  
  #Sampling Distribution - resample data
  base_data <- class_dat_function(40, 25)
  SATE_sim <- mean(base_data$Y1) - mean(base_data$Y0)
  base_data_model <- base_data %>% select(Y_class, Z_class, yearstea, teach.edu, avgtest, minority, parent.edu,
                                          fam.income, freelunch, dist.school.hour, gender, pretest, classid)
  
  #Linear Regression Sim
  j <- count
  gl_sd_sim[j,1] <- "lr"
  lr_gl_sim <- lm(Y_class ~., data = base_data_model[,-13])
  gl_sd_sim[j,2] <- lr_gl_sim$coefficients[2]
  gl_sd_sim[j,3] <- confint(lr_gl_sim, 'Z_class', level = .95)[1,1]
  gl_sd_sim[j,4] <- confint(lr_gl_sim, 'Z_class', level = .95)[1,2]
  gl_sd_sim[j,5] <- SATE_sim
  
  #Fixed Effect Sim
  j <- count+1
  gl_sd_sim[j,1] <- "fixed"
  fixed_gl_sim <- lfe::felm(base_data_model$Y_class ~ base_data_model$Z_class + base_data_model$minority + 
                              base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch + 
                              base_data_model$dist.school.hour + base_data_model$gender + 
                              base_data_model$pretest | base_data_model$classid)
  gl_sd_sim[j,2] <- fixed_gl_sim$coefficients[1]
  gl_sd_sim[j,3] <- confint(fixed_gl_sim, 'base_data_model$Z_class', level = .95)[1,1]
  gl_sd_sim[j,4] <- confint(fixed_gl_sim, 'base_data_model$Z_class', level = .95)[1,2]
  gl_sd_sim[j,5] <- SATE_sim
  
  #Random Effect Sim
  j <- count+2
  gl_sd_sim[j,1] <- "random"
  random_gl_sim <- lmerTest::lmer(Y_class ~ Z_class + minority + parent.edu + fam.income + freelunch + dist.school.hour +
                                    gender + pretest + yearstea + avgtest + teach.edu + (1 | classid), data = base_data_model) 
  gl_sd_sim[j,2] <- summary(random_gl_sim)$coefficients[2,1]
  gl_sd_sim[j,3] <- confint(random_gl_sim, 'Z_class', level = .95)[1,1]
  gl_sd_sim[j,4] <- confint(random_gl_sim, 'Z_class', level = .95)[1,2]  
  gl_sd_sim[j,5] <- SATE_sim
  
  count <- j+1
  print(i)
}

gl_sd_sim$bias <- gl_sd_sim$coef - gl_sd_sim$SATE

# histograms for group level treatment effect
## linear regression
hist(gl_sd_sim$coef[gl_sd_sim$type == "lr"], main = "Randomization Distribution - IV (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(gl_sd_sim$coef)-.5, max = max(gl_sd_sim$coef)+.5))
abline(v = mean(gl_sd_sim$SATE), col = "red")

## fixed effects
hist(gl_sd_sim$coef[gl_sd_sim$type == "fixed"], main = "Randomization Distribution - IV (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_sd_sim$coef)-.5, max = max(gl_sd_sim$coef)+.5))
abline(v = mean(gl_sd_sim$SATE), col = "red")

## random effects
hist(gl_sd_sim$coef[gl_sd_sim$type == "random"], main = "Randomization Distribution - IV (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_sd_sim$coef)-.5, max = max(gl_sd_sim$coef)+.5))
abline(v = mean(gl_sd_sim$SATE), col = "red")



# BIAS COMPARISON ###################################################################################################

bias_compare_initial <- data.frame("Bias Comparison" = c("Linear Regression", "Fixed Effects", "Random Effects"),
                           "Base Case" = round(c(lr.base.bias, fe.base.bias, re.base.bias),4),
                           "RE Violation" = round(c(lr.re.bias, fe.re.bias, re.re.bias),4),
                           "Ig. Violation" = round(c(lr.ig.bias, fe.ig.bias, re.ig.bias),4),
                           "Group Treatment" = c(round(lr.gl.bias,4), "N/A", round(re.gl.bias,4)))

grid.arrange(tableGrob(bias_compare_initial, theme=ttheme_default()), nrow = 1)


bias_compare_sim <- data.frame("Bias Comparison" = c("Linear Regression", "Fixed Effects", "Random Effects"), 
                           "Base Case" = round(c(mean(base_rd_sim$bias[base_rd_sim$type == "lr"]), 
                                                 mean(base_rd_sim$bias[base_rd_sim$type == "fixed"]), 
                                                 mean(base_rd_sim$bias[base_rd_sim$type == "random"])),4), 
                           "RE Violation" = round(c(mean(vre_rd_sim$bias[vre_rd_sim$type == "lr"]), 
                                                    mean(vre_rd_sim$bias[vre_rd_sim$type == "fixed"]), 
                                                    mean(vre_rd_sim$bias[vre_rd_sim$type == "random"])),4), 
                           "Ig. Violation" = round(c(mean(ig_rd_sim$bias[ig_rd_sim$type=="lr"]), 
                                                     mean(ig_rd_sim$bias[ig_rd_sim$type=="fixed"]), 
                                                     mean(ig_rd_sim$bias[ig_rd_sim$type=="random"])),4), 
                           "Group Treatment" = c(round(mean(gl_rd_sim$bias[gl_rd_sim$type == "lr"]),4), 
                                                 "N/A", 
                                                 round(mean(gl_rd_sim$bias[gl_rd_sim$type == "random"]),4)))

grid.arrange(tableGrob(bias_compare_sim, theme=ttheme_default()), nrow = 1)


