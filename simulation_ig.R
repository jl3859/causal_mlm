library(lfe)
library(lme4)
library(lmerTest)

# Source data generation process functions
source("dgp_script.R") 

# Set iteration number for simulations
iter <- 100

# Set seed for reproducibility
set.seed(2020)


# VIOLATE IGNORABILITY SIMULATION ####################################################################################

## call ignorability dgp and store data frame
ig_dat <- ig_dat_function(40, 25)

## store SATE and generate data frame for models
SATE_ig <- mean(ig_dat$Y1) - mean(ig_dat$Y0)
model_ig <- ig_dat %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, fam.income, 
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
ig_rd_sim <- data.frame(type = rep(NA, iter*3), coef = rep(NA, iter*3), conf_int_low = rep(NA, iter*3), conf_int_high = rep(NA, iter*3))

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
  
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, fam.income, 
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
                              base_data_model$gender + base_data_model$pretest | model_ig$classid)
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

ig_lr_sd_sim <- ig_rd_sim %>% filter(type == "lr")
ig_fixed_sd_sim <- ig_rd_sim %>% filter(type == "fixed")
ig_random_sd_sim <- ig_rd_sim %>% filter(type == "random")



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
  base_data_model <- base_data %>% select(Y_stud, Z_stud, yearstea, teach.edu, avgtest, minority, parent.edu, fam.income, 
                                          freelunch, gender, pretest, classid)
  
  #Linear Regression Sim
  j <- count
  ig_sd_sim[j,1] <- "lr"
  lr_ig_sim <- lm(Y_stud ~., data = base_data_model[,-13])
  ig_sd_sim[j,2] <- lr_base_sim$coefficients[2]
  ig_sd_sim[j,3] <- confint(lr_ig_sim, 'Z_stud', level = .95)[1,1]
  ig_sd_sim[j,4] <- confint(lr_ig_sim, 'Z_stud', level = .95)[1,2]
  ig_sd_sim[j,5] <- SATE_sim
  
  #Fixed Effect Sim
  j <- count+1
  ig_sd_sim[j,1] <- "fixed"
  fixed_ig_sim <- lfe::felm(base_data_model$Y_stud ~ base_data_model$Z_stud + base_data_model$minority + 
                              base_data_model$parent.edu + base_data_model$fam.income + base_data_model$freelunch + 
                              base_data_model$gender + base_data_model$pretest | model_ig$classid)
  ig_sd_sim[j,2] <- fixed_base_sim$coefficients[1]
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
