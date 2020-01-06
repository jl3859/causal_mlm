library(lfe)
library(lme4)
library(lmerTest)

# Source data generation process functions
source("dgp_script.R") 

# Set iteration number for simulations
iter <- 1000

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

# Create separate dataframes for the different models
base_lr_rd_sim <- base_rd_sim %>% filter(type == "lr")
base_fixed_rd_sim <- base_rd_sim %>% filter(type == "fixed")
base_random_rd_sim <- base_rd_sim %>% filter(type == "random")

write_csv(base_lr_rd_sim, "output/base_lr_rd_sim.csv")
write_csv(base_fixed_rd_sim, "output/base_fixed_rd_sim.csv")
write_csv(base_random_rd_sim, "output/base_random_rd_sim.csv")
write_csv(base_rd_sim, "output/base_full_rd_sim.csv")


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

# Create separate dataframes for the different models
base_lr_sd_sim <- base_sd_sim %>% filter(type == "lr")
base_fixed_sd_sim <- base_sd_sim %>% filter(type == "fixed")
base_random_sd_sim <- base_sd_sim %>% filter(type == "random")

write_csv(base_lr_sd_sim, "output/base_lr_sd_sim.csv")
write_csv(base_fixed_sd_sim, "output/base_fixed_sd_sim.csv")
write_csv(base_random_sd_sim, "output/base_random_sd_sim.csv")
write_csv(base_sd_sim, "output/base_full_sd_sim.csv")
