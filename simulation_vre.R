library(lfe)
library(lme4)
library(lmerTest)

# Source data generation process functions
source("dgp_script.R") 

# Set iteration number for simulations
iter <- 1000

# Set seed for reproducibility
set.seed(2123)

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

vre_lr_rd_sim <- vre_rd_sim %>% filter(type == "lr")
vre_fixed_rd_sim <- vre_rd_sim %>% filter(type == "fixed")
vre_random_rd_sim <- vre_rd_sim %>% filter(type == "random")

write_csv(vre_lr_rd_sim, "output/vre_lr_rd_sim.csv")
write_csv(vre_fixed_rd_sim, "output/vre_fixed_rd_sim.csv")
write_csv(vre_random_rd_sim, "output/vre_random_rd_sim.csv")
write_csv(vre_rd_sim, "output/vre_full_rd_sim.csv")


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

# Create separate dataframes for the different models
vre_lr_sd_sim <- vre_sd_sim %>% filter(type == "lr")
vre_fixed_sd_sim <- vre_sd_sim %>% filter(type == "fixed")
vre_random_sd_sim <- vre_sd_sim %>% filter(type == "random")

write_csv(vre_lr_sd_sim, "output/vre_lr_d_sim.csv")
write_csv(vre_fixed_sd_sim, "output/vre_fixed_sd_sim.csv")
write_csv(vre_random_sd_sim, "output/vre_random_d_sim.csv")
write_csv(vre_sd_sim, "output/vre_full_sd_sim.csv")

