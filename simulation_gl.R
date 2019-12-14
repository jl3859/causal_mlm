library(lfe)
library(lme4)
library(lmerTest)

# Source data generation process functions
source("dgp_script.R") 

# Set iteration number for simulations
iter <- 100

# Set seed for reproducibility
set.seed(2020)

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
gl_rd_sim <- data.frame(type = rep(NA, iter*3), coef = rep(NA, iter*3), conf_int_low = rep(NA, iter*3), conf_int_high = rep(NA, iter*3))

# initialize count number for results data frame rows
count <- 1

for(i in 1:iter){
  
  base_data <- classroom_dat
  
  # Randomization Distribution - randomize treatment
  avg_tea <- data.frame(classid = base_data$classid, avgtest = base_data$avgtest)
  avg_tea <- unique(avg_tea)
  X_class <- rnorm(unique(base_data$classid), 0, 1) - (.03*avg_tea$avgtest) 
  # correlate classroom treatment with classroom level predictor
  prob_class <- inv.logit((X_class/max(abs(X_class)))*log(19))
  Z_class <- rbinom(unique(base_data$classid), 1, prob = prob_class)
  Z_class <- rep(Z_class, each = unique(base_data$studentid)/unique(base_data$classid))
  
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

# Create separate dataframes for the different models
gl_lr_sd_sim <- gl_rd_sim %>% filter(type == "lr")
gl_fixed_sd_sim <- gl_rd_sim %>% filter(type == "fixed")
gl_random_sd_sim <- gl_rd_sim %>% filter(type == "random")


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
  gl_sd_sim[j,2] <- lr_base_sim$coefficients[2]
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

# Create separate dataframes for the different models
gl_lr_sd_sim <- gl_sd_sim %>% filter(type == "lr")
gl_fixed_sd_sim <- gl_sd_sim %>% filter(type == "fixed")
gl_random_sd_sim <- gl_sd_sim %>% filter(type == "random")







