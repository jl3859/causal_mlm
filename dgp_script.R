### load libraries needed for data generating process 
library(tidyverse)

### Inverse logit function to assign probabilities later
inv.logit <- function(X) exp(X)/(1 + exp(X))

### Basic set up: This function also assigns treatment at the individual level and at the group level
class_dat_function <- function(classrooms, students){
  # classroom level covariates
  yearstea <- abs(rep(round(rnorm(classrooms, 5, 5),0), each = students)) # trying to get a range between 0-20
  teach.edu <- rep(sample(c(rep(1,classrooms*.725), rep(2,classrooms*.25), rep(3,classrooms*.025))), each = students) # 
  avgtest <- rep(round(rnorm(classrooms, 72, 8),2), each = students) + 0.75*yearstea #correlated with teachers
  classroom_score <- rep(rnorm(classrooms, 0, 3), each = students) 
  
  # student level covariates
  N <- classrooms*students #number of total students
  
  minority <- sample(c(0:1), N, replace = T, prob = c(0.65, 0.35)) # 65% white students, 35% minority students
  parent.edu <- ifelse(minority == 1, sample(c(0,1,2,3,4), N, replace = T, prob = c(.40, .30, .24, .05, .01)),
                       sample(c(0,1,2,3,4), N, replace = T, prob =c(.17, .26, .40, .10, .07))) # conditional if the student is a minority
  fam.income <- abs(round(abs(rnorm(N, 50, 20)) + ifelse(parent.edu > 2, 15*parent.edu, 0) + ifelse(parent.edu == 2, 6*parent.edu, 0) + 
                            ifelse(minority == 1, rnorm(N, -7, 1), 0),3)) #correlated with parent education and minority 
  freelunch <- ifelse(fam.income < 46.635, 1, 0) # free lunch if family income is less than ~46.6K 
  dist.school.hour <- round(abs(rnorm(N, 20, 4)),0)/60 # indepdent 
  gender <- sample(c(0:1), N, replace = T, prob = c(0.50, 0.50)) # independent
  pretest <-  61 + 0.4*yearstea + 0.3*teach.edu + 0.71*parent.edu + 0.15*fam.income - 2*minority - 1.5*dist.school.hour + rnorm(N, 1, 7)
  
  # random probability of assignment at student-level and classroom-level 
  X_stud <- rnorm(N, 0, 1)
  prob_stud <- inv.logit((X_stud/max(abs(X_stud))) * log(19))
  Z_stud <- rbinom(N, 1, prob = prob_stud)
  
  # random probability of assignment at classroom-level 
  avg_tea <- data.frame(classid = rep(1:classrooms, each = students), avgtest = avgtest)
  avg_tea <- unique(avg_tea)
  X_class <- rnorm(classrooms, 0, 1) - (.03*avg_tea$avgtest) # correlate classroom treatment with classroom level predictor
  prob_class <- inv.logit((X_class/max(abs(X_class)))*log(19))
  Z_class <- rbinom(classrooms, 1, prob = prob_class)
  Z_class <- rep(Z_class, each = students)
  
  # outcomes (using both classroom-level, student-level predictors, and the random effect)
  Y0 = 56 + 0 + 0.4*yearstea + 0.33*teach.edu + 0.74*parent.edu + 0.18*fam.income - 2.23*minority - 1.4*dist.school.hour + classroom_score +  rnorm(N, 0 ,1)
  Y1 = 56 + 5 + 0.4*yearstea + 0.33*teach.edu + 0.74*parent.edu + 0.18*fam.income - 2.23*minority - 1.4*dist.school.hour + classroom_score +  rnorm(N, 0 ,1)
  
  # observed outcome based on treatment 
  Y_stud <- ifelse(Z_stud == 1, Y1, Y0) #totally random
  Y_class <- ifelse(Z_class == 1, Y1, Y0) #group-level randomization
  # create dataset
  classroom_dat <- data.frame(yearstea = yearstea, teach.edu = as.factor(teach.edu), avgtest = avgtest,  
                              minority = as.factor(minority), parent.edu = as.factor(parent.edu), fam.income = fam.income,  
                              freelunch = as.factor(freelunch), dist.school.hour = dist.school.hour, gender = as.factor(gender), 
                              pretest = pretest,classid = as.factor(rep(1:classrooms, each = students)), studentid  = 1:N, 
                              Z_stud = Z_stud, Z_class = Z_class, Y0 = Y0, Y1 = Y1, Y_stud = Y_stud, Y_class = Y_class)
  
  return(classroom_dat)
}

### Violate Randon Effects Assumption: correlate the classroom_score (the random effect with mean classroom pretest scores)

re_dat_function <- function(classrooms, students){
  # classroom level covariates
  yearstea <- abs(rep(round(rnorm(classrooms, 5, 5),0), each = students)) # trying to get a range between 0-20
  teach.edu <- rep(sample(c(rep(1,classrooms*.725), rep(2,classrooms*.25), rep(3,classrooms*.025))), each = students) # 
  avgtest <- rep(round(rnorm(classrooms, 72, 8),2), each = students) + 0.75*yearstea #correlated with teachers
  
  # student level covariates
  N <- classrooms*students #number of total students
  
  minority <- sample(c(0:1), N, replace = T, prob = c(0.65, 0.35)) # 65% white students, 35% minority students
  parent.edu <- ifelse(minority == 1, sample(c(0,1,2,3,4), N, replace = T, prob = c(.40, .30, .24, .05, .01)),
                       sample(c(0,1,2,3,4), N, replace = T, prob =c(.17, .26, .40, .10, .07))) # conditional if the student is a minority
  fam.income <- abs(round(abs(rnorm(N, 50, 20)) + ifelse(parent.edu > 2, 15*parent.edu, 0) + ifelse(parent.edu == 2, 6*parent.edu, 0) + 
                            ifelse(minority == 1, rnorm(N, -7, 1), 0),3)) #correlated with parent education and minority 
  freelunch <- ifelse(fam.income < 46.635, 1, 0) # free lunch if family income is less than ~46.6K 
  dist.school.hour <- round(abs(rnorm(N, 20, 4)),0)/60 # indepdent 
  gender <- sample(c(0:1), N, replace = T, prob = c(0.50, 0.50)) # independent
  pretest <-  61 + 0.4*yearstea + 0.3*teach.edu + 0.71*parent.edu + 0.15*fam.income - 2*minority - 1.5*dist.school.hour + rnorm(N, 1, 7)
  
  # random probability of assignment at student-level and classroom-level 
  X_stud <- rnorm(N, 0, 1)
  prob_stud <- inv.logit((X_stud/max(abs(X_stud))) * log(19))
  Z_stud <- rbinom(N, 1, prob = prob_stud)
  
  # random probability of assignment at classroom-level
  avg_tea <- data.frame(classid = rep(1:classrooms, each = students), avgtest = avgtest)
  avg_tea <- unique(avg_tea)
  X_class <- rnorm(classrooms, 0, 1) - (0.1*avg_tea$avgtest) # correlate classroom treatment with classroom level predictor
  prob_class <- inv.logit((X_class/max(abs(X_class)))*log(19))
  Z_class <- rbinom(classrooms, 1, prob = prob_class)
  Z_class <- rep(Z_class, each = students)
  
  #!!! violate random effects assumption by correlating classroom_score with pretest
  classroom_pretest <- data.frame(classid = rep(1:classrooms, each = students), pretest = pretest)
  classroom_pretest <- classroom_pretest %>% group_by(classid) %>% summarize(pretest_mean = mean(pretest))
  classroom_score <- rep(rnorm(classrooms, 0, 3), each = students) + .05*classroom_pretest$pretest_mean
  
  # outcomes (using both classroom-level, student-level predictors, and the random effect that is correlated with pretest)
  Y0 = 56 + 0 + 0.4*yearstea + 0.33*teach.edu + 0.74*parent.edu + 0.18*fam.income - 2.23*minority - 1.4*dist.school.hour + classroom_score +  rnorm(N, 0 ,1)
  Y1 = 56 + 5 + 0.4*yearstea + 0.33*teach.edu + 0.74*parent.edu + 0.18*fam.income - 2.23*minority - 1.4*dist.school.hour + classroom_score +  rnorm(N, 0 ,1)
  
  # observed outcome based on treatment 
  Y_stud <- ifelse(Z_stud == 1, Y1, Y0)
  Y_class <- ifelse(Z_class == 1, Y1, Y0)
 
  # create dataset
  classroom_dat <- data.frame(yearstea = yearstea, teach.edu = as.factor(teach.edu), avgtest = avgtest,  
                              minority = as.factor(minority), parent.edu = as.factor(parent.edu), fam.income = fam.income,  
                              freelunch = as.factor(freelunch), dist.school.hour = dist.school.hour, gender = as.factor(gender), 
                              pretest = pretest, classid = as.factor(rep(1:classrooms, each = students)), studentid  = 1:N, 
                              Z_stud = Z_stud, Z_class = Z_class, Y0 = Y0, Y1 = Y1, Y_stud = Y_stud, Y_class = Y_class)
  
  return(classroom_dat) 
}


#### Violate Ignoribility Assumption: correlate treatment with pretest scores and removed distance to school from our dataset (missing covariate so we are not measuring all covariates that influence the outcome)

# Alter function to violate ignorability
# 1) Treatment assignment probability is based on pretest scores
# 2) Output data frame does not include distance to school in hours, which violates assumption of ignorability as this confounder affects both treatment assignment and outcome
### HAVE NOT CONFIRMED THIS!!!! 

ig_dat_function <- function(classrooms, students){
  # classroom level covariates
  yearstea <- abs(rep(round(rnorm(classrooms, 5, 5),0), each = students)) # trying to get a range between 0-20
  teach.edu <- rep(sample(c(rep(1,classrooms*.725), rep(2,classrooms*.25), rep(3,classrooms*.025))), each = students) # 
  avgtest <- rep(round(rnorm(classrooms, 72, 8),2), each = students) + 0.75*yearstea #correlated with teachers
  classroom_score <- rep(rnorm(classrooms, 0, 3), each = students) 
  
  # student level covariates
  N <- classrooms*students #number of total students
  
  minority <- sample(c(0:1), N, replace = T, prob = c(0.65, 0.35)) # 65% white students, 35% minority students
  parent.edu <- ifelse(minority == 1, sample(c(0,1,2,3,4), N, replace = T, prob = c(.40, .30, .24, .05, .01)),
                       sample(c(0,1,2,3,4), N, replace = T, prob =c(.17, .26, .40, .10, .07))) # conditional if the student is a minority
  fam.income <- abs(round(abs(rnorm(N, 50, 20)) + ifelse(parent.edu > 2, 15*parent.edu, 0) + ifelse(parent.edu == 2, 6*parent.edu, 0) + 
                            ifelse(minority == 1, rnorm(N, -7, 1), 0),3)) #correlated with parent education and minority 
  freelunch <- ifelse(fam.income < 46.635, 1, 0) # free lunch if family income is less than ~46.6K 
  dist.school.hour <- round(abs(rnorm(N, 20, 4)),0)/60 # indepdent 
  gender <- sample(c(0:1), N, replace = T, prob = c(0.50, 0.50)) # independent
  pretest <-  61 + 0.4*yearstea + 0.3*teach.edu + 0.71*parent.edu + 0.15*fam.income - 2*minority - 1.5*dist.school.hour + rnorm(N, 1, 7)
  
  # probability of assignment is based on pretest scores 
  low_count <- sum(pretest <= 65)
  high_count <- length(pretest) - low_count
  low_score <- sample(c(rep(0,round(.3*low_count,0)),rep(1,1 - round(.3*low_count,0))), low_count) # higher probability (70%) of treatment if test score is <= 65 
  high_score <- sample(c(rep(0,round(.5*high_count,0)), rep(1, 1-round(.5*high_count,0))), high_count) #equal probability of treatment if test score is > 65 
  Z_stud <- rep(NA,N)
  Z_stud[pretest > 65] <- high_score
  Z_stud[pretest <= 65] <- low_score
  
  # outcomes 
  Y0 = 56 + 0 + 0.4*yearstea + 0.33*teach.edu + 0.74*parent.edu + 0.18*fam.income - 2.23*minority - 1.4*dist.school.hour + classroom_score +  rnorm(N, 0 ,1)
  Y1 = 56 + 5 + 0.4*yearstea + 0.33*teach.edu + 0.74*parent.edu + 0.18*fam.income - 2.23*minority - 1.4*dist.school.hour + classroom_score +  rnorm(N, 0 ,1)
  
  # observed outcome based on treatment 
  Y_stud <- ifelse(Z_stud == 1, Y1, Y0)
  
  # create dataset
  # output does not include a confounder, distance to school in hours, which violates ignorability as this informs both
  # pretest scores (and therefore treatment assignment) and outcomes
  classroom_dat <- data.frame(yearstea = yearstea, teach.edu = as.factor(teach.edu), avgtest = avgtest,  
                              minority = as.factor(minority), parent.edu = as.factor(parent.edu), fam.income = fam.income,  
                              freelunch = as.factor(freelunch), gender = as.factor(gender), pretest = pretest,
                              classid = as.factor(rep(1:classrooms, each = students)), studentid  = 1:N, 
                              Z_stud = Z_stud, Y0 = Y0, Y1 = Y1, Y_stud = Y_stud)
  
  return(classroom_dat)
}








