# Source data generation process functions
source("dgp_script.R") 

# Set seed for reproducibility
set.seed(2020)

## call base dgp and store data frame
classroom_dat <- class_dat_function(40, 25)

# plot data 
par(mfrow = c(2,3))
plot(classroom_dat$yearstea, classroom_dat$avgtest, xlab = "Years teaching", ylab="Average Test Scores", main = "")
hist(classroom_dat$fam.income, main = "Family Income")
hist(classroom_dat$pretest, main = "Pretest Scores")
plot(classroom_dat$parent.edu, classroom_dat$fam.income, xlab= "Parent Education", ylab = "Family Income")
plot(classroom_dat$pretest, classroom_dat$Y_stud, xlab = "9th grades scores", ylab = "10th grade scores", main = "Randomized at student-level", col = ifelse(classroom_dat$Z_stud == 1, "blue", "red")) 
plot(classroom_dat$pretest, classroom_dat$Y_class, xlab = "9th grades scores", ylab = "10th grade scores", main = "Randomized at classroom-level", col = ifelse(classroom_dat$Z_class == 1, "blue", "red")) 

student_level_treatment <- ggplot(data=classroom_dat, aes(x=classid, y = Y_stud)) + geom_boxplot() + 
  theme_classic() +labs(x = "Class ID", y = "10th grade test scores", title = "Randomization at student level")
student_level_treatment

classroom_level_treatment <- classroom_dat %>% mutate(treatment = ifelse(Z_class == 1, "Treatment", "Control")) %>% 
  ggplot(aes(x = classid, y = Y_class, fill = treatment)) + geom_boxplot() + 
  theme_classic() + labs(x = "Class ID", y = "10th grade test scores", title = "Randomization at classroom level")
classroom_level_treatment

outcome_plot <- ggpubr::ggarrange(student_level_treatment, classroom_level_treatment, ncol = 1, nrow = 2)
outcome_plot
ggsave("outcome_plot.png", plot = outcome_plot, width = 11.7, height = 8)
