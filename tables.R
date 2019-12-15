library(gridExtra)

# Source data generation process functions
oldtime <- Sys.time()
source("dgp_script.R") 
source("simulation_script.R")

Sys.time() - oldtime

# BIAS COMPARISON ###################################################################################################

# From base models 
bias_compare_initial <- data.frame("Bias Comparison" = c("Linear Regression", "Fixed Effects", "Random Effects"),
                                   "Base Case" = round(c(lr.base.bias, fe.base.bias, re.base.bias),4),
                                   "RE Violation" = round(c(lr.re.bias, fe.re.bias, re.re.bias),4),
                                   "Ig. Violation" = round(c(lr.ig.bias, fe.ig.bias, re.ig.bias),4),
                                   "Group Treatment" = c(round(lr.gl.bias,4), "N/A", round(re.gl.bias,4)))

grid.arrange(tableGrob(bias_compare_initial, theme=ttheme_default()), nrow = 1)


# From randomization distributions 
bias_compare_rd_sim <- data.frame("Bias Comparison" = c("Linear Regression", "Fixed Effects", "Random Effects"), 
                               "Base Case" = round(c(mean(base_rd_sim$bias[base_rd_sim$type == "lr"]), 
                                                     mean(base_rd_sim$bias[base_rd_sim$type == "fixed"]), 
                                                     mean(base_rd_sim$bias[base_rd_sim$type == "random"])),4),
                               "Ig Violation" = round(c(mean(ig_rd_sim$bias[ig_rd_sim$type=="lr"]), 
                                                         mean(ig_rd_sim$bias[ig_rd_sim$type=="fixed"]), 
                                                         mean(ig_rd_sim$bias[ig_rd_sim$type=="random"])),4), 
                               "RE Violation" = round(c(mean(vre_rd_sim$bias[vre_rd_sim$type == "lr"]), 
                                                        mean(vre_rd_sim$bias[vre_rd_sim$type == "fixed"]), 
                                                        mean(vre_rd_sim$bias[vre_rd_sim$type == "random"])),4),
                               "Group Treatment" = c(round(mean(gl_rd_sim$bias[gl_rd_sim$type == "lr"]),4), 
                                                     "N/A", 
                                                     round(mean(gl_rd_sim$bias[gl_rd_sim$type == "random"]),4)))

grid.arrange(tableGrob(bias_compare_rd_sim, theme=ttheme_default()), nrow = 1)

# From sampling distributions 
bias_compare_sd_sim <- data.frame("Bias Comparison" = c("Linear Regression", "Fixed Effects", "Random Effects"), 
                                  "Base Case" = round(c(mean(base_sd_sim$bias[base_sd_sim$type == "lr"]), 
                                                        mean(base_sd_sim$bias[base_sd_sim$type == "fixed"]), 
                                                        mean(base_sd_sim$bias[base_sd_sim$type == "random"])),4), 
                                  "RE Violation" = round(c(mean(vre_sd_sim$bias[vre_sd_sim$type == "lr"]), 
                                                           mean(vre_sd_sim$bias[vre_sd_sim$type == "fixed"]), 
                                                           mean(vre_sd_sim$bias[vre_sd_sim$type == "random"])),4), 
                                  "Ig. Violation" = round(c(mean(ig_sd_sim$bias[ig_sd_sim$type=="lr"]), 
                                                            mean(ig_sd_sim$bias[ig_sd_sim$type=="fixed"]), 
                                                            mean(ig_sd_sim$bias[ig_sd_sim$type=="random"])),4), 
                                  "Group Treatment" = c(round(mean(gl_sd_sim$bias[gl_sd_sim$type == "lr"]),4), 
                                                        "N/A", 
                                                        round(mean(gl_sd_sim$bias[gl_sd_sim$type == "random"]),4)))

grid.arrange(tableGrob(bias_compare_sd_sim, theme=ttheme_default()), nrow = 1)

# Subset simulation results by model type
base_lr_rd_sim <- base_rd_sim %>% filter(type == "lr")
base_fixed_rd_sim <- base_rd_sim %>% filter(type == "fixed")
base_random_rd_sim <- base_rd_sim %>% filter(type == "random")

vre_lr_rd_sim <- vre_rd_sim %>% filter(type == "lr")
vre_fixed_rd_sim <- vre_rd_sim %>% filter(type == "fixed")
vre_random_rd_sim <- vre_rd_sim %>% filter(type == "random")

ig_lr_rd_sim <- ig_rd_sim %>% filter(type == "lr")
ig_fixed_rd_sim <- ig_rd_sim %>% filter(type == "fixed")
ig_random_rd_sim <- ig_rd_sim %>% filter(type == "random")

gl_lr_rd_sim <- gl_rd_sim %>% filter(type == "lr")
gl_random_rd_sim <- gl_rd_sim %>% filter(type == "random")


#### RMSE and CI
## base ####
## lr
base_lr_rmse <- sqrt(mean((base_lr_rd_sim$coef - base_lr_rd_sim$SATE)^2))
base_lr_in_ci <- sum(base_lr_rd_sim$coef > base_lr_rd_sim$conf_int_low & base_lr_rd_sim$coef < base_lr_rd_sim$conf_int_high)/nrow(base_lr_rd_sim)
base_lr_in_ci

## fixed
base_fixed_rmse <- sqrt(mean((base_fixed_rd_sim$coef - base_fixed_rd_sim$SATE)^2))
base_fixed_in_ci <- sum(base_fixed_rd_sim$coef > base_fixed_rd_sim$conf_int_low & base_fixed_rd_sim$coef < base_fixed_rd_sim$conf_int_high)/nrow(base_fixed_rd_sim)
base_fixed_in_ci

## random
base_random_rmse <- sqrt(mean((base_random_rd_sim$coef - base_random_rd_sim$SATE)^2))
base_random_in_ci <- sum(base_random_rd_sim$coef > base_random_rd_sim$conf_int_low & base_random_rd_sim$coef < base_random_rd_sim$conf_int_high)/nrow(base_random_rd_sim)
base_random_in_ci

## violate random effects assumption ####
## lr
vre_lr_rmse <- sqrt(mean((vre_lr_rd_sim$coef - vre_lr_rd_sim$SATE)^2))
vre_lr_in_ci <- sum(vre_lr_rd_sim$coef > vre_lr_rd_sim$conf_int_low & vre_lr_rd_sim$coef < vre_lr_rd_sim$conf_int_high)/nrow(vre_lr_rd_sim)
vre_lr_in_ci

## fixed
vre_fixed_rmse <- sqrt(mean((vre_fixed_rd_sim$coef - vre_fixed_rd_sim$SATE)^2))
vre_fixed_in_ci <- sum(vre_fixed_rd_sim$coef > vre_fixed_rd_sim$conf_int_low & vre_fixed_rd_sim$coef < vre_fixed_rd_sim$conf_int_high)/nrow(vre_fixed_rd_sim)
vre_fixed_in_ci

## random
vre_random_rmse <- sqrt(mean((vre_random_rd_sim$coef - vre_random_rd_sim$SATE)^2))
vre_random_in_ci <- sum(vre_random_rd_sim$coef > vre_random_rd_sim$conf_int_low & vre_random_rd_sim$coef < vre_random_rd_sim$conf_int_high)/nrow(vre_random_rd_sim)
vre_random_in_ci

## violate ignorability assumption ####
## lr
ig_lr_rmse <- sqrt(mean((ig_lr_rd_sim$coef - ig_lr_rd_sim$SATE)^2))
ig_lr_in_ci <- sum(ig_lr_rd_sim$coef > ig_lr_rd_sim$conf_int_low & ig_lr_rd_sim$coef < ig_lr_rd_sim$conf_int_high)/nrow(ig_lr_rd_sim)
ig_lr_in_ci

## fixed
ig_fixed_rmse <- sqrt(mean((ig_fixed_rd_sim$coef - ig_fixed_rd_sim$SATE)^2))
ig_fixed_in_ci <- sum(ig_fixed_rd_sim$coef > ig_fixed_rd_sim$conf_int_low & ig_fixed_rd_sim$coef < ig_fixed_rd_sim$conf_int_high)/nrow(ig_fixed_rd_sim)
ig_fixed_in_ci

## random
ig_random_rmse <- sqrt(mean((ig_random_rd_sim$coef - ig_random_rd_sim$SATE)^2))
ig_random_in_ci <- sum(ig_random_rd_sim$coef > ig_random_rd_sim$conf_int_low & ig_random_rd_sim$coef < ig_random_rd_sim$conf_int_high)/nrow(ig_random_rd_sim)
ig_random_in_ci

## group level treatment ####
## lr
gl_lr_rmse <- sqrt(mean((gl_lr_rd_sim$coef - gl_lr_rd_sim$SATE)^2))
gl_lr_in_ci <- sum(gl_lr_rd_sim$coef > gl_lr_rd_sim$conf_int_low & gl_lr_rd_sim$coef < gl_lr_rd_sim$conf_int_high)/nrow(gl_lr_rd_sim)
gl_lr_in_ci

## treatment effect cannot be estimated with fixed effects 

## random
gl_random_rmse <- sqrt(mean((gl_random_rd_sim$coef - gl_random_rd_sim$SATE)^2))
gl_random_in_ci <- sum(gl_random_rd_sim$coef > gl_random_rd_sim$conf_int_low & gl_random_rd_sim$coef < gl_random_rd_sim$conf_int_high)/nrow(gl_random_rd_sim)
gl_random_in_ci

## RMSE ####
rmse_rd_compare <- data.frame("RMSE Comparison" = c("Linear Regression", "Fixed Effects", "Random Effects"),
                                   "Base Case" = round(c(base_lr_rmse, base_fixed_rmse, base_random_rmse),2),
                                   "Ig Violation" = round(c(ig_lr_rmse, ig_fixed_rmse, ig_random_rmse),2),
                                   "RE Violation" = round(c(vre_lr_rmse, vre_fixed_rmse, vre_random_rmse),2),
                                   "Group Treatment" = c(round(gl_lr_rmse,2), "N/A", round(gl_random_rmse,2)))

grid.arrange(tableGrob(rmse_rd_compare, theme=ttheme_default()), nrow = 1)

## CI ### 
## all 100% ###
ci_rd_compare <- data.frame("CI Comparison" = c("Linear Regression", "Fixed Effects", "Random Effects"),
                              "Base Case" = round(c(base_lr_in_ci, base_fixed_in_ci, base_random_in_ci),2),
                              "RE Violation" = round(c(vre_lr_in_ci, vre_fixed_in_ci, vre_random_in_ci),2),
                              "Ig. Violation" = round(c(ig_lr_in_ci, ig_fixed_in_ci, ig_random_in_ci),2),
                              "Group Treatment" = c(round(gl_lr_in_ci,4), "N/A", round(gl_random_in_ci,2)))

grid.arrange(tableGrob(ci_rd_compare, theme=ttheme_default()), nrow = 1)


