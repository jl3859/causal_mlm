library(gridExtra)

# Source data generation process functions
oldtime <- Sys.time()
source("dgp_script.R") 
source("simulation_base.R")
source("simulation_ig.R")
source("simulation_vre.R")
source("simulation_gl.R")
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
                               "RE Violation" = round(c(mean(vre_rd_sim$bias[vre_rd_sim$type == "lr"]), 
                                                        mean(vre_rd_sim$bias[vre_rd_sim$type == "fixed"]), 
                                                        mean(vre_rd_sim$bias[vre_rd_sim$type == "random"])),4), 
                               "Ig. Violation" = round(c(mean(ig_rd_sim$bias[ig_rd_sim$type=="lr"]), 
                                                         mean(ig_rd_sim$bias[ig_rd_sim$type=="fixed"]), 
                                                         mean(ig_rd_sim$bias[ig_rd_sim$type=="random"])),4), 
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
