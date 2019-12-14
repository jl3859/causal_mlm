library(tidyverse)

gl_rd_sim <- read_csv("output/gl_rd_sim.csv")
gl_sd_sim <- read_csv("output/gl_sd_sim.csv")

# histograms for group level treatment effect

par(mfrow=c(1,3))
## linear regression
hist(gl_rd_sim$coef[gl_rd_sim$type == "lr"], main = "Randomization Distribution - IV (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef, na.rm =T)-.5, 
                                         max = max(gl_rd_sim$coef, na.rm = T)+.5))
abline(v = SATE, col = "red")

## fixed effects
hist(gl_rd_sim$coef[gl_rd_sim$type == "fixed"], main = "Randomization Distribution - IV (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef, na.rm =T)-.5, 
                                         max = max(gl_rd_sim$coef, na.rm = T)+.5))
abline(v = SATE, col = "red")

## random effects
hist(gl_rd_sim$coef[gl_rd_sim$type == "random"], main = "Randomization Distribution - IV (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef, na.rm =T)-.5, 
                                         max = max(gl_rd_sim$coef, na.rm = T)+.5))
abline(v = SATE, col = "red")


# histograms for group level treatment effect
par(mfrow=c(1,3))
## linear regression
hist(gl_sd_sim$coef[gl_sd_sim$type == "lr"], main = "Randomization Distribution - IV (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(gl_sd_sim$coef, na.rm = T)-.5, 
                                         max = max(gl_sd_sim$coef, na.rm = T)+.5))
abline(v = mean(gl_sd_sim$SATE), col = "red")

## fixed effects
hist(gl_sd_sim$coef[gl_sd_sim$type == "fixed"], main = "Randomization Distribution - IV (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_sd_sim$coef, na.rm = T)-.5, 
                                         max = max(gl_sd_sim$coef, na.rm = T)+.5))
abline(v = mean(gl_sd_sim$SATE), col = "red")

## random effects
hist(gl_sd_sim$coef[gl_sd_sim$type == "random"], main = "Randomization Distribution - IV (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(gl_sd_sim$coef, na.rm = T)-.5, 
                                         max = max(gl_sd_sim$coef, na.rm = T)+.5))
abline(v = mean(gl_sd_sim$SATE), col = "red")
