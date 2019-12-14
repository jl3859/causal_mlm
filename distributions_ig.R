source("simulation_ig.R")

# histograms for violation of ignorability assumption (from randomization distribution)
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

# histograms for violation of ignorability assumption (from sampling distribution)
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
