source("simulation_vre.R") 

# histograms for violation of random effects assumption
par(mfrow=c(1,3))
## linear regression
hist(vre_rd_sim$coef[vre_rd_sim$type == "lr"], main = "Randomization Distribution - VRE (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5))
abline(v = SATE_re, col = "red")

## fixed effects
hist(vre_rd_sim$coef[vre_rd_sim$type == "fixed"], main = "Randomization Distribution - VRE (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5))
abline(v = SATE_re, col = "red")

## random effects
hist(vre_rd_sim$coef[vre_rd_sim$type == "random"], main = "Randomization Distribution - VRE (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5))
abline(v = SATE_re, col = "red")


# histograms for sampling distribution - random effects violation
## linear regression
hist(vre_sd_sim$coef[vre_sd_sim$type == "lr"], main = "Sampling Distribution - VRE (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(vre_sd_sim$coef)-.5, max = max(vre_sd_sim$coef)+.5))
abline(v = mean(vre_sd_sim$SATE), col = "red")

## fixed effects
hist(vre_sd_sim$coef[vre_sd_sim$type == "fixed"], main = "Sampling Distribution - VRE (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_sd_sim$coef)-.5, max = max(vre_sd_sim$coef)+.5))
abline(v = mean(vre_sd_sim$SATE), col = "red")

## random effects
hist(vre_sd_sim$coef[vre_sd_sim$type == "random"], main = "Sampling Distribution - VRE (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(vre_sd_sim$coef)-.5, max = max(vre_sd_sim$coef)+.5))
abline(v = mean(vre_sd_sim$SATE), col = "red")

ggplot() +
  geom_histogram(data = vre_sd_sim[vre_sd_sim$type == "lr",], aes(x = coef), color = "blue", fill = "white", alpha = .25) +
  geom_histogram(data = vre_sd_sim[vre_sd_sim$type == "fixed",], aes(x = coef), color = "green", fill = "white", alpha = .25) +
  geom_histogram(data = vre_sd_sim[vre_sd_sim$type == "random",], aes(x = coef), color = "red", fill = "white", alpha = .25) +
  labs(x = "Treatment Effect", y = "Frequency") +
  geom_vline(xintercept=mean(vre_sd_sim$SATE), color = "black", linetype = "dashed", size = 1.25)
