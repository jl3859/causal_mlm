source("simulation_base.R") 

# Histograms for base case randomization distribution
par(mfrow=c(1,3))
hist(base_rd_sim$coef[base_rd_sim$type == "lr"], main = "Randomization Distribution - Base (Linear Regression)", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

hist(base_rd_sim$coef[base_rd_sim$type == "fixed"], main = "Randomization Distribution - Base (Fixed Effects)", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

hist(base_rd_sim$coef[base_rd_sim$type == "random"], main = "Randomization Distribution - Base (Random Effects)", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5))
abline(v = SATE, col = "red")

# Histograms for base case sampling distribution
par(mfrow=c(1,3))
hist(base_sd_sim$coef[base_sd_sim$type == "lr"], main = "Sampling Distribution - Base (Linear Regression)", 
     xlab = "Treatment Effect", xlim = c(min(base_sd_sim$coef)-.5, max = max(base_sd_sim$coef)+.5))
abline(v = mean(base_sd_sim$SATE), col = "red")

hist(base_sd_sim$coef[base_sd_sim$type == "fixed"], main = "Sampling Distribution - Base (Fixed Effects)", 
     xlab = "Treatment Effect", xlim = c(min(base_sd_sim$coef)-.5, max = max(base_sd_sim$coef)+.5))
abline(v = mean(base_sd_sim$SATE), col = "red")

hist(base_sd_sim$coef[base_sd_sim$type == "random"], main = "Sampling Distribution - Base (Random Effects)", 
     xlab = "Treatment Effect", xlim = c(min(base_sd_sim$coef)-.5, max = max(base_sd_sim$coef)+.5))
abline(v = mean(base_sd_sim$SATE), col = "red")

ggplot() +
  geom_histogram(data = base_sd_sim[base_sd_sim$type == "lr",], aes(x = coef), 
                 color = "blue", fill = "white", alpha = .25) +
  geom_histogram(data = base_sd_sim[base_sd_sim$type == "fixed",], aes(x = coef), 
                 color = "green", fill = "white", alpha = .25) +
  geom_histogram(data = base_sd_sim[base_sd_sim$type == "random",], aes(x = coef), 
                 color = "red", fill = "white", alpha = .25) +
  labs(x = "Treatment Effect", y = "Frequency") +
  geom_vline(xintercept=SATE, color = "black", linetype = "dashed", size = 1.25)