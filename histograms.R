
## Base
par(mfrow = c(2,3))
hist(base_rd_sim$coef[base_rd_sim$type == "lr"], main = "Base - Linear Regression", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(base_rd_sim$coef[base_rd_sim$type == "lr"]), col = "blue", lwd = 2)

hist(base_rd_sim$coef[base_rd_sim$type == "fixed"], main = "Base - Fixed Effects", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(base_rd_sim$coef[base_rd_sim$type == "fixed"]), col = "blue", lwd =2)

hist(base_rd_sim$coef[base_rd_sim$type == "random"], main = "Base - Random Effects", xlab = "Treatment Effect", xlim = c(min(base_rd_sim$coef)-.5, max = max(base_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(base_rd_sim$coef[base_rd_sim$type == "random"]), col = "blue", lwd = 2)

## Violate Ignorability
hist(ig_rd_sim$coef[ig_rd_sim$type == "lr"], main = "Violate Ignorability - Linear Regression", 
     xlab = "Treatment Effect", xlim = c(min(ig_rd_sim$coef)-.5, max = max(ig_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE_ig, col = "red", lty = "dashed", lwd =2)
abline(v = mean(ig_rd_sim$coef[ig_rd_sim$type == "lr"]), col = "blue", lwd =2)

hist(ig_rd_sim$coef[ig_rd_sim$type == "fixed"], main = "Violate Ignorability - Fixed Effects", 
     xlab = "Treatment Effect", xlim = c(min(ig_rd_sim$coef)-.5, max = max(ig_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE_ig, col = "red", lty = "dashed", lwd =2)
abline(v = mean(ig_rd_sim$coef[ig_rd_sim$type == "fixed"]), col = "blue", lwd =2)

hist(ig_rd_sim$coef[ig_rd_sim$type == "random"], main = "Violate Ignorability - Random Effects", 
     xlab = "Treatment Effect", xlim = c(min(ig_rd_sim$coef)-.5, max = max(ig_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE_ig, col = "red", lty = "dashed", lwd =2)
abline(v = mean(ig_rd_sim$coef[ig_rd_sim$type == "random"]), col = "blue", lwd =2)

## Random Effects Violation
par(mfrow = c(2,3))
hist(vre_rd_sim$coef[vre_rd_sim$type == "lr"], main = "Violate Random Effects - Linear Regression", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE_re, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(vre_rd_sim$coef[vre_rd_sim$type == "lr"]), col = "blue", lwd = 2)

hist(vre_rd_sim$coef[vre_rd_sim$type == "fixed"], main = "Violate Random Effects - Fixed Effects", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE_re, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(vre_rd_sim$coef[vre_rd_sim$type == "fixed"]), col = "blue", lwd = 2)

hist(vre_rd_sim$coef[vre_rd_sim$type == "random"], main = "Violate Random Effects - Random Effects", 
     xlab = "Treatment Effect", xlim = c(min(vre_rd_sim$coef)-.5, max = max(vre_rd_sim$coef)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE_re, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(vre_rd_sim$coef[vre_rd_sim$type == "random"]), col = "blue", lwd = 2)

## Group Level Treament Effect
hist(gl_rd_sim$coef[gl_rd_sim$type == "lr"], main = "Group Level Treatment - Linear Regression", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef, na.rm=T)-.5, max = max(gl_rd_sim$coef, na.rm=T)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(gl_rd_sim$coef[gl_rd_sim$type == "lr"]), col = "blue", lwd = 2)

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('SATE', 'Mean'), lty=c(2,1), cex=1, bty='n',
       col = c('red', 'blue'))
mtext("Line:", at=0.2, cex=.9)

hist(gl_rd_sim$coef[gl_rd_sim$type == "random"], main = "Group Level Treatment - Random Effects", 
     xlab = "Treatment Effect", xlim = c(min(gl_rd_sim$coef, na.rm=T)-.5, max = max(gl_rd_sim$coef, na.rm=T)+.5), cex.axis=1.5, cex.lab = 1.5, cex.main = 1.5)
abline(v = SATE, col = "red", lty = "dashed", lwd = 2)
abline(v = mean(gl_rd_sim$coef[gl_rd_sim$type == "random"]), col = "blue", lwd = 2)


