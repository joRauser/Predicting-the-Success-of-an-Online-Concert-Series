# Figure 1
par(mfrow=c(1,3))
hist(log(vidStats$viewCount), breaks = 30, main = "log(viewCount)", xlab="", ylab="")
hist(log(vidStats$likeCount), breaks = 30, main = "log(likeCount)", xlab="", ylab="")
hist(log(vidStats$commentCount), breaks = 30, main = "log(commentCount)", xlab="", ylab="")

# Figure 2.1
par(mfrow=c(1,1))
boxplot(log(vidStats$viewCount) ~ vidStats$concertType, xlab = "concertType", ylab = "log(viewCount)")

# Figure 2.2 ?
boxplot(log(vidStats$viewCount) ~ vidStats$concertNumber)


plot(vidStats$age, log(vidStats$viewCount))
abline(h = mean(log(vidStats$viewCount)), col = "blue", lwd = 2)

# Residuals of an OLS approach
plot(lm(formula = viewCount ~ artistFollower + durationMinutes, data = vidStatsMC), which = 1)

# QQ-Plot of views on normality
qqnorm(vidStatsMC$viewCount, main = "")
