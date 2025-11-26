library(tidyverse)

# Figure 1 
plot(lm(formula = viewCount ~ artistFollower + durationMinutes, data = vidStatsMC), which = 1)


# Figure 2
qqnorm(vidStatsMC$viewCount, main = "")


# Data for Figure 3 and 4
agePlotData <- inner_join(vidStats_df%>%select(id, publishedAt), vidStatsMC, by = "id") %>%
  filter(as.Date(publishedAt) >= as.Date("2021-01-01"), as.Date(publishedAt) <= as.Date("2024-12-31"))

# Figure 3
ggplot(agePlotData, aes(x = publishedAt, y = year)) +
  geom_step(direction = "hv", size = 0.8) +   # hv = horizontal-vertical step
  scale_y_continuous(breaks = unique(agePlotData$year)) +
  labs(x = "Upload date", y = "year", title = "") +
  theme_minimal(base_size = 12)

# Figure 4
ggplot(agePlotData, aes(x = publishedAt, y = age)) +
  geom_line(size = 0.8) +
  labs(x = "Upload date", y = "Age (in days)", title = "") +
  theme_minimal(base_size = 12)


# Figure 5
par(mfrow=c(1,3))
hist(log(vidStats$viewCount), breaks = 30, main = "log(viewCount)", xlab="", ylab="")
hist(log(vidStats$likeCount), breaks = 30, main = "log(likeCount)", xlab="", ylab="")
hist(log(vidStats$commentCount), breaks = 30, main = "log(commentCount)", xlab="", ylab="")


# Figure 6
par(mfrow=c(1,1))
boxplot(log(vidStats$viewCount) ~ vidStats$concertType, xlab = "concertType", ylab = "log(viewCount)")


# Figure 7 (Appendix)
par(mfrow=c(1,3))
hist(vidStats$viewCount, breaks = 30, main = "viewCount", xlab="", ylab="")
hist(vidStats$likeCount, breaks = 30, main = "likeCount", xlab="", ylab="")
hist(vidStats$commentCount, breaks = 30, main = "commentCount", xlab="", ylab="")
