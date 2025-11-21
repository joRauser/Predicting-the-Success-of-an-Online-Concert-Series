######################## LIKES ########################

like_r3_m1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})

like_r3_m2.4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


# best model
like_r3_m3.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


# best model
like_r3_m4.5 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})

# best model
like_r3_m5.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


# best model
like_r3_m6.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})

# Bestes Modell! 
like_r3_m7.4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month + year*artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


like_r3_allModels <- inner_join(like_r3_m1, like_r3_m2.4, by = "tau")%>%
  inner_join(.,like_r3_m3.2, by = "tau")%>%
  inner_join(.,like_r3_m4.5, by = "tau")%>%
  inner_join(.,like_r3_m5.2, by = "tau")%>%
  inner_join(.,like_r3_m6.2, by = "tau")%>%
  inner_join(.,like_r3_m7.4, by = "tau")

colnames(like_r3_allModels) <- c("tau", "+aF", "+dur", "+typ", "+cap", "+num", "+mon", "+year*aF")

######################## COMMENTS ########################

comment_r3_m1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})

comment_r3_m2.4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


# best model
comment_r3_m3.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


# best model
comment_r3_m4.5 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})

# best model
comment_r3_m5.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


# best model
comment_r3_m6.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})

# Bestes Modell! 
comment_r3_m7.4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month + year*artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"])
})


comment_r3_allModels <- inner_join(comment_r3_m1, comment_r3_m2.4, by = "tau")%>%
  inner_join(.,comment_r3_m3.2, by = "tau")%>%
  inner_join(.,comment_r3_m4.5, by = "tau")%>%
  inner_join(.,comment_r3_m5.2, by = "tau")%>%
  inner_join(.,comment_r3_m6.2, by = "tau")%>%
  inner_join(.,comment_r3_m7.4, by = "tau")

colnames(comment_r3_allModels) <- c("tau", "+aF", "+dur", "+typ", "+cap", "+num", "+mon", "+year*aF")