######################## LIKES ########################

like_r2_m1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})

like_r2_m2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


# best model
like_r2_m3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


# best model
like_r2_m4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})

# best model
like_r2_m5 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


# best model
like_r2_m6 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})

# Bestes Modell! 
like_r2_m7 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = likeCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month + year*artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


like_r2_allModels <- inner_join(like_r2_m1%>%select(tau, R2_mean), like_r2_m2%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,like_r2_m3%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,like_r2_m4%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,like_r2_m5%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,like_r2_m6%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,like_r2_m7%>%select(tau, R2_mean), by = "tau")

colnames(like_r2_allModels) <- c("tau", "+aF", "+dur", "+typ", "+cap", "+num", "+mon", "+year*aF")


######################## COMMENTS ########################

comment_r2_m1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})

comment_r2_m2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


# best model
comment_r2_m3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


# best model
comment_r2_m4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})

# best model
comment_r2_m5 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


# best model
comment_r2_m6 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})

# Bestes Modell! 
comment_r2_m7 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = commentCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month + year*artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})


comment_r2_allModels <- inner_join(comment_r2_m1%>%select(tau, R2_mean), comment_r2_m2%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,comment_r2_m3%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,comment_r2_m4%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,comment_r2_m5%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,comment_r2_m6%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,comment_r2_m7%>%select(tau, R2_mean), by = "tau")

colnames(comment_r2_allModels) <- c("tau", "+aF", "+dur", "+typ", "+cap", "+num", "+mon", "+year*aF")
