# Examination of a certain subset of the data, which is assumed to circumvent
# possible opposing forces to the influence of age on a youtube videos clicks. 

# Generate Dataset similar to vidStatsMC, but add back the publishdate
vidStats_ageAna <- inner_join(vidStats_df%>%select(id, publishedAt), vidStatsMC, by = "id") %>%
  filter(as.Date(publishedAt) >= as.Date("2022-01-01"), as.Date(publishedAt) <= as.Date("2023-04-01")) %>%
  filter(concertType != "S") # Optional, but here mandatory since only one concert is a special one



# Now make the same analysises for this dataset, concerning the age of a variable:
r2age_m1.1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats_ageAna, formula = viewCount ~ artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2age_m1.1) #=4,2


r2age_m1.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats_ageAna, formula = viewCount ~ age, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2age_m1.2) #=-1,8


r2age_m2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats_ageAna, formula = viewCount ~ age + artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2age_m2) #=4,82

# Model yielding the highest Overall-R2:
r2age_m3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats_ageAna, formula = viewCount ~ artistFollower + age + durationMinutes + concertType + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2age_m3) #=6,33


r2age_allModels <- inner_join(r2age_m1.1%>%select(tau, R2_mean), r2age_m1.2%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,r2age_m2%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,r2age_m3%>%select(tau, R2_mean), by = "tau")

colnames(r2age_allModels) <- c("tau", "aF", "age", "age+aF", "age+aF+dur+typ+num+mon")


# Bheta-Analysis for this dataset (optinal)
bhetaAgeAna <- quantRegCoef(
  formula = viewCount ~ artistFollower + age,
  taus = c(0.1, 0.25, 0.5, 0.75, 0.9),
  data = vidStats_ageAna
)
