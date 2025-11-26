# Forward Variable Selection / Model Comparison
library(purrr)
library(quantreg)
library(scoringRules)
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)


vidStatsMC <- vidStats %>%
  filter(., artistFollower != 1)
export(vidStatsMC, "vidStatsMC.csv")


# Estimating R2(Tau) and its QuantileLosses for single Tau's:
r2_kfold_cv <- function(data, formula, tau = .5, k = 5) {
  n <- nrow(data)
  set.seed(7)
  varCount <- length(all.vars(formula)) - 1
  # split data into 5 distinct sets (folds)
  folds <- sample(rep(1:k, length.out = n))  
  
  # extract response variable
  responseVar <- all.vars(formula)[1]
  
  scoreMatrix <- matrix(NA, nrow = k+1, ncol = 3, dimnames = list(c(1:k, "mean"), c("QuantileScores", "QSNullmod", "R2")))
  
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data  <- data[folds == i, ]
    
    # fit model
    fit <- rq(formula, tau = tau, data = train_data)
    # nullmodel: 
    fitNull <- rq(as.formula(paste(responseVar, "~ 1")), tau = tau, data = train_data)
    
    # prediction
    pred <- predict(fit, newdata = test_data)
    predNull <- predict(fitNull, newdata = test_data)
    
    
    qs <- qs_quantiles(y = test_data[[responseVar]], x = pred, alpha = tau) %>% 
      mean()
    qsNull <- qs_quantiles(y = test_data[[responseVar]], x = predNull, alpha = tau) %>% 
      mean()
    
    scoreMatrix[i,1] <- as.numeric(qs)
    scoreMatrix[i,2] <- as.numeric(qsNull)
    scoreMatrix[i,3] <- 1 - qs/qsNull # R-Squared
  }
  for(j in 1:3){
    scoreMatrix[k+1,j] <- mean(scoreMatrix[1:k,j])
  }
  return(scoreMatrix)
}


# Function to estimate the overall R2 of a model:
r2GTot <- function(modelData){
  sumModelLoss <- sum(modelData["QuantileScores"])
  sumNullmodelLoss <- sum(modelData["QSNullmod"])
  r2 <- 1 - sumModelLoss/sumNullmodelLoss
  return(r2)
}



####### Model Comparison #######

# Model 1: 
r2_m1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m1) #=13,8

# Exemplarly, the models for every possible "successor" variable are derived, from which the one with the highest total r2 is chosen. 
r2_m2.1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.1) #=14,59


r2_m2.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + concertType, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.2) #=14,48


r2_m2.3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + concertNumber, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.3) #=13,91


r2_m2.4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.4) #=13,96


r2_m2.5 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + age, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.5) #=13,8


r2_m2.6 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + year, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.6) #=13,8


r2_m2.7 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + caption, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.7) #=13,84


r2_m2.8 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + year*artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m2.8) #=14,45

# => The best model is model 2.1 => choose durationMinutes as successor

# This procedure was applied to the rest as well.
# In the following, only models achieving the highest R2 compared to all other models, are listed

r2_m3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes + concertType, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m3) #=15,24


r2_m4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes + concertType + caption, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m4) #=15,32


r2_m5 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m5) #=15,37


r2_m6 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m6) #=15,41


r2_m7.1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month + age, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m7.1) #=15,46

r2_m7.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month + year, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m7.2) #=15,46

r2_m7.3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStatsMC, formula = viewCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber + month + year*artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"],
         QuantileScores = as.numeric(res["mean", "QuantileScores"]),
         QSNullmod = as.numeric(res["mean", "QSNullmod"]))
})
r2GTot(r2_m7.3) #=16,11


# Combining all R2(Tau) of any important model into one dataframe:
r2_allModels <- inner_join(r2_m1%>%select(tau, R2_mean), r2_m2.1%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,r2_m3%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,r2_m4%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,r2_m5%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,r2_m6%>%select(tau, R2_mean), by = "tau")%>%
  inner_join(.,r2_m7.3%>%select(tau, R2_mean), by = "tau")

colnames(r2_allModels) <- c("tau", "+aF", "+dur", "+typ", "+cap", "+num", "+mon", "+year*aF")

