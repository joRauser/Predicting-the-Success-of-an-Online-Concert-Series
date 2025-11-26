library(quantreg)

####### exemplary for a single quantile: ####### 

# estimate quantreg
testMod <- rq(viewCount ~ 1, tau = 0.75, data = vidStatsMC)

# summarise the model
test_summaryModel <- summary(testMod, se = "nid")  # "nid" = robust standard errors

# extract betas and significances
test_coefs <- test_summaryModel$coefficients



####### function for multiple quantiles #######
library(quantreg)
library(dplyr)
library(tidyr)
library(purrr)

quantRegCoef <- function(formula, taus, data) {
  
  # estimate model and extract bhetas
  get_betas <- function(tau) {
    mod <- rq(formula, tau = tau, data = data)
    sm  <- summary(mod, se = "nid")
    coef <- sm$coefficients
    
    # significance-stars
    stars <- case_when(
      coef[, "Pr(>|t|)"] <= 0.001 ~ "***",
      coef[, "Pr(>|t|)"] <= 0.01  ~ "**",
      coef[, "Pr(>|t|)"] <= 0.05  ~ "*",
      coef[, "Pr(>|t|)"] <= 0.1   ~ ".",
      TRUE ~ ""
    )
    
    betaStars <- paste0(round(coef[, "Value"], 4), stars)
    names(betaStars) <- rownames(coef)
    
    return(betaStars)
  }
  
  # extract coefficient for every tau
  beta_list <- map(taus, get_betas)
  names(beta_list) <- taus
  
  # format
  beta_df <- beta_list %>% 
    bind_cols() %>%
    mutate(Variable = rownames(summary(rq(formula, tau = taus[1], data = data))$coefficients)) %>%
    relocate(Variable)
  
  return(beta_df)
}

# execute the function
bhetaFullMod <- quantRegCoef(
  formula = viewCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber,
  taus = c(0.1, 0.25, 0.5, 0.75, 0.9),
  data = vidStatsMC
)
