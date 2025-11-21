library(quantreg)

#### Für ein Quantil: 

# Quantilsregression schätzen (z.B. Median-Regression)
testMod <- rq(viewCount ~ 1, tau = 0.75, data = vidStatsMC)

# Zusammenfassung des Modells (ähnlich wie summary(lm()))
test_summaryModel <- summary(testMod, se = "nid")  # "nid" = robust standard errors

# Betas + Signifikanzen extrahieren
test_coefs <- test_summaryModel$coefficients


# Ausgabe anzeigen
print(test_coefs)


#### Function für mehrere Quantile:
library(quantreg)
library(dplyr)
library(tidyr)
library(purrr)

quantRegCoef <- function(formula, taus, data) {
  
  # Funktion zur Schätzung eines Modells + Extraktion der Betas
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
  
  # Für jedes Tau Koeffizienten extrahieren
  beta_list <- map(taus, get_betas)
  names(beta_list) <- taus
  
  # In breites Format bringen
  beta_df <- beta_list %>% 
    bind_cols() %>%
    mutate(Variable = rownames(summary(rq(formula, tau = taus[1], data = data))$coefficients)) %>%
    relocate(Variable)
  
  return(beta_df)
}

bhetaFullMod <- quantRegCoef(
  formula = viewCount ~ artistFollower + durationMinutes + concertType + caption + concertNumber,
  taus = c(0.1, 0.25, 0.5, 0.75, 0.9),
  data = vidStatsMC
)

bhetaConstMod <- quantRegCoef(
  formula = viewCount ~ 1,
  taus = c(0.1, 0.25, 0.5, 0.75, 0.9),
  data = vidStatsMC
)

# Bheta-Tabelle in Latex übertragen. Dabei aber auch mit den P-Values eintragen (jeweils einfach als Sternchen unter die Werte)
# Nur mit den Sternchen sind Aussagen über die Werte auch von Relevanz