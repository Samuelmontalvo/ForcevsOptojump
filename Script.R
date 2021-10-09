library(readxl)
ForcevsOptojump <- read_excel("ForcevsOptojump.xlsx", 
                              sheet = "R")
View(ForcevsOptojump)

##Reliability Analysis Plate ICC2k
library(dplyr)

## Force ICC
library(tidyr)
ForceICC <- ForcevsOptojump %>% select(Rep,FORCE)%>% 
  group_by(Rep) %>%mutate(row = row_number()) %>%
  pivot_wider(names_from = Rep, values_from = FORCE) %>%
  select(-row)

library(psych)
ICC(ForceICC)

## Optojump ICC
OptojumpICC <- ForcevsOptojump %>% select(Rep,OPTO)%>% 
  group_by(Rep) %>%mutate(row = row_number()) %>%
  pivot_wider(names_from = Rep, values_from = OPTO) %>%
  select(-row)

ICC(OptojumpICC)

##Validity
library(bmbstats)
bmbstats::plot_pair_OLP(
  predictor = ForcevsOptojump$FORCE,
  outcome = ForcevsOptojump$OPTO,
  predictor_label = "Optojump",
  outcome_label = "Force Platform",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5
)

library(ggplot2)
ggsave("ForcevsOptojump.png")

olp_method <- function(data=ForcevsOptojump,
                       criterion=FORCE,
                       practical=OPTO,
                       SESOI_lower = 0,
                       SESOI_upper = 0,
                       na.rm = FALSE) {
  practical_obs <- data[[practical]]
  criterion_obs <- data[[criterion]]
  SESOI_range <- SESOI_upper - SESOI_lower
  
  olp_model <- bmbstats::OLP_regression(
    outcome = criterion_obs,
    predictor = practical_obs,
    na.rm = na.rm)
  
  n_obs <- length(criterion_obs)
  
  intercept <- olp_model$intercept
  slope <- olp_model$slope
  rse <- olp_model$rse
  
  PPER <- stats::pt((SESOI_upper) / rse, df = n_obs - 1) -
    stats::pt((SESOI_lower) / rse, df = n_obs - 1)
  
  c(
    "Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER
  )
}


olp_method(
  data = ForcevsOptojump,
  criterion = "FORCE",
  practical = "OPTO",
  SESOI_lower = -0.25,
  SESOI_upper = 0.25
)

olp_validity <- bmbstats::validity_analysis(
  data = ForcevsOptojump,
  criterion = "FORCE",
  practical = "OPTO",
  SESOI_lower = -0.25,
  SESOI_upper = .25,
  estimator_function = olp_method,
  control = model_control(seed = 1667)
)
olp_validity