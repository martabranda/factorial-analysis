# Load required packages
library(haven)     # SPSS file handling
library(lavaan)    # Confirmatory factor analysis
library(dplyr)     # Data manipulation

# Import dataset
Predictors_Ben_1 <- read_sav("Predictors_Ben_1.sav")

# Convert haven_labelled variables to numeric
Predictors_Ben_1 <- Predictors_Ben_1 %>%
  mutate(
    across(starts_with("Q9_condizlav_"), ~ as.numeric(.)),      # Work conditions
    across(starts_with("Q11_appartenenzaorg_"), ~ as.numeric(.)) # Organizational belonging
  )

# ==============================================================================
# CFA 1: WORK CONDITIONS (5-FACTOR MODEL)
# ==============================================================================
# Examining different dimensions of work conditions as predictors of wellbeing

condiz_model <- '
  # Work Overload (4 items)
  SOVR_LAV =~ Q9_condizlav_1 + Q9_condizlav_6 + Q9_condizlav_12 + Q9_condizlav_19
  
  # Supervisor Support (3 items)
  SUPP_Dir =~ Q9_condizlav_2 + Q9_condizlav_13 + Q9_condizlav_16
  
  # Group/Team Technical Support (3 items)
  GroupTech =~ Q9_condizlav_3 + Q9_condizlav_7 + Q9_condizlav_10
  
  # Parent-School Relations (3 items)
  Parents =~ Q9_condizlav_4 + Q9_condizlav_5 + Q9_condizlav_15
  
  # Discipline Management (3 items)
  GestDisc =~ Q9_condizlav_8 + Q9_condizlav_11 + Q9_condizlav_14
'

# Fit model with ML estimation for missing data
fit_condiz <- cfa(condiz_model, data = Predictors_Ben_1, missing = "ml")
summary(fit_condiz, fit.measures = TRUE, standardized = TRUE)

# Extract and append factor scores
factor_scores <- lavPredict(fit_condiz)
Predictors_Ben_1 <- cbind(factor_scores, Predictors_Ben_1)

# ==============================================================================
# CFA 2: ORGANIZATIONAL BELONGING (MODIFIED MODEL)
# ==============================================================================
# Single factor with correlated residuals for theoretically related items

appartenenza_model <- '
  # Organizational Belonging (6 items)
  APP =~ Q11_appartenenzaorg_1 + Q11_appartenenzaorg_2 + Q11_appartenenzaorg_3 + 
         Q11_appartenenzaorg_4 + Q11_appartenenzaorg_5 + Q11_appartenenzaorg_6
  
  # Correlated residuals (content overlap)
  Q11_appartenenzaorg_3 ~~ Q11_appartenenzaorg_4  # Pride-related items
  Q11_appartenenzaorg_1 ~~ Q11_appartenenzaorg_4  # Identity-related items
  Q11_appartenenzaorg_2 ~~ Q11_appartenenzaorg_5  # Commitment-related items
'

# Fit model
fit_app <- cfa(appartenenza_model, data = Predictors_Ben_1, missing = "ml")
summary(fit_app, fit.measures = TRUE, standardized = TRUE)

# Check for additional model improvements
modindices(fit_app, sort = TRUE, minimum.value = 10)

# Extract and append factor scores
factor_scores_app <- lavPredict(fit_app)
Predictors_Ben_1 <- cbind(factor_scores_app, Predictors_Ben_1)

# ==============================================================================
# DATA QUALITY CHECK
# ==============================================================================

# Check for duplicate column names (important when cbinding multiple times)
if(any(duplicated(names(Predictors_Ben_1)))) {
  warning("Duplicate column names detected:")
  print(names(Predictors_Ben_1)[duplicated(names(Predictors_Ben_1))])
}

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

# Export dataset with all factor scores
write_sav(Predictors_Ben_1, "PredictorsBen_Factors.sav")

# ==============================================================================
# ANALYSIS NOTES
# ==============================================================================
# Work Conditions Factors:
# - SOVR_LAV: Work overload/time pressure
# - SUPP_Dir: Leadership support quality
# - GroupTech: Peer collaboration effectiveness  
# - Parents: Parent-teacher relationship quality
# - GestDisc: Classroom management challenges
#
# Organizational Belonging:
# - APP: Overall organizational identification and commitment
# - Correlated errors suggest subscale structure within belonging cons