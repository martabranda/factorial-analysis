
# ==============================================================================
# SETUP AND DATA PREPARATION
# ==============================================================================

# Load required packages
library(lavaan)    # For confirmatory factor analysis
library(dplyr)     # For data manipulation
library(haven)     # For reading/writing SPSS files

# ------------------------------------------------------------------------------
# Data Type Conversion
# ------------------------------------------------------------------------------
# Convert haven_labelled variables to numeric to avoid compatibility issues
# This step ensures all scale items are properly recognized as continuous variables

UniBen_factors <- UniBen_factors %>%
  mutate(
    # Psychological functioning items (General Health Questionnaire)
    across(starts_with("Q6_funzionamento_psi_"), ~ as.numeric(.)),
    
    # Participation items
    across(starts_with("Q17_partecipazione_"), ~ as.numeric(.)),
    
    # Wellbeing items
    across(starts_with("Q5_benessere_"), ~ as.numeric(.)),
    
    # Burnout items
    across(starts_with("Q58_burnout_"), ~ as.numeric(.))
  )

# ==============================================================================
# CONFIRMATORY FACTOR ANALYSES
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. GENERAL HEALTH QUESTIONNAIRE (GHQ-12)
# ------------------------------------------------------------------------------
# Three-factor structure:
# - Fpsy_AD: Anxiety/Depression
# - Fpsy_SD: Social Dysfunction
# - FpsyLC: Loss of Confidence

GHQ <- '
  # Anxiety/Depression factor (4 items)
  Fpsy_AD =~ Q6_funzionamento_psi_2riv + Q6_funzionamento_psi_5riv + 
             Q6_funzionamento_psi_6riv + Q6_funzionamento_psi_9riv
  
  # Social Dysfunction factor (6 items)
  Fpsy_SD =~ Q6_funzionamento_psi_1riv + Q6_funzionamento_psi_3riv + 
             Q6_funzionamento_psi_4riv + Q6_funzionamento_psi_7riv + 
             Q6_funzionamento_psi_8riv + Q6_funzionamento_psi_12riv
  
  # Loss of Confidence factor (2 items)
  FpsyLC =~ Q6_funzionamento_psi_10riv + Q6_funzionamento_psi_11riv
'

# Fit the model using ML estimation with partial missing data handling
fitGHQ <- cfa(GHQ, 
              data = UniBen_factors, 
              missing = "ml")  # Maximum likelihood for missing data

# Display results with fit indices and standardized parameters
summary(fitGHQ, fit.measures = TRUE, standardized = TRUE)

# Extract and append factor scores to dataset
GHQ_scores <- lavPredict(fitGHQ)
UniBen_factors <- cbind(GHQ_scores, UniBen_factors)

# ------------------------------------------------------------------------------
# 2. PARTICIPATION SCALE
# ------------------------------------------------------------------------------
# Two-factor structure:
# - Part_att: Active participation attitudes
# - Part_spazi: Space utilization

Partecip <- '
  # Active participation factor (3 items)
  Part_att =~ Q17_partecipazione_1 + Q17_partecipazione_2 + Q17_partecipazione_6
  
  # Space utilization factor (2 items)
  Part_spazi =~ Q17_partecipazione_3 + Q17_partecipazione_4
'

# Fit the participation model
fitPart <- cfa(Partecip, 
               data = UniBen_factors, 
               missing = "ml")

summary(fitPart, fit.measures = TRUE, standardized = TRUE)

# Extract and append factor scores
Partecip_scores <- lavPredict(fitPart)
UniBen_factors <- cbind(Partecip_scores, UniBen_factors)

# ------------------------------------------------------------------------------
# 3. WELLBEING SCALE
# ------------------------------------------------------------------------------
# Three-factor structure based on wellbeing dimensions:
# - BenEM: Emotional wellbeing
# - BenSO: Social wellbeing
# - BenPSI: Psychological wellbeing

Ben <- '
  # Emotional wellbeing (3 items)
  BenEM =~ Q5_benessere_1riv + Q5_benessere_2riv + Q5_benessere_3riv
  
  # Social wellbeing (5 items)
  BenSO =~ Q5_benessere_4riv + Q5_benessere_5riv + Q5_benessere_6riv + 
           Q5_benessere_7riv + Q5_benessere_8riv
  
  # Psychological wellbeing (6 items)
  BenPSI =~ Q5_benessere_9riv + Q5_benessere_10riv + Q5_benessere_11riv + 
            Q5_benessere_12riv + Q5_benessere_13riv + Q5_benessere_14riv
'

# Fit the wellbeing model
fitBen <- cfa(Ben, 
              data = UniBen_factors, 
              missing = "ml")

summary(fitBen, fit.measures = TRUE, standardized = TRUE)

# Extract and append factor scores
Ben_Scores <- lavPredict(fitBen)
UniBen_factors <- cbind(Ben_Scores, UniBen_factors)

# ------------------------------------------------------------------------------
# 4. BURNOUT SCALE (Initial Model)
# ------------------------------------------------------------------------------
# Three-factor structure based on burnout dimensions:
# - Burn_EE: Emotional Exhaustion
# - Burn_Cyn: Cynicism
# - Burn_Eff: Professional Efficacy (reverse coded)

Burnout <- '
  # Emotional Exhaustion (5 items)
  Burn_EE =~ Q58_burnout_1riv + Q58_burnout_2riv + Q58_burnout_3riv + 
             Q58_burnout_4riv + Q58_burnout_5riv
  
  # Cynicism (4 items)
  Burn_Cyn =~ Q58_burnout_6riv + Q58_burnout_7riv + Q58_burnout_8riv + 
              Q58_burnout_9riv
  
  # Professional Efficacy (6 items)
  Burn_Eff =~ Q58_burnout_10riv + Q58_burnout_11riv + Q58_burnout_12riv + 
              Q58_burnout_13riv + Q58_burnout_14riv + Q58_burnout_15riv
'

# Fit initial burnout model
fitBurn <- cfa(Burnout, 
               data = UniBen_factors, 
               missing = "ml")

summary(fitBurn, fit.measures = TRUE, standardized = TRUE)

# ------------------------------------------------------------------------------
# Model Modification Analysis
# ------------------------------------------------------------------------------
# Check modification indices to identify potential model improvements
# Focus on modifications with MI > 10 (substantial improvement in fit)

modindices(fitBurn, sort = TRUE, minimum.value = 10)

# Based on modification indices, items 8 and 9 (both Cynicism items) show
# high residual correlation (MI = 2189.526). This suggests these items share
# variance beyond their common factor, possibly due to similar wording or content.

# ------------------------------------------------------------------------------
# 5. BURNOUT SCALE (Modified Model with Correlated Errors)
# ------------------------------------------------------------------------------
# NOTE: Theoretical justification required for correlating error terms.
# Items 8 and 9 may share method variance or conceptual overlap within Cynicism.

Burn_cor <- '
  # Emotional Exhaustion (5 items)
  Burn_EE =~ Q58_burnout_1riv + Q58_burnout_2riv + Q58_burnout_3riv + 
             Q58_burnout_4riv + Q58_burnout_5riv
  
  # Cynicism (4 items)
  Burn_Cyn =~ Q58_burnout_6riv + Q58_burnout_7riv + Q58_burnout_8riv + 
              Q58_burnout_9riv
  
  # Professional Efficacy (6 items)
  Burn_Eff =~ Q58_burnout_10riv + Q58_burnout_11riv + Q58_burnout_12riv + 
              Q58_burnout_13riv + Q58_burnout_14riv + Q58_burnout_15riv
  
  # Correlated error between items 8 and 9 (both measuring Cynicism)
  Q58_burnout_8riv ~~ Q58_burnout_9riv
'

# Fit modified burnout model
fitBurn_cor <- cfa(Burn_cor, 
                   data = UniBen_factors, 
                   missing = "ml")

summary(fitBurn_cor, fit.measures = TRUE, standardized = TRUE)

# Extract and append factor scores from the modified model
Burn_scores <- lavPredict(fitBurn_cor)
UniBen_factors <- cbind(Burn_scores, UniBen_factors)

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

# Export the dataset with all factor scores to SPSS format
write_sav(UniBen_factors, "UniBen_F23giugno.sav")

# ==============================================================================
# NOTES FOR INTERPRETATION
# ==============================================================================
# 1. Check fit indices: CFI > .95, RMSEA < .06, SRMR < .08 indicate good fit
# 2. Review standardized loadings: should be > .40 for adequate indicator reliability
# 3. Examine factor correlations for discriminant validity
# 4. Consider reliability (omega) for each factor
# 5. Document theoretical justification for any model modifications