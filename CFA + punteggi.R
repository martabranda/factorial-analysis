# Load required packages
library(lavaan)    # Confirmatory factor analysis
library(haven)     # SPSS file handling

# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------
# Convert haven_labelled variables to numeric for CFA compatibility
# This prevents type conflicts during model estimation

HBSC_LPA <- as.data.frame(lapply(HBSC_2022_Insegnanti_Da_Usare_4, function(x) {
  if (inherits(x, "haven_labelled")) {
    as.numeric(x)
  } else {
    x
  }
}))

# ==============================================================================
# CONFIRMATORY FACTOR ANALYSES
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. WORK ENGAGEMENT (UWES - Single Factor Model)
# ------------------------------------------------------------------------------
# Note: Complex structure with correlated residuals suggesting method effects
# or shared item content across engagement dimensions

engagement <- '
  # Single engagement factor with 9 items
  F1 =~ Q12_engagement_1 + Q12_engagement_2 + Q12_engagement_5 + 
        Q12_engagement_3 + Q12_engagement_4 + Q12_engagement_7 + 
        Q12_engagement_6 + Q12_engagement_8 + Q12_engagement_9
  
  # Correlated residuals - Items 1, 2, 5 (possibly vigor dimension)
  Q12_engagement_1 ~~ Q12_engagement_2 
  Q12_engagement_1 ~~ Q12_engagement_5
  Q12_engagement_2 ~~ Q12_engagement_5
  
  # Correlated residuals - Items 3, 4, 7 (possibly dedication dimension)
  Q12_engagement_3 ~~ Q12_engagement_4 
  Q12_engagement_3 ~~ Q12_engagement_7
  Q12_engagement_4 ~~ Q12_engagement_7
  
  # Correlated residuals - Items 6, 8, 9 (possibly absorption dimension)
  Q12_engagement_6 ~~ Q12_engagement_8
  Q12_engagement_6 ~~ Q12_engagement_9
  Q12_engagement_8 ~~ Q12_engagement_9
'

fit_engagement <- cfa(engagement, data = HBSC_LPA, missing = "fiml")
summary(fit_engagement, fit.measures = TRUE)

# ------------------------------------------------------------------------------
# 2. TEACHER BURNOUT (Four-Factor Model)
# ------------------------------------------------------------------------------
# Extended burnout model with four dimensions
# F1: Emotional exhaustion (9 items)
# F2: Depersonalization/Cynicism (5 items)
# F3: Personal accomplishment (8 items)
# F4: Secondary dimension (5 items)

burnout <- '
# Factor 1: Emotional exhaustion 
F1 =~ Q13_burnout_1 + Q13_burnout_2 + Q13_burnout_3 + Q13_burnout_7 + 
        Q13_burnout_9 + Q13_burnout_15 + Q13_burnout_16 + Q13_burnout_18 + 
        Q13_burnout_24
  
# Factor 2: Depersonalization
F2 =~ Q13_burnout_6 + Q13_burnout_11 + Q13_burnout_12 + Q13_burnout_17 + 
        Q13_burnout_27
  
# Factor 3: PRealization
F3 =~ Q13_burnout_4 + Q13_burnout_8 + Q13_burnout_10 + Q13_burnout_13 + 
        Q13_burnout_20 + Q13_burnout_21 + Q13_burnout_23 + Q13_burnout_25
  
# Factor 4: Cynism
F4 =~ Q13_burnout_5 + Q13_burnout_14 + Q13_burnout_19 + Q13_burnout_22 + 
        Q13_burnout_26
'

fit_burnout <- cfa(burnout, data = HBSC_LPA, missing = "fiml")
summary(fit_burnout, fit.measures = TRUE)

# ------------------------------------------------------------------------------
# 3. TEACHER ROLE IDENTITY QUESTIONNAIRE (TRIQ)
# ------------------------------------------------------------------------------
# Three dimensions of teacher role perception

triq <- '
  # F1: Promotion Role
  F1 =~ Q16_ruolopercepito_10 + Q16_ruolopercepito_11 + Q16_ruolopercepito_12 + 
        Q16_ruolopercepito_17 + Q16_ruolopercepito_18
  
  # F2: Pedagogical Role (5 items)
  F2 =~ Q16_ruolopercepito_1 + Q16_ruolopercepito_2 + Q16_ruolopercepito_4 + 
        Q16_ruolopercepito_6 + Q16_ruolopercepito_7
  
  # F3: Traditional Role (3 items)
  F3 =~ Q16_ruolopercepito_5 + Q16_ruolopercepito_8 + Q16_ruolopercepito_15
'

fit_TRIQ <- cfa(triq, data = HBSC_LPA, missing = "fiml")
summary(fit_TRIQ, fit.measures = TRUE)

# ------------------------------------------------------------------------------
# 4. ACTIVE TEACHING METHODOLOGIES
# ------------------------------------------------------------------------------
# Two-factor structure distinguishing methodology types

metattive <- '
  # F1: Cooperative Learning (7 items)
  F1 =~ Q19_metodolattive_1 + Q19_metodolattive_2 + Q19_metodolattive_4 + 
        Q19_metodolattive_5 + Q19_metodolattive_6 + Q19_metodolattive_7 + 
        Q19_metodolattive_8
  
  # F2: Metacognitive strategies (4 items)
  F2 =~ Q19_metodolattive_3 + Q19_metodolattive_9 + Q19_metodolattive_10 + 
        Q19_metodolattive_11
'

fit_metattive <- cfa(metattive, data = HBSC_LPA, missing = "fiml")
summary(fit_metattive, fit.measures = TRUE)

# ------------------------------------------------------------------------------
# 5. TEACHER SELF-EFFICACY (Modified Model)
# ------------------------------------------------------------------------------
# Single factor with correlated residuals based on content similarity

selfeff <- '
  # General teaching self-efficacy (12 items)
  F1 =~ Q17_autoefficacia_1 + Q17_autoefficacia_2 + Q17_autoefficacia_3 + 
        Q17_autoefficacia_4 + Q17_autoefficacia_5 + Q17_autoefficacia_6 + 
        Q17_autoefficacia_7 + Q17_autoefficacia_8 + Q17_autoefficacia_9 + 
        Q17_autoefficacia_10 + Q17_autoefficacia_11 + Q17_autoefficacia_12
  
  # Correlated residuals (content overlap or method effects)
  Q17_autoefficacia_9 ~~ Q17_autoefficacia_11   # Classroom management items
  Q17_autoefficacia_7 ~~ Q17_autoefficacia_8    # Student engagement items
  Q17_autoefficacia_9 ~~ Q17_autoefficacia_10   # Instructional strategies
  Q17_autoefficacia_2 ~~ Q17_autoefficacia_10   # Assessment-related items
'

fit_selfeff <- cfa(selfeff, data = HBSC_LPA, missing = "fiml")
summary(fit_selfeff, fit.measures = TRUE)

# Check for additional model improvements
modindices(fit_selfeff, sort = TRUE, minimum.value = 10)

# ==============================================================================
# FACTOR SCORE EXTRACTION AND DATASET PREPARATION
# ==============================================================================

# Extract factor scores from all models
F_engagement <- lavPredict(fit_engagement)
F_burnout <- lavPredict(fit_burnout)
F_triq <- lavPredict(fit_TRIQ)
F_metatt <- lavPredict(fit_metattive)
F_selfeff <- lavPredict(fit_selfeff)

# Convert to dataframes for manipulation
F_engagement_df <- as.data.frame(F_engagement)
F_burnout_df <- as.data.frame(F_burnout)
F_triq_df <- as.data.frame(F_triq)
F_metatt_df <- as.data.frame(F_metatt)
F_selfeff_df <- as.data.frame(F_selfeff)

# Assign meaningful column names
colnames(F_engagement_df) <- "Engagement_Factor"
colnames(F_burnout_df) <- c("BurnoutEE", "Burnout_CY", 
                            "Burnout_RP", "Burnout_DEP")
colnames(F_triq_df) <- c("TRIQ_Prom", "TRIQ_Ped", 
                         "TRIQ_Trad")
colnames(F_metatt_df) <- c("CoopLea", "Metacognitive")
colnames(F_selfeff_df) <- "Teacher_Self_Efficacy"

# Merge factor scores with original dataset
HBSC_LPA <- cbind(HBSC_LPA, 
                  F_engagement_df,
                  F_burnout_df,
                  F_triq_df,
                  F_metatt_df,
                  F_selfeff_df)

# ==============================================================================
# MODEL DIAGNOSTICS SUMMARY
# ==============================================================================
# Key fit indices to evaluate:
# - CFI/TLI > .95 (good fit)
# - RMSEA < .06 (good fit)
# - SRMR < .08 (good fit)
# 
# Note: Correlated residuals indicate shared method variance or content overlap
# beyond the primary factor structure. These should be theoretically justified.
