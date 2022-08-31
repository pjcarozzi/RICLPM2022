# ### ### ### ### ### ### ### ### ###
# RICLPM: MODEL SYNTAX
# PJCAROZZI@UC.CL
# UPDATED 8/31/2022
# ### ### ### ### ### ### ### ### ###

# PREV ---
# Clean workspace 
rm(list = ls(all.names = TRUE))

# Set working directory
here::i_am("script/2_model_syntax.R")

# Load workspace from previous steps
load("script/output/environment.RData")

# Create model syntax objects
# Note: To test hypotheses about the direction of causality,
# we follow a nested models strategy to estimate:
# I.   Model A containing only auto regressive paths.
# II.  Model B containing auto regressive and cross lagged paths in forward direction.
# III. Model C containing auto regressive and cross lagged paths in backward direction.
# IV.  Model D containing auto regressive and all cross lagged paths.

# To test if the effects are stable in time, we estimate 2 versions of
# each model:
# I.   Model 1, in which the effects are freely estimated (unconstrained)  
# II.  Model 2, in which the effects are constrained to be the same across waves. 

# In summary, 8 models are estimated.
# I.    Model A1, containing unconstrained auto regressive paths only.
# II.   Model A2, containing constrained auto regressive paths only.
# III.  Model B1, by adding unconstrained cross lagged paths in forward direction to A1.
# IV.   Model B2, by adding constrained cross lagged paths in forward direction to A2.
# V.    Model C1, by adding unconstrained cross lagged paths in backward direction to A1.
# VI.   Model C2, by adding constrained cross lagged paths in backward direction to A2.
# VII.  Model D1, by adding unconstrained cross lagged path in both directions (full bidirectional model).
# VIII. Model D2, by adding constrained cross lagged path in both directions (full bidirectional model).

syntax <- list()

# We split the RI-CLPM syntax (as coded by Mulder and Hamaker: https://jeroendmulder.github.io/RI-CLPM/lavaan.html) 
# in multiple parts: A "common part" and an "effects part".

# COMMON PART ----
# It contains: 
# - RIs (between components)
# - Within-person centered variables.
# - Variances of measurement errors constrained to zero.
# - Covariance between the within-person centered variables at the first wave
# - Covariances between the residuals of the within-person centered variables
# - Variance and covariance of the random intercepts
# - Correlation between RIs and within components in t=1 fixed to zero

syntax[["bwcomp"]] <- '
            # Create between components (RIs)
            RI_x =~ 1*off1 + 1*off2 + 1*off3 + 1*off4 + 1*off5
            RI_y =~ 1*onl1 + 1*onl2 + 1*onl3 + 1*onl4 + 1*onl5
            
            # Create within-person centered variables
            cx1 =~ 1*off1
            cx2 =~ 1*off2
            cx3 =~ 1*off3
            cx4 =~ 1*off4
            cx5 =~ 1*off5
            
            cy1 =~ 1*onl1
            cy2 =~ 1*onl2
            cy3 =~ 1*onl3
            cy4 =~ 1*onl4
            cy5 =~ 1*onl5
            
            # Constrain the measurement error variances to zero.
            off1 ~~ 0*off1
            off2 ~~ 0*off2
            off3 ~~ 0*off3
            off4 ~~ 0*off4
            off5 ~~ 0*off5
            
            onl1 ~~ 0*onl1
            onl2 ~~ 0*onl2
            onl3 ~~ 0*onl3
            onl4 ~~ 0*onl4
            onl5 ~~ 0*onl5
          '

syntax[["varcov"]] <- ' 
           # Estimate the covariance between the within-person centered variables at t=1
           cx1 ~~ cy1
           
           # Estimate the covariances between the residuals of the within-person centered variables
           cx2 ~~ cy2
           cx3 ~~ cy3
           cx4 ~~ cy4
           cx5 ~~ cy5
           
           # Estimate the (residual) variance of the within-person centered variables
           cx1 ~~ cx1 # Variances
           cy1 ~~ cy1 
           cx2 ~~ cx2 # Residual variances
           cy2 ~~ cy2 
           cx3 ~~ cx3 
           cy3 ~~ cy3 
           cx4 ~~ cx4 
           cy4 ~~ cy4 
           cx5 ~~ cx5
           cy5 ~~ cy5 
           
           # Estimate the variance and covariance of the RI
           RI_x ~~ RI_x
           RI_y ~~ RI_y
           RI_x ~~ RI_y
           
           # Fix the correlation between the RIs and the exogenous variables (within-person centered variables at t=1) to zero
           RI_x ~~ 0*cx1
           RI_x ~~ 0*cy1
           RI_y ~~ 0*cx1
           RI_y ~~ 0*cy1 
          '

# EFFECTS PART ----
## MODEL A ----
syntax[["a1"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model A1, only auto regressive paths. 
          # Unconstrained.
              cx2 ~ cx1
              cx3 ~ cx2
              cx4 ~ cx3
              cx5 ~ cx4
              cy2 ~ cy1
              cy3 ~ cy2
              cy4 ~ cy3
              cy5 ~ cy4
'
syntax[["a2"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model A2, only auto regressive paths. 
          # Constrained.
              cx2 ~ a*cx1
              cx3 ~ a*cx2
              cx4 ~ a*cx3
              cx5 ~ a*cx4
              cy2 ~ b*cy1
              cy3 ~ b*cy2
              cy4 ~ b*cy3
              cy5 ~ b*cy4
'

## MODEL B ----
syntax[["b1"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model B1, auto regressive and cross lagged paths in forward direction.
          # Unconstrained.
              cx2 ~ cx1
              cx3 ~ cx2 
              cx4 ~ cx3
              cx5 ~ cx4
              cy2 ~ cx1 + cy1
              cy3 ~ cx2 + cy2
              cy4 ~ cx3 + cy3
              cy5 ~ cx4 + cy4
'
syntax[["b2"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model B2, auto regressive and cross lagged paths in forward direction.
          # Constrained.
              cx2 ~ a*cx1
              cx3 ~ a*cx2
              cx4 ~ a*cx3
              cx5 ~ a*cx4
              cy2 ~ c*cx1 + b*cy1
              cy3 ~ c*cx2 + b*cy2
              cy4 ~ c*cx3 + b*cy3
              cy5 ~ c*cx4 + b*cy4
'

## MODEL C ----
syntax[["c1"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model C1, auto regressive and cross lagged paths in backward direction.
          # Unconstrained.
              cx2 ~ cx1 + cy1
              cx3 ~ cx2 + cy2 
              cx4 ~ cx3 + cy3
              cx5 ~ cx4 + cy4
              cy2 ~ cy1
              cy3 ~ cy2
              cy4 ~ cy3
              cy5 ~ cy4
'
syntax[["c2"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model C2, auto regressive and cross lagged paths in backward direction.
          # Constrained.
              cx2 ~ a*cx1 + d*cy1
              cx3 ~ a*cx2 + d*cy2 
              cx4 ~ a*cx3 + d*cy3
              cx5 ~ a*cx4 + d*cy4
              cy2 ~ b*cy1
              cy3 ~ b*cy2
              cy4 ~ b*cy3
              cy5 ~ b*cy4
'
## MODEL D ----
syntax[["d1"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model D1, auto regressive and all cross lagged paths.
          # Unconstrained.
              cx2 ~ cx1 + cy1
              cx3 ~ cx2 + cy2 
              cx4 ~ cx3 + cy3
              cx5 ~ cx4 + cy4
              cy2 ~ cx1 + cy1
              cy3 ~ cx2 + cy2
              cy4 ~ cx3 + cy3
              cy5 ~ cx4 + cy4
'
syntax[["d2"]] <- '
          # Estimate the lagged effects between the within‐person centered variables.
          # Model D2, auto regressive and all cross lagged paths.
          # Constrained.
              cx2 ~ a*cx1 + d*cy1
              cx3 ~ a*cx2 + d*cy2 
              cx4 ~ a*cx3 + d*cy3
              cx5 ~ a*cx4 + d*cy4
              cy2 ~ c*cx1 + b*cy1
              cy3 ~ c*cx2 + b*cy2
              cy4 ~ c*cx3 + b*cy3
              cy5 ~ c*cx4 + b*cy4
'

# Check `syntax` list
summary(syntax)

# Save workspace
save.image(file = "script/output/environment.RData")
