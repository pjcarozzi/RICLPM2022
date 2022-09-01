# ### ### ### ### ### ### ### ### ###
# CHAYINSKA ET AL: MODELS
# PJCAROZZI@UC.CL
# UPDATED 8/31/2022
# ### ### ### ### ### ### ### ### ###


# PREV ---
# Clean workspace 
rm(list = ls(all.names = TRUE))

# Libraries
library(data.table)
library(lavaan)

# Set working directory
here::i_am("script/chayinska_et_al/2_ch_model.R")

# Load workspace from previous steps
load("script/chayinska_et_al/output/environment.RData")

# Create model syntax object
# Fig. 3. Full longitudinal bidirectional model for 
# online and ofﬂine participation in collective action

d2_3w <- '
          # Measurment model
          off1 =~ fir1 + mar1 + hue1
          off2 =~ fir2 + mar2 + hue2
          off3 =~ fir3 + mar3 + hue3
          
          # Correlated error terms
          fir1 ~~ fir2
          fir1 ~~ fir3
          fir2 ~~ fir3
          mar1 ~~ mar2
          mar1 ~~ mar3
          mar2 ~~ mar3
          hue1 ~~ hue2
          hue1 ~~ hue3
          hue2 ~~ hue3

          # Lagged effects    
          off2 ~ a*off1 + c*onl1
          off3 ~ a*off2 + c*onl2 + off1

          onl2 ~ d*off1 + b*onl1
          onl3 ~ d*off2 + b*onl2 + onl1

          # Varcov
          off1 ~~ onl1
          off2 ~~ onl2
          off3 ~~ onl3

          # Control vars
          off1 ~ edad + educ + factor(sexo)
          off2 ~ edad + educ + factor(sexo)
          off3 ~ edad + educ + factor(sexo)
          
          onl1 ~ edad + educ + factor(sexo)
          onl2 ~ edad + educ + factor(sexo)
          onl3 ~ edad + educ + factor(sexo)
'

fit_d2_3w <- sem(d2_3w, data=elsoc, missing = "FIML", mimic = "Mplus")
summary(fit_d2_3w, fit.measures=TRUE, standardized = T)
