# ### ### ### ### ### ### ### ### ###
# RICLPM: ESTIMATION
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
here::i_am("script/3_estimation.R")

# Load workspace from previous steps
load("script/output/environment.RData")

# Estimation
# Loop over a vector of model names
models <- c("a1","a2","b1","b2","c1","c2","d1","d2")

# Fitted models will be stored in the list `fitted`
fitted <- list()

# For each name in `models`, fit the model concatenating the "common parts" and the corresponding "effect part" together
for (i in models){
  fitted[[i]] <- lavaan(model = c(syntax[["bwcomp"]],syntax[[i]],syntax[["varcov"]]),
                     data = elsoc, 
                     estimator = "MLR", 
                     missing = "FIML",
                     meanstructure = T, 
                     int.ov.free = T)
}

# Store parameter estimations in the list `param`
param <- list()
for (i in models){
param[[i]] <- data.table(model=i,parameterEstimates(fitted[[i]]))
}

# Export estimations to excel
for (i in models) {
  xlsx::write.xlsx(param[[i]], "script/output/estimates.xlsx", 
                   sheetName=i, 
                   row.names = FALSE,
                   append=TRUE)
}

# Save workspace
save.image(file = "script/output/environment.RData")
