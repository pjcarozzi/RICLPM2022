# ### ### ### ### ### ### ### ### ###
# RICLPM: MODEL COMPARISON
# PJCAROZZI@UC.CL
# UPDATED 8/31/2022
# ### ### ### ### ### ### ### ### ###

# PREV
# Clean workspace 
rm(list = ls(all.names = TRUE))

# Libraries
library(data.table)
library(lavaan)

# Set working directory
here::i_am("script/4_model_comparison.R")

# Load workspace from previous steps
load("script/output/environment.RData")

# Extract goodness of fit measures
gofdt <- list()
for (i in models){
    x <- fitMeasures(fitted[[i]])[c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr_mplus","aic","bic","bic2","logl","npar","scaling.factor.h0")]
    
    gofdt[[i]] <- setNames(as.numeric(x),c("X2","df","pvalue","CFI","TLI","RMSEA","SRMR","AIC","BIC","aBIC","LL","par","LLcorrectf"))
}
gofdt <- data.table(model=names(gofdt),dplyr::bind_rows(gofdt))

# Function for comparing models
gof.comp  = function(data, pairs,
                     measures = c("CFI","TLI","RMSEA","SRMR",
                                  "AIC","BIC","aBIC","par","LL")){
  comp <- list()
  for (i in 1:length(pairs)){
    gof <- data
    nest <- pairs[[i]][1]
    full <- pairs[[i]][2]
    delta <- NULL
    for (k in measures){
      delta[paste0(k,"_D")] <- gof[model==nest, get(k)] - gof[model==full, get(k)]
    }
    par_LLcorf_nest <- gof[model==nest,par]*gof[model==nest,LLcorrectf]
    par_LLcorf_full <- gof[model==full,par]*gof[model==full,LLcorrectf]
    delta["CD"] <- (par_LLcorf_nest-par_LLcorf_full)/delta["par_D"]
    delta["TRd"] <- (-2*delta["LL_D"])/delta["CD"]
    delta["TRd_df"] <- gof[model==full, "par"] - gof[model==nest, "par"]
    delta["TRd_pvalue"] <- pchisq(as.numeric(delta["TRd"]),
                                  as.numeric(delta["TRd_df"]), lower.tail = F)
    comp[[paste0(nest," vs. ",full,sep="")]] <- delta
  }
  comp <- data.table(comp=names(comp),dplyr::bind_rows(comp))
  return(comp)
}

# Comparing constrained vs. uncontrained models
comp1 <- gof.comp(data = gofdt, pairs = list(c("a2","a1"),c("b2","b1"),c("c2","c1"),c("d2","d1")))
comp1
# Comparing nested models
comp2 <- gof.comp(data = gofdt, pairs = list(c("a2","b2"),c("a2","c2"),c("a2","d2"),c("b2","d2"),c("c2","d2")))
comp2

# Export results to excel
openxlsx::write.xlsx(list(gof=gofdt,comp_constrains=comp1,comp_nested=comp2), "script/output/gof_model_comparison.xlsx",rowNames = FALSE)

# Save workspace
save.image(file = "script/output/environment.RData")
