# ### ### ### ### ### ### ### ### ###
# RICLPM: PREPARING ELSOC DATA
# PJCAROZZI@UC.CL
# UPDATED 8/31/2022
# ### ### ### ### ### ### ### ### ###

# PREV ---
# Clean workspace 
rm(list = ls(all.names = TRUE))

# Libraries
library(data.table)

# Set working directory
here::i_am("script/1_preparing_data.R")

# Load Data
load(url("https://dataverse.harvard.edu/api/access/datafile/6160173"))
setDT(elsoc_long_2016_2021)

# Reshape data from long to wide
elsoc <- dcast(elsoc_long_2016_2021, idencuesta ~ ola, value.var = c("c08_02","c08_04","m0_edad","m01","m0_sexo"))

# Select variables
old <- c("idencuesta",paste0("c08_02_",1:5),paste0("c08_04_",1:5),"m0_edad_1","m01_1","m0_sexo_1")
new <- c("id",paste0("off",1:5),paste0("onl",1:5),"edad","educ","sexo")
setnames(elsoc,old,new)
elsoc <- elsoc[, ..new]

# Recode NAs
elsoc <- elsoc[, lapply(.SD, function(x) replace(x, which(x %in% c(-666,-888,-999)), NA))]

# Remove rows with all NAs in variables of interest
vars <- grep("^off|^onl",names(elsoc),value=T)
elsoc <- elsoc[rowSums(elsoc[,..vars],na.rm=T)!=0,]

# Clean workspace
rm(elsoc_long_2016_2021,old,new,vars)
elsoc <- as.data.frame(elsoc)

# Describe data
summarytools::descr(elsoc[-1], order="p", stats = "common", transpose = T, headings = F)

# Save workspace
save.image(file = "script/output/environment.RData")

