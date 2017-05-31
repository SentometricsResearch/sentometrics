#options(digits = 4, max.print = 5000, prompt = "> ", warn = -1)

# list.of.packages = c("MSGARCH", "Rsolnp", "nloptr", "DEoptim", "MitISEM","methods", 
#                       "AdMit", "compiler", "mvtnorm", "car", "sandwich", "tseries",
#                       "numDeriv", "Matrix","Rcpp","stringr")

# list.of.packages <- c("MSGARCH", "Rsolnp", "nloptr", "DEoptim", "MitISEM",
#                       "AdMit","compiler","mvtnorm","car","sandwich","tseries",
#                       "numDeriv","Matrix","Rcpp","adaptMCMC","mcmcsampler")

list.of.packages = c("XML", "compiler")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Load packages
str = c("./code/", "./misc/")
file.sources = list.files(str, pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)
