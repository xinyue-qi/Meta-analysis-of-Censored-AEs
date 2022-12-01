sessionInfo()
packageVersion("rjags")

R.version.string
install.packages("installr")
library(installr)
updateR() #RGui

rm(list=ls(all=TRUE))
setwd("C:/Users/xqi/245890")

library(rjags)
library(gtools)
library(glmnet)
library(dclone)

library(reshape2)
library(dplyr)

install.packages("remotes")
remotes::install_github("keaven/nphsim")
library(nphism)

library(dagitty)
library(rstan)

save(ObjectToBeSaved, file = "FileName.RData")

install.packages("bookdown")
library(bookdown)

install.packages("blogdown")
library(blogdown)

install.packages("forestly")
install.packages("devtools")
devtools::install_github("elong0527/forestly")

library(xtable)
data <- as.data.frame(df)
colname(data) <- c("V1", "V2")
print(xtable(data))

# Metadata
install.packages("metacore")
install.packages("metatools")

# build the metadata from ADaMs
install.packages("metalite")
library(metalite)

install_github("metalite.ae") # TLF deliverables: AE tables
library(forestly) # interactive AE forest plot
