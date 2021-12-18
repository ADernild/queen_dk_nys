#!/usr/bin/Rscript
#Detach all  packages
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  setwd("scripts/")
  remove(list=ls())
}

source("new_speech_appender.R")
detachAllPackages()
print("New speech collected")
source("cleaning_speeches.R")
detachAllPackages()
print("Cleaned speeches")
source("preprocessing.R")
detachAllPackages()
print("Speeches preprocessed")
source("sentiment_analysis.R")
detachAllPackages()
print("Sentiment analyzed")
source("sentiment_sentences.R")
detachAllPackages()
print("Sentence sentiment analyzed")
source("stm.R")
detachAllPackages()
print("Structural Topic model converged")
source("country_codes.R")
print("Countries extracted")
