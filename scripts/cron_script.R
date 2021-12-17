#!/usr/bin/Rscript
#Detach all  packages
setwd(system("pwd", intern=T))

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

source("new_speech_appender.R")

scripts <- c("new_speech_appender.R", "cleaning_speeches.R", "preprocessing.R", "sentiment_analysis.R", "sentiment_sentences.R", "stm.R", "country_codes.R")

for(i in scripts){
  source(i)
  detachAllPackages()
}

