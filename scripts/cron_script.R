#!/usr/bin/Rscript
#Detach all  packages
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

source("new_speech_appender.R")
detachAllPackages()
source("cleaning_speeches.R")
detachAllPackages()
source("preprocessing.R")
detachAllPackages()
source("sentiment_analysis.R")
detachAllPackages()
source("sentiment_sentences.R")
detachAllPackages()
source("stm.R")
detachAllPackages()
source("country_codes.R")
