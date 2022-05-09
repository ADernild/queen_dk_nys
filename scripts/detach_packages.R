detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  package.list <- package.list[package.list != "package:shiny"];
  
  package.list <- package.list[package.list != "package:shinydashboard"];
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()
