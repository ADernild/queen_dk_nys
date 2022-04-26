library(jsonlite) # API stuff
library(tictoc)
library(rbenchmark)
library(microbenchmark)
sys.time.sleep <- c()
sys.time.json <- c()
system.time.sleep <- c()
system.time.json <- c()
tictoc.time.sleep <- c()
tictoc.time.json <- c()
# Sleep tests ----
# Using: Sys.sleep(0.5)
## sys.time ----
for(i in 1:5){
  startTime <- Sys.time()
  Sys.sleep(0.5)
  endTime <- Sys.time()
  sys.time.sleep <- c(sys.time.sleep, startTime-endTime)
}

## system.time ----
for(i in 1:5){
  system.time.sleep <- c(system.time.sleep, system.time({Sys.sleep(0.5)})[3][[1]])
}

## tictoc library ----
for(i in 1:5){
  tic()
  Sys.sleep(0.5)
  toc.res <- toc()
  tictoc.time.sleep <- c(tictoc.time.sleep, toc.res$tic[[1]]-toc.res$toc[[1]])
}
## rbenchmark library ----
rbench.time.sleep <- benchmark(Sys.sleep(0.5), replications = 5)

## rbenchmark library ----
mbench.time.sleep <- microbenchmark(Sys.sleep(0.5), times = 5)


# API tests ----
# Calling the public API for TV2 Fyn using the Bazo
public <- "https://public.fyn.bazo.dk"
ver <- "/v1/"
public <- paste(public, ver, sep="")
page_size <- 100 # Max 100 in API
article_list_call <- paste(public, "articles?page[size]=", page_size, "&page[number]=", 1,
                           "&include[0]=activeContentRevision.publishedPrimaryLocation",
                           "&include[1]=activeContentRevision.authors",
                           "&include[2]=activeContentRevision.publishedPrimarySection",
                           sep="") # Define call
## sys.time ----
for(i in 1:5){
  startTime <- Sys.time()
  fromJSON(article_list_call, flatten = FALSE)
  endTime <- Sys.time()
  sys.time.json <- c(sys.time.json, startTime-endTime)
}

## system.time ----
for(i in 1:5){
  system.time.json <- c(system.time.json, system.time({fromJSON(article_list_call, flatten = FALSE)})[3][[1]])
}

## tictoc library ----
for(i in 1:5){
  tic()
  fromJSON(article_list_call, flatten = FALSE)
  toc.res <- toc()
  tictoc.time.json <- c(tictoc.time.json, toc.res$tic[[1]]-toc.res$toc[[1]])
}
## rbenchmark library ----
rbench.time.json <- benchmark(fromJSON(article_list_call, flatten = FALSE), replications = 5)

## rbenchmark library ----
mbench.time.json <- microbenchmark(fromJSON(article_list_call, flatten = FALSE), times = 5)

# Evaluation ----
## Sleep ----
summary(sys.time.sleep*-1)
# > summary(sys.time.sleep*-1)
# Min.    1st Qu.   Median    Mean    3rd Qu.   Max. 
# 0.5098  0.5119    0.5137    0.5129  0.5144    0.5147
sys.time.sleep.diff <- summary(sys.time.sleep*-1)[6]-summary(sys.time.sleep*-1)[1]
#0.004931688 
sys.time.sleep.var <- var(sys.time.sleep)
# [1] 4.264989e-06

summary(system.time.sleep)
# > summary(system.time.sleep)
# Min.    1st Qu.   Median    Mean    3rd Qu.   Max. 
# 0.500   0.500     0.500     0.506   0.510     0.520 
system.time.sleep.diff <- summary(system.time.sleep)[6]-summary(system.time.sleep)[1]
# 0.02
system.time.sleep.var <- var(system.time.sleep)
# [1] 8e-05

summary(tictoc.time.sleep*-1)
# > summary(tictoc.time.sleep*-1)
# Min.    1st Qu.   Median    Mean    3rd Qu.   Max. 
# 0.510   0.510     0.520     0.516   0.520     0.520
tictoc.time.sleep.diff <- summary(tictoc.time.sleep*-1)[6]-summary(tictoc.time.sleep*-1)[1]
# 0.01
tictoc.time.sleep.var <- var(tictoc.time.sleep)
# [1] 3e-05

rbench.time.sleep
# > rbench.time.sleep
#   test            replications  elapsed   relative  user.self sys.self  user.child  sys.child
# 1 Sys.sleep(0.5)  5             2.55      1         0         0         NA          NA
# Cannot evaluate span or variance

mbench.time.sleep
# > mbench.time.sleep
# Unit: milliseconds
# expr            min       lq        mean      median    uq        max       neval
# Sys.sleep(0.5)  503.2135  507.2081  510.2063  511.8606  513.6046  515.1445  5
mbench.time.sleep.diff <- (summary(mbench.time.sleep)[7]-summary(mbench.time.sleep)[2])/100
# 0.11931
mbench.time.sleep.var <- var(mbench.time.sleep$time)
# [1] 2.413986e+13

## API ----
summary(sys.time.json*-1)
# > summary(sys.time.json*-1)
# Min.    1st Qu.   Median    Mean    3rd Qu.   Max. 
# 1.498   1.559     1.560     1.562   1.579     1.613 
sys.time.json.diff <- summary(sys.time.json*-1)[6]-summary(sys.time.json*-1)[1]
# 0.1150539
sys.time.json.var <- var(sys.time.json)
# [1] 0.001749613

summary(system.time.json)
# > summary(system.time.json)
# Min.    1st Qu.   Median    Mean    3rd Qu.   Max. 
# 1.500   1.500     1.540     1.558   1.590     1.660 
system.time.json.diff <- summary(system.time.json)[6]-summary(system.time.json)[1]
# 0.16
system.time.json.var <- var(system.time.json)
# [1] 0.00462

summary(tictoc.time.json*-1)
# > summary(tictoc.time.json*-1)
# Min.    1st Qu.   Median    Mean    3rd Qu.   Max. 
# 1.500   1.510     1.520     1.532   1.560     1.570
tictoc.time.json.diff <- summary(tictoc.time.json*-1)[6]-summary(tictoc.time.json*-1)[1]
# 0.07
tictoc.time.json.var <- var(tictoc.time.json)
# [1] 0.00097

rbench.time.json
#   test                                            replications  elapsed relative  user.self   sys.self    user.child  sys.child
# 1 fromJSON(article_list_call, flatten = FALSE)    5             13.93   1         5.3         0.11        NA          NA
# Cannot evaluate span

mbench.time.json
# > mbench.time.json
# Unit: seconds
# expr                                         min        lq      mean      median      uq        max       neval
# fromJSON(article_list_call, flatten = FALSE) 1.502663   1.55201 1.595491  1.559459    1.64915   1.714172  5
mbench.time.json.diff <- (summary(mbench.time.json)[7]-summary(mbench.time.json)[2])
# 0.2115083
mbench.time.json.var <- var(mbench.time.json$time)
# [1] 7.192563e+15
