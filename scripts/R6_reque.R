rm(list=ls())
## library(R6)
library(tictoc)
source("scripts/reque.R")
## timing of this is very variable....
## certainly restart kernal before use, but even then....

## ############################
## Push to front
## not fast - but faster then a niave list implentation
## depends a lot on block size
tic()
n <- 100000  
tmp <- reque$new(100)
for(ii in 1:n){
    tmp$push_front(ii)
}
toc() ## ~21s / 100k

## base R
tic()
tmp <- list()
for(ii in 1:n){
    tmp <- c(list(ii),tmp)
}
toc() ##~28s / 100k


## ## ############################
## ## push to back 
## ## not that fast

## depends a lot on block size
tic()
n <- 100000  
tmp <- reque$new(100)
for(ii in 1:n){
    tmp$push_back(ii)
}
toc() ## 0.44s / 100k

## basic R list growth
tic()
tmp <- list()
for(ii in 1:n){
    tmp[[ii]] <- ii
}
toc() ## 0.044s / 100k

## ## ############################
## ## pop from back

n <- 100000  
tmp <- reque$new(100)
for(ii in 1:n){
    tmp$push_back(ii)
}

tic()
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- tmp$pop_back()
}
toc() # 0.7s / 100k

## in base R
tic()
tmp <- as.list(1:n)
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- tmp[length(tmp)]
    tmp <- head(tmp,-1)
}
toc() ## 28s / 100k

## ## #################################
## ## pop from front

n <- 100000  
tmp <- reque$new(100)
for(ii in 1:n){
    tmp$push_back(ii)
}

tic()
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- tmp$pop_front()
}
toc()## 1.7s / 100k


## As two calls in base R - slower
tic()
n <- 100000
ttmp <- as.list(1:n)
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- ttmp[1]
    ttmp <- ttmp[-1]
}
toc() ## 30s / 100k





