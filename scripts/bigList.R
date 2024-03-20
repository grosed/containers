rm(list=ls())
library(tictoc)

## this is a niave implimentation that doesn't pop properly
bigList <- function(block_size=5){
    list(
        rec = list(rep(list(NULL),block_size)),
        cnt = 0,
        bk = block_size,
        frst = 1,
        lst = 1
    )
}
# push back
push_back = function(bl,x){
    ## location within final block
    if(bl$lst == 1){
        ii <- bl$cnt[ bl$lst ] + 1
    }else{
        ii <- bl$cnt[ bl$lst ] - bl$cnt[ bl$lst -1 ] + 1
    }
    
    if(ii > bl$bk){
                ## need to grow block
        bl$lst <- bl$lst + 1
        bl$rec[[ bl$lst ]] <- rep(list(NULL),bl$bk)
        bl$cnt[ bl$lst ] <- bl$cnt[ bl$lst - 1 ]
        ii <- 1
    }
    bl$rec[[ bl$lst ]][[ii]] <- x
    bl$cnt[bl$lst] <- bl$cnt[bl$lst]+1
    invisible(bl)
}
## pop back
pop_back = function(bl){
    if(bl$lst == 1){
        ii <- bl$cnt[ bl$lst ]
    }else{
        ii <- bl$cnt[ bl$lst ] - bl$cnt[ bl$lst -1 ]
    }
    out <- bl$rec[[ bl$lst ]][[ii]]
    bl$cnt[bl$lst] <- bl$cnt[bl$lst]-1
    bl$rec[[ bl$lst ]][[ii]] <- list(NULL)
    if(ii == 1){ bl$lst <- bl$lst - 1 }
    out
}
## doing pop back as two steps
get_last <- function(bl){
    if(bl$lst == 1){
        ii <- bl$cnt[ bl$lst ]
    }else{
        ii <- bl$cnt[ bl$lst ] - bl$cnt[ bl$lst -1 ]
    }
    bl$rec[[ bl$lst ]][[ii]]
}
drop_last <- function(bl){
    if(bl$lst == 1){
        ii <- bl$cnt[ bl$lst ]
    }else{
        ii <- bl$cnt[ bl$lst ] - bl$cnt[ bl$lst -1 ]
    }
    bl$cnt[bl$lst] <- bl$cnt[bl$lst]-1
    bl$rec[[ bl$lst ]][[ii]] <- list(NULL)
    if(ii == 1){ bl$lst <- bl$lst - 1 }
    bl
}   

##
push_front = function(bl,x){
    ## number of places filled in the first element
    if(bl$frst == 1){
        jj <- bl$cnt[ bl$frst ]
    }else{
        jj <- bl$cnt[ bl$frst ] - bl$cnt[ bl$frst -1 ]
    }
    if(jj == bl$bk){
        if(bl$frst==1){
            ## need to grow forward
            bl$rec <- c( rep(list(NULL),bl$bk), bl$rec )
            bl$cnt <- c( 0, bl$cnt )
        }
    }
    bl$rec[[ bl$frst ]] <- c(list(x), bl$rec[[ bl$frst ]][-bl$bk])
    bl$cnt[ bl$frst:length(bl$cnt) ] <- bl$cnt[ bl$frst:length(bl$cnt) ] +1
    invisible(bl)
}
##
pop_front = function(bl){
    out <- bl$rec[[ bl$frst ]][[1]]
    bl$rec[[ bl$frst ]] <- c( bl$rec[[bl$frst]][-1], list(NULL) )
    bl$cnt[ bl$frst:length(bl$cnt) ] <- bl$cnt[ bl$frst:length(bl$cnt) ] - 1
##    bl$rec[[ bl$frst ]][[ii]] <- list(NULL)
    if(bl$frst == 1){
        jj <- bl$cnt[ bl$frst ]
    }else{
        jj <- bl$cnt[ bl$frst ] - bl$cnt[ bl$frst -1 ]
    }
    if( jj == 0 ){ bl$frst <- bl$frst + 1 }
    out
}
get_front = function(bl){
    bl$rec[[ bl$frst ]][[1]]
}
drop_front = function(bl){
    bl$rec[[ bl$frst ]] <- c( bl$rec[[bl$frst]][-1], list(NULL) )
    bl$cnt[ bl$frst:length(bl$cnt) ] <- bl$cnt[ bl$frst:length(bl$cnt) ] - 1
    if(bl$frst == 1){
        jj <- bl$cnt[ bl$frst ]
    }else{
        jj <- bl$cnt[ bl$frst ] - bl$cnt[ bl$frst -1 ]
    }
    if( jj == 0 ){ bl$frst <- bl$frst + 1 }
    bl
}








## ############################
## Push to front
## not fast - but faster then a niave list implentation
## depends a lot on block size
tic()
n <- 100000  
tmp <- bigList(100)
for(ii in 1:n){
    tmp <- push_front(tmp,ii)
}
toc()

tic()
tmp <- list()
for(ii in 1:n){
    tmp <- c(list(ii),tmp)
    ## this is even slower
    ##tmp[[ length(tmp)+1 ]] <- list(NULL)
    ##tmp[2:(length(tmp)+1)] <- tmp
    ##tmp[[1]] <- ii
}
toc()


## ############################
## push to back 
## not that fast

## depends a lot on block size
tic()
n <- 100000  
tmp <- bigList(100)
for(ii in 1:n){
    tmp <- push_back(tmp,ii)
}
toc()

## basic R list growth
tic()
tmp <- list()
for(ii in 1:n){
    tmp[[ii]] <- ii
}
toc()

## ############################
## pop from back
## doesn't work properly since catch alter bigList object

## make bigList
n <- 100000  
tmp <- bigList(100)
for(ii in 1:n){
    tmp <- push_back(tmp,ii)
}

## wrong since no changes to tmp
tic()
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- pop_back(tmp)
}
toc()

## As two functions
tic()
ttmp <- tmp
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- get_last(ttmp)
    ttmp <- drop_last(ttmp)
}
toc()

## #################################
## pop from front

## make bigList
n <- 100000  
tmp <- bigList(100)
for(ii in 1:n){
    tmp <- push_back(tmp,ii)
}

## wrong since no changes to tmp
tic()
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- pop_front(tmp)
}
toc()

## As two functions
tic()
ttmp <- tmp
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- get_front(ttmp)
    ttmp <- drop_front(ttmp)
}
toc()

## As two calls in base R - slower
tic()
ttmp <- as.list(1:n)
out <- rep(NA,n)
for(ii in 1:n){
    out[ii] <- ttmp[1]
    ttmp <- ttmp[-1]
}
toc()




## ##############################################
## Some basic list tests in R
n <- 100000
tic(); tmp <- as.list(1:n); toc()
tic(); tmp <- list(); for(ii in 1:n){tmp[[ii]] <- ii}; toc()
tic(); tmp <- rep(list(NULL),n); for(ii in 1:n){tmp[[ii]] <- ii}; toc()
