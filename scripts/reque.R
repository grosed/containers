library(R6)
reque <- R6Class(
    "reque",
    class=FALSE,
    list(
        ## data stores
        rec = NULL, ## blocks
        bk = NULL, ## block size
        frst = NULL, ## index of the first block with data in it
        lst = NULL, ## index of the last block with data in it
        cnt = NULL, ## how many elements to the left of the end of the block
        ## initialise the object
        initialize = function(block_size=5){
            self$rec <- list(rep(list(NULL),block_size))
            self$cnt <- 0 
            self$bk <- block_size
            self$frst <- 1
            self$lst <- 1
            invisible(self)
        },
        ## push back
        push_back = function(x){
            
            ## location within final block
            if(self$lst == 1){
                ii <- self$cnt[ self$lst ] + 1
            }else{
                ii <- self$cnt[ self$lst ] - self$cnt[ self$lst -1 ] + 1
            }
            if(ii > self$bk){
                ## need to grow
                self$lst <- self$lst + 1
                self$rec[[ self$lst ]] <- rep(list(NULL),self$bk)
                self$cnt[ self$lst ] <- self$cnt[ self$lst - 1 ]
                ii <- 1
            }
            self$rec[[ self$lst ]][[ii]] <- x
            self$cnt[self$lst] <- self$cnt[self$lst]+1
            invisible(self)
        },
        ## pop back
        pop_back = function(){
            if(self$lst == 1){
                ii <- self$cnt[ self$lst ]
            }else{
                ii <- self$cnt[ self$lst ] - self$cnt[ self$lst -1 ]
            }
            out <- self$rec[[ self$lst ]][[ii]]
            bl$cnt[ bl$lst:length(bl$cnt) ] <- bl$cnt[  bl$lst:length(bl$cnt) ]-1
            self$rec[[ self$lst ]][[ii]] <- list(NULL)
            if(ii == 1){ self$lst <- self$lst - 1 }
            out
        },
        ##
        push_front = function(x){
            ## number of places filled in the first element
            if(self$frst == 1){
                jj <- self$cnt[ self$frst ]
            }else{
                jj <- self$cnt[ self$frst ] - self$cnt[ self$frst -1 ]
            }
            if(jj == self$bk){
                if(self$frst==1){
                    ## need to grow forward
                    self$rec <- c( rep(list(NULL),self$bk), self$rec )
                    self$cnt <- c( 0, self$cnt )
                }
                #self$frst <- self$frst - 1
            }
            self$rec[[ self$frst ]] <- c(list(x), self$rec[[ self$frst ]][-self$bk])
            self$cnt[ self$frst:length(self$cnt) ] <- self$cnt[ self$frst:length(self$cnt) ] +1
            invisible(self)
        },
        ##
        pop_front = function(){
            out <- self$rec[[ self$frst ]][[1]]
            self$rec[[ self$frst ]] <- c( self$rec[[self$frst]][-1], list(NULL) )
            self$cnt[ self$frst:length(self$cnt) ] <- self$cnt[ self$frst:length(self$cnt) ] - 1
            if(self$frst == 1){
                jj <- self$cnt[ self$frst ]
            }else{
                jj <- self$cnt[ self$frst ] - self$cnt[ self$frst -1 ]
            }
            if( jj == 0 ){ self$frst <- self$frst + 1 }
            out
        }
    )
)

